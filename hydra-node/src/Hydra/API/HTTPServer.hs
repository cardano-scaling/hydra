{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.HTTPServer where

import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Data.Aeson (KeyValue ((.=)), object, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short ()
import Data.Text (pack)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ClientMessage (..), CommitInfo (..), ServerOutput (..), TimedServerOutput (..), getConfirmedSnapshot, getSeenSnapshot, getSnapshotUtxo)
import Hydra.Cardano.Api (Coin, LedgerEra, Tx)
import Hydra.Chain (Chain (..), PostTxError (..), draftCommitTx)
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic.State (HeadState (..))
import Hydra.Ledger (ValidationError (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout (..))
import Hydra.Node.DepositPeriod (toNominalDiffTime)
import Hydra.Node.Environment (Environment (..))
import Hydra.Tx (CommitBlueprintTx (..), ConfirmedSnapshot, IsTx (..), Snapshot (..), UTxOType)
import Network.HTTP.Types (ResponseHeaders, hContentType, status200, status202, status400, status404, status500)
import Network.Wai (Application, Request (pathInfo, requestMethod), Response, consumeRequestBodyStrict, rawPathInfo, responseLBS)

newtype DraftCommitTxResponse tx = DraftCommitTxResponse
  { commitTx :: tx
  }
  deriving stock (Generic)

deriving stock instance Show tx => Show (DraftCommitTxResponse tx)

instance IsTx tx => ToJSON (DraftCommitTxResponse tx) where
  toJSON (DraftCommitTxResponse tx) = toJSON tx

instance IsTx tx => FromJSON (DraftCommitTxResponse tx) where
  parseJSON v = DraftCommitTxResponse <$> parseJSON v

instance Arbitrary tx => Arbitrary (DraftCommitTxResponse tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxResponse xs -> DraftCommitTxResponse <$> shrink xs

data DraftCommitTxRequest tx
  = SimpleCommitRequest
      { utxoToCommit :: UTxOType tx
      , amount :: Maybe Coin
      }
  | FullCommitRequest
      { blueprintTx :: tx
      , utxo :: UTxOType tx
      }
  deriving stock (Generic)

deriving stock instance (Eq tx, Eq (UTxOType tx)) => Eq (DraftCommitTxRequest tx)
deriving stock instance (Show tx, Show (UTxOType tx)) => Show (DraftCommitTxRequest tx)

instance (ToJSON tx, ToJSON (UTxOType tx)) => ToJSON (DraftCommitTxRequest tx) where
  toJSON = \case
    FullCommitRequest{blueprintTx, utxo} ->
      object
        [ "blueprintTx" .= toJSON blueprintTx
        , "utxo" .= toJSON utxo
        ]
    SimpleCommitRequest{utxoToCommit, amount} ->
      object
        [ "utxoToCommit" .= toJSON utxoToCommit
        , "amount" .= toJSON amount
        ]

instance (FromJSON tx, FromJSON (UTxOType tx)) => FromJSON (DraftCommitTxRequest tx) where
  parseJSON v = fullVariant v <|> simpleVariant v <|> simpleDirectVariant v
   where
    fullVariant = withObject "FullCommitRequest" $ \o -> do
      blueprintTx :: tx <- o .: "blueprintTx"
      utxo <- o .: "utxo"
      pure FullCommitRequest{blueprintTx, utxo}

    simpleVariant = withObject "SimpleCommitRequest" $ \o -> do
      utxoToCommit <- o .: "utxoToCommit"
      amount <- o .:? "amount"
      pure SimpleCommitRequest{utxoToCommit, amount}

    simpleDirectVariant :: Aeson.Value -> Parser (DraftCommitTxRequest tx)
    simpleDirectVariant val = SimpleCommitRequest <$> parseJSON val <*> pure Nothing

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    SimpleCommitRequest u amt -> SimpleCommitRequest <$> shrink u <*> shrink amt
    FullCommitRequest a b -> FullCommitRequest <$> shrink a <*> shrink b

newtype SubmitTxRequest tx = SubmitTxRequest
  { txToSubmit :: tx
  }
  deriving newtype (Eq, Show, Arbitrary)
  deriving newtype (ToJSON, FromJSON)

data TransactionSubmitted = TransactionSubmitted
  deriving stock (Eq, Show, Generic)

instance ToJSON TransactionSubmitted where
  toJSON _ =
    object
      [ "tag" .= Aeson.String "TransactionSubmitted"
      ]

instance FromJSON TransactionSubmitted where
  parseJSON = withObject "TransactionSubmitted" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "TransactionSubmitted" ->
        pure TransactionSubmitted
      _ -> fail "Expected tag to be TransactionSubmitted"

instance Arbitrary TransactionSubmitted where
  arbitrary = genericArbitrary

newtype SideLoadSnapshotRequest tx = SideLoadSnapshotRequest
  { snapshot :: ConfirmedSnapshot tx
  }
  deriving newtype (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

instance (Arbitrary tx, Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (SideLoadSnapshotRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    SideLoadSnapshotRequest snapshot -> SideLoadSnapshotRequest <$> shrink snapshot

-- | Request to submit a transaction to the head
newtype SubmitL2TxRequest tx = SubmitL2TxRequest
  { submitL2Tx :: tx
  }
  deriving newtype (Eq, Show, Arbitrary)
  deriving newtype (ToJSON, FromJSON)

-- | Response for transaction submission
data SubmitL2TxResponse
  = -- | Transaction was included in a confirmed snapshot
    SubmitTxConfirmed Integer
  | -- | Transaction was rejected due to validation errors
    SubmitTxInvalidResponse Text
  | -- | Transaction was accepted but not yet confirmed
    SubmitTxSubmitted
  deriving stock (Eq, Show, Generic)

instance ToJSON SubmitL2TxResponse where
  toJSON = \case
    SubmitTxConfirmed snapshotNumber ->
      object
        [ "tag" .= Aeson.String "SubmitTxConfirmed"
        , "snapshotNumber" .= snapshotNumber
        ]
    SubmitTxInvalidResponse validationError ->
      object
        [ "tag" .= Aeson.String "SubmitTxInvalid"
        , "validationError" .= validationError
        ]
    SubmitTxSubmitted -> object ["tag" .= Aeson.String "SubmitTxSubmitted"]

instance FromJSON SubmitL2TxResponse where
  parseJSON = withObject "SubmitTxResponse" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "SubmitTxConfirmed" -> SubmitTxConfirmed <$> o .: "snapshotNumber"
      "SubmitTxInvalid" -> SubmitTxInvalidResponse <$> o .: "validationError"
      "SubmitTxSubmitted" -> pure SubmitTxSubmitted
      _ -> fail "Expected tag to be SubmitTxConfirmed, SubmitTxInvalid, or SubmitTxSubmitted"

instance Arbitrary SubmitL2TxResponse where
  arbitrary = genericArbitrary

jsonContent :: ResponseHeaders
jsonContent = [(hContentType, "application/json")]

-- | Hydra HTTP server
httpApp ::
  forall tx.
  IsChainState tx =>
  Tracer IO APIServerLog ->
  Chain tx IO ->
  Environment ->
  PParams LedgerEra ->
  -- | Get latest 'HeadState'.
  IO (HeadState tx) ->
  -- | A means to get commit info.
  IO CommitInfo ->
  -- | Get the pending commits (deposits)
  IO [TxIdType tx] ->
  -- | Callback to yield a 'ClientInput' to the main event loop.
  (ClientInput tx -> IO ()) ->
  -- | Timeout for transaction submission
  ApiTransactionTimeout ->
  -- | Channel to listen for events
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  Application
httpApp tracer directChain env pparams getHeadState getCommitInfo getPendingDeposits putClientInput apiTransactionTimeout responseChannel request respond = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod request
      , path = PathInfo $ rawPathInfo request
      }
  case (requestMethod request, pathInfo request) of
    ("GET", ["head"]) ->
      getHeadState >>= respond . okJSON
    ("GET", ["snapshot"]) -> do
      hs <- getHeadState
      case getConfirmedSnapshot hs of
        Just confirmedSnapshot -> respond $ okJSON confirmedSnapshot
        Nothing -> respond notFound
    ("GET", ["snapshot", "utxo"]) -> do
      hs <- getHeadState
      case getSnapshotUtxo hs of
        Just utxo -> respond $ okJSON utxo
        _ -> respond notFound
    ("GET", ["snapshot", "last-seen"]) -> do
      hs <- getHeadState
      respond . okJSON $ getSeenSnapshot hs
    ("POST", ["snapshot"]) ->
      consumeRequestBodyStrict request
        >>= handleSideLoadSnapshot putClientInput apiTransactionTimeout responseChannel
        >>= respond
    ("POST", ["commit"]) ->
      consumeRequestBodyStrict request
        >>= handleDraftCommitUtxo env pparams directChain getCommitInfo
        >>= respond
    ("DELETE", ["commits", _]) ->
      consumeRequestBodyStrict request
        >>= handleRecoverCommitUtxo putClientInput apiTransactionTimeout responseChannel (last . fromList $ pathInfo request)
        >>= respond
    ("GET", ["commits"]) ->
      getPendingDeposits >>= respond . responseLBS status200 jsonContent . Aeson.encode
    ("POST", ["decommit"]) ->
      consumeRequestBodyStrict request
        >>= handleDecommit putClientInput apiTransactionTimeout responseChannel
        >>= respond
    ("GET", ["protocol-parameters"]) ->
      respond . responseLBS status200 jsonContent . Aeson.encode $ pparams
    ("POST", ["cardano-transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitUserTx directChain
        >>= respond
    ("POST", ["transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitL2Tx putClientInput apiTransactionTimeout responseChannel
        >>= respond
    _ ->
      respond $ responseLBS status400 jsonContent . Aeson.encode $ Aeson.String "Resource not found"

-- * Handlers

-- FIXME: Api specification for /commit is broken in the spec/docs.

-- | Handle request to obtain a draft commit tx.
handleDraftCommitUtxo ::
  forall tx.
  IsChainState tx =>
  Environment ->
  PParams LedgerEra ->
  Chain tx IO ->
  -- | A means to get commit info.
  IO CommitInfo ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleDraftCommitUtxo env pparams directChain getCommitInfo body = do
  case Aeson.eitherDecode' body :: Either String (DraftCommitTxRequest tx) of
    Left err ->
      pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ pack err)
    Right someCommitRequest ->
      getCommitInfo >>= \case
        NormalCommit headId ->
          case someCommitRequest of
            FullCommitRequest{blueprintTx, utxo} -> do
              draftCommit headId utxo blueprintTx
            SimpleCommitRequest{utxoToCommit} -> do
              let blueprintTx = txSpendingUTxO utxoToCommit
              draftCommit headId utxoToCommit blueprintTx
        IncrementalCommit headId -> do
          case someCommitRequest of
            FullCommitRequest{blueprintTx, utxo} -> do
              deposit headId CommitBlueprintTx{blueprintTx, lookupUTxO = utxo} Nothing
            SimpleCommitRequest{utxoToCommit, amount} ->
              deposit headId CommitBlueprintTx{blueprintTx = txSpendingUTxO utxoToCommit, lookupUTxO = utxoToCommit} amount
        CannotCommit -> pure $ responseLBS status500 [] (Aeson.encode (FailedToDraftTxNotInitializing :: PostTxError tx))
 where
  deposit headId commitBlueprint amount = do
    -- NOTE: Three times deposit period means we have one deposit period time to
    -- increment because a deposit only activates after one deposit period and
    -- expires one deposit period before deadline.
    deadline <- addUTCTime (3 * toNominalDiffTime depositPeriod) <$> getCurrentTime
    draftDepositTx headId pparams commitBlueprint deadline amount <&> \case
      Left e -> responseLBS status400 jsonContent (Aeson.encode $ toJSON e)
      Right depositTx -> okJSON $ DraftCommitTxResponse depositTx

  draftCommit headId lookupUTxO blueprintTx = do
    draftCommitTx headId CommitBlueprintTx{lookupUTxO, blueprintTx} <&> \case
      Left e ->
        -- Distinguish between errors users can actually benefit from and
        -- other errors that are turned into 500 responses.
        case e of
          CommittedTooMuchADAForMainnet _ _ -> badRequest e
          UnsupportedLegacyOutput _ -> badRequest e
          CannotFindOwnInitial _ -> badRequest e
          DepositTooLow _ _ -> badRequest e
          AmountTooLow _ _ -> badRequest e
          _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
      Right commitTx ->
        okJSON $ DraftCommitTxResponse commitTx

  Chain{draftCommitTx, draftDepositTx} = directChain

  Environment{depositPeriod} = env

-- | Handle request to recover a pending deposit.
handleRecoverCommitUtxo ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  Text ->
  LBS.ByteString ->
  IO Response
handleRecoverCommitUtxo putClientInput apiTransactionTimeout responseChannel recoverPath _body = do
  case parseTxIdFromPath recoverPath of
    Left err -> pure err
    Right recoverTxId -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput Recover{recoverTxId}
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = CommitRecovered{}} ->
                pure $ responseLBS status200 jsonContent (Aeson.encode $ Aeson.String "OK")
              Right (CommandFailed{clientInput = Recover{}}) ->
                pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String "Recover failed")
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $
            responseLBS
              status202
              jsonContent
              ( Aeson.encode $
                  object
                    [ "tag" .= Aeson.String "RecoverSubmitted"
                    , "timeout" .= Aeson.String ("Operation timed out after " <> pack (show apiTransactionTimeout) <> " seconds")
                    ]
              )
 where
  parseTxIdFromPath txIdStr =
    case Aeson.eitherDecode (encodeUtf8 txIdStr) :: Either String (TxIdType tx) of
      Left e -> Left (responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ "Cannot recover funds. Failed to parse TxId: " <> pack e))
      Right txid -> Right txid

-- | Handle request to submit a cardano transaction.
handleSubmitUserTx ::
  forall tx.
  FromJSON tx =>
  Chain tx IO ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleSubmitUserTx directChain body = do
  case Aeson.eitherDecode' body of
    Left err ->
      pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ pack err)
    Right txToSubmit -> do
      try (submitTx txToSubmit) <&> \case
        Left (e :: PostTxError Tx) -> badRequest e
        Right _ ->
          responseLBS status200 jsonContent (Aeson.encode TransactionSubmitted)
 where
  Chain{submitTx} = directChain

handleDecommit ::
  forall tx.
  FromJSON tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  LBS.ByteString ->
  IO Response
handleDecommit putClientInput apiTransactionTimeout responseChannel body =
  case Aeson.eitherDecode' body :: Either String tx of
    Left err ->
      pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ pack err)
    Right decommitTx -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput Decommit{decommitTx}
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = DecommitFinalized{}} ->
                pure $ responseLBS status200 jsonContent (Aeson.encode $ Aeson.String "OK")
              Left TimedServerOutput{output = DecommitInvalid{}} ->
                pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String "Decommit invalid")
              Right (CommandFailed{clientInput = Decommit{}}) ->
                pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String "Decommit failed")
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $
            responseLBS
              status202
              jsonContent
              ( Aeson.encode $
                  object
                    [ "tag" .= Aeson.String "DecommitSubmitted"
                    , "timeout" .= Aeson.String ("Operation timed out after " <> pack (show apiTransactionTimeout) <> " seconds")
                    ]
              )

-- | Handle request to side load confirmed snapshot.
handleSideLoadSnapshot ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  LBS.ByteString ->
  IO Response
handleSideLoadSnapshot putClientInput apiTransactionTimeout responseChannel body = do
  case Aeson.eitherDecode' body :: Either String (SideLoadSnapshotRequest tx) of
    Left err ->
      pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ pack err)
    Right SideLoadSnapshotRequest{snapshot} -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput $ SideLoadSnapshot snapshot
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = SnapshotSideLoaded{}} ->
                pure $ responseLBS status200 jsonContent (Aeson.encode $ Aeson.String "OK")
              Right (CommandFailed{clientInput = SideLoadSnapshot{}}) ->
                pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String "Side-load snapshot failed")
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $
            responseLBS
              status202
              jsonContent
              ( Aeson.encode $
                  object
                    [ "tag" .= Aeson.String "SideLoadSnapshotSubmitted"
                    , "timeout" .= Aeson.String ("Operation timed out after " <> pack (show apiTransactionTimeout) <> " seconds")
                    ]
              )

-- | Handle request to submit a transaction to the head.
handleSubmitL2Tx ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  LBS.ByteString ->
  IO Response
handleSubmitL2Tx putClientInput apiTransactionTimeout responseChannel body = do
  case Aeson.eitherDecode' @(SubmitL2TxRequest tx) body of
    Left err ->
      pure $ responseLBS status400 jsonContent (Aeson.encode $ Aeson.String $ pack err)
    Right SubmitL2TxRequest{submitL2Tx} -> do
      -- Duplicate the channel to avoid consuming messages from other consumers.
      dupChannel <- atomically $ dupTChan responseChannel

      -- Submit the transaction to the head
      putClientInput (NewTx submitL2Tx)

      let txid = txId submitL2Tx
      result <-
        timeout
          (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout))
          (waitForTransactionResult dupChannel txid)

      case result of
        Just (SubmitTxConfirmed snapshotNumber) ->
          pure $ responseLBS status200 jsonContent (Aeson.encode $ SubmitTxConfirmed snapshotNumber)
        Just (SubmitTxInvalidResponse validationError) ->
          pure $ responseLBS status400 jsonContent (Aeson.encode $ SubmitTxInvalidResponse validationError)
        Just SubmitTxSubmitted ->
          pure $ responseLBS status202 jsonContent (Aeson.encode SubmitTxSubmitted)
        Nothing ->
          -- Timeout occurred - return 202 Accepted with timeout info
          pure $
            responseLBS
              status202
              jsonContent
              ( Aeson.encode $
                  object
                    [ "tag" .= Aeson.String "SubmitTxSubmitted"
                    , "timeout" .= Aeson.String ("Transaction submission timed out after " <> pack (show apiTransactionTimeout) <> " seconds")
                    ]
              )
 where
  --  Wait for transaction result by listening to events
  waitForTransactionResult :: TChan (Either (TimedServerOutput tx) (ClientMessage tx)) -> TxIdType tx -> IO SubmitL2TxResponse
  waitForTransactionResult dupChannel txid = go
   where
    go = do
      event <- atomically $ readTChan dupChannel
      case event of
        Left (TimedServerOutput{output}) -> case output of
          TxValid{transactionId}
            | transactionId == txid ->
                pure SubmitTxSubmitted
          TxInvalid{transaction, validationError = ValidationError reason}
            | txId transaction == txid ->
                pure $ SubmitTxInvalidResponse reason
          SnapshotConfirmed{snapshot} ->
            -- Check if the transaction is in the confirmed snapshot
            if txid `elem` map txId (confirmed snapshot)
              then pure $ SubmitTxConfirmed (fromIntegral $ number snapshot)
              else go
          _ -> go
        Right _ -> go

badRequest :: IsChainState tx => PostTxError tx -> Response
badRequest = responseLBS status400 jsonContent . Aeson.encode . toJSON

notFound :: Response
notFound = responseLBS status404 jsonContent (Aeson.encode $ Aeson.String "")

okJSON :: ToJSON a => a -> Response
okJSON = responseLBS status200 jsonContent . Aeson.encode
