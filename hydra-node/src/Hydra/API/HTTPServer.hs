{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.HTTPServer where

import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Data.Aeson (KeyValue ((.=)), object, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short ()
import Data.Text (pack)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (CommitInfo (..))
import Hydra.Cardano.Api (
  LedgerEra,
  SlotNo (..),
  Tx,
  TxIn,
 )
import Hydra.Chain (Chain (..), PostTxError (..), draftCommitTx)
import Hydra.Chain.ChainState (
  IsChainState,
 )
import Hydra.Chain.Direct.State ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Plutus.Extras.Time (posixFromUTCTime)
import Hydra.Tx (
  CommitBlueprintTx (..),
  HeadId,
  IsTx (..),
  UTxOType,
  headIdToCurrencySymbol,
 )
import Network.HTTP.Types (QueryItem, status200, status400, status404, status500)
import Network.Wai (
  Application,
  Request (pathInfo, requestMethod),
  Response,
  consumeRequestBodyStrict,
  queryString,
  rawPathInfo,
  responseLBS,
 )
import Test.QuickCheck (oneof, suchThat)

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
      }
  | FullCommitRequest
      { blueprintTx :: tx
      , utxo :: UTxOType tx
      }
  | IncrementalCommitDepositRequest
      { utxo :: UTxOType tx
      }
  | IncrementalCommitRecoverRequest
      { recoverHeadId :: HeadId
      , recoverUTxO :: UTxOType tx
      , depositUTxO :: UTxOType tx
      , recoverStart :: Natural
      }
  deriving stock (Generic)

deriving stock instance (Eq tx, Eq (UTxOType tx)) => Eq (DraftCommitTxRequest tx)
deriving stock instance (Show tx, Show (UTxOType tx)) => Show (DraftCommitTxRequest tx)

instance (ToJSON tx, ToJSON (UTxOType tx)) => ToJSON (DraftCommitTxRequest tx) where
  toJSON = \case
    IncrementalCommitRecoverRequest{recoverHeadId, recoverUTxO, depositUTxO, recoverStart} ->
      object
        [ "recoverHeadId" .= toJSON recoverHeadId
        , "recoverUTxO" .= toJSON recoverUTxO
        , "depositUTxO" .= toJSON depositUTxO
        , "recoverStart" .= toJSON recoverStart
        ]
    IncrementalCommitDepositRequest{utxo} ->
      object
        [ "utxo" .= toJSON utxo
        ]
    FullCommitRequest{blueprintTx, utxo} ->
      object
        [ "blueprintTx" .= toJSON blueprintTx
        , "utxo" .= toJSON utxo
        ]
    SimpleCommitRequest{utxoToCommit} ->
      toJSON utxoToCommit

instance (FromJSON tx, FromJSON (UTxOType tx)) => FromJSON (DraftCommitTxRequest tx) where
  parseJSON v = fullVariant v <|> simpleVariant v <|> depositVariant v <|> recoverVariant v
   where
    fullVariant = withObject "FullCommitRequest" $ \o -> do
      blueprintTx :: tx <- o .: "blueprintTx"
      utxo <- o .: "utxo"
      pure FullCommitRequest{blueprintTx, utxo}

    simpleVariant val = SimpleCommitRequest <$> parseJSON val

    depositVariant = withObject "IncrementalCommitDepositRequest" $ \o -> do
      utxo <- o .: "utxo"
      pure IncrementalCommitDepositRequest{utxo}

    recoverVariant = withObject "IncrementalCommitRecoverRequest" $ \o -> do
      recoverHeadId <- o .: "recoverHeadId"
      recoverUTxO <- o .: "recoverUTxO"
      depositUTxO <- o .: "depositUTxO"
      recoverStart <- o .: "recoverStart"
      pure IncrementalCommitRecoverRequest{recoverHeadId, recoverUTxO, depositUTxO, recoverStart}

instance (Arbitrary tx, Arbitrary (UTxOType tx), Eq (UTxOType tx), Monoid (UTxOType tx)) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary =
    oneof
      [ FullCommitRequest <$> arbitrary <*> arbitrary
      , SimpleCommitRequest <$> arbitrary
      , IncrementalCommitDepositRequest <$> arbitrary `suchThat` (/= mempty)
      , IncrementalCommitRecoverRequest <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      ]

  shrink = \case
    SimpleCommitRequest u -> SimpleCommitRequest <$> shrink u
    FullCommitRequest a b -> FullCommitRequest <$> shrink a <*> shrink b
    IncrementalCommitDepositRequest a -> IncrementalCommitDepositRequest <$> shrink a
    IncrementalCommitRecoverRequest a b c d -> IncrementalCommitRecoverRequest <$> shrink a <*> shrink b <*> shrink c <*> shrink d

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

-- | Hydra HTTP server
httpApp ::
  forall tx.
  IsChainState tx =>
  Tracer IO APIServerLog ->
  Chain tx IO ->
  PParams LedgerEra ->
  -- | A means to get commit info.
  IO CommitInfo ->
  -- | Get latest confirmed UTxO snapshot.
  IO (Maybe (UTxOType tx)) ->
  -- | Callback to yield a 'ClientInput' to the main event loop.
  (ClientInput tx -> IO ()) ->
  Application
httpApp tracer directChain pparams getCommitInfo getConfirmedUTxO putClientInput request respond = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod request
      , path = PathInfo $ rawPathInfo request
      }
  case (requestMethod request, pathInfo request) of
    ("GET", ["snapshot", "utxo"]) ->
      -- XXX: Should ensure the UTxO is of the right head and the head is still
      -- open. This is something we should fix on the "read model" side of the
      -- server.
      getConfirmedUTxO >>= \case
        Nothing -> respond notFound
        Just utxo -> respond $ okJSON utxo
    ("POST", ["commit"]) ->
      consumeRequestBodyStrict request
        >>= handleDraftCommitUtxo directChain getCommitInfo
        >>= respond
    ("DELETE", ["commits"]) ->
      consumeRequestBodyStrict request
        >>= handleRecoverCommitUtxo directChain getCommitInfo putClientInput (queryString request)
        >>= respond
    ("POST", ["decommit"]) ->
      consumeRequestBodyStrict request
        >>= handleDecommit putClientInput
        >>= respond
    ("GET", ["protocol-parameters"]) ->
      respond . responseLBS status200 [] . Aeson.encode $ pparams
    ("POST", ["cardano-transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitUserTx directChain
        >>= respond
    _ ->
      respond $ responseLBS status400 [] "Resource not found"

-- * Handlers

-- FIXME: Api specification for /commit is broken in the spec/docs.

-- | Handle request to obtain a draft commit tx.
handleDraftCommitUtxo ::
  forall tx.
  IsChainState tx =>
  Chain tx IO ->
  -- | A means to get commit info.
  IO CommitInfo ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleDraftCommitUtxo directChain getCommitInfo body = do
  case Aeson.eitherDecode' body :: Either String (DraftCommitTxRequest tx) of
    Left err ->
      pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right someCommitRequest ->
      getCommitInfo >>= \case
        NormalCommit headId ->
          case someCommitRequest of
            FullCommitRequest{blueprintTx, utxo} -> do
              draftCommit headId utxo blueprintTx
            SimpleCommitRequest{utxoToCommit} -> do
              let blueprintTx = txSpendingUTxO utxoToCommit
              draftCommit headId utxoToCommit blueprintTx
            _ -> pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String "Invalid request: expected a FullCommitRequest or SimpleCommitRequest")
        IncrementalCommit headId -> do
          case someCommitRequest of
            IncrementalCommitDepositRequest{utxo} ->
              deposit headId utxo
            _ -> pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String "Invalid request: expected a IncrementalCommitDepositRequest")
        -- XXX: This is not really an internal server error
        -- FIXME: FailedToDraftTxNotInitializing is not a good name and it's not a PostTxError. Should create an error type somewhere in Hydra.API for this.
        CannotCommit -> pure $ responseLBS status500 [] (Aeson.encode (FailedToDraftTxNotInitializing :: PostTxError tx))
 where
  deposit headId utxo = do
    -- TODO: How to make this configurable for testing?
    deadline <- getCurrentTime <&> addUTCTime 60
    draftDepositTx headId utxo deadline <&> \case
      Left e -> responseLBS status400 [] (Aeson.encode $ toJSON e)
      Right depositTx -> okJSON $ DraftCommitTxResponse depositTx

  draftCommit headId lookupUTxO blueprintTx = do
    draftCommitTx headId CommitBlueprintTx{lookupUTxO, blueprintTx} <&> \case
      Left e ->
        -- Distinguish between errors users can actually benefit from and
        -- other errors that are turned into 500 responses.
        case e of
          CommittedTooMuchADAForMainnet _ _ -> badRequest e
          UnsupportedLegacyOutput _ -> badRequest e
          _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
      Right commitTx ->
        okJSON $ DraftCommitTxResponse commitTx
  Chain{draftCommitTx, draftDepositTx} = directChain

-- | Handle request to recover a pending deposit.
handleRecoverCommitUtxo ::
  forall tx.
  IsChainState tx =>
  Chain tx IO ->
  -- | A means to get commit info.
  IO CommitInfo ->
  (ClientInput tx -> IO ()) ->
  [QueryItem] ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleRecoverCommitUtxo directChain getCommitInfo putClientInput recoverQuery body = do
  case Aeson.eitherDecode' body :: Either String (DraftCommitTxRequest tx) of
    Left err ->
      pure (responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err))
    Right someCommitRequest ->
      getCommitInfo >>= \case
        IncrementalCommit _ -> do
          case someCommitRequest of
            IncrementalCommitRecoverRequest{recoverHeadId, recoverUTxO, depositUTxO, recoverStart} -> do
              deadline <- posixFromUTCTime <$> getCurrentTime
              case checkRecover recoverQuery of
                Left err -> pure err
                Right recoverTxIn ->
                  recoverDeposit recoverHeadId recoverUTxO depositUTxO deadline recoverStart recoverTxIn >>= \case
                    Left err -> pure $ responseLBS status500 [] (Aeson.encode $ toJSON err)
                    Right recoverTx -> do
                      putClientInput Recover{recoverTx}
                      pure $ responseLBS status200 [] (Aeson.encode $ Aeson.String "OK")
            _ -> pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String "Invalid request: expected a IncrementalCommitRecoverRequest")
        _ -> pure (responseLBS status500 [] (Aeson.encode (FailedToDraftTxNotInitializing :: PostTxError tx)))
 where
  recoverDeposit recoverHeadId recoverUTxO depositUTxO recoverDeadline recoverStart txIn = do
    draftRecoverTx (headIdToCurrencySymbol recoverHeadId) recoverUTxO depositUTxO recoverDeadline (SlotNo $ fromIntegral recoverStart) txIn <&> \case
      Left e -> Left e
      Right recoverTx -> Right recoverTx

  checkRecover query =
    case Base16.decode $ foldMap fst query of
      Left e -> Left (responseLBS status400 [] (Aeson.encode $ Aeson.String $ "Cannot recover funds. Failed to decode TxIn string: " <> pack e))
      Right txInStr ->
        case Aeson.eitherDecode (fromStrict txInStr) :: Either String TxIn of
          Left e -> Left (responseLBS status400 [] (Aeson.encode $ Aeson.String $ "Cannot recover funds. Failed to parse TxIn: " <> pack e))
          Right txIn -> Right txIn

  Chain{draftRecoverTx} = directChain

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
      pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right txToSubmit -> do
      try (submitTx txToSubmit) <&> \case
        Left (e :: PostTxError Tx) -> badRequest e
        Right _ ->
          responseLBS status200 [] (Aeson.encode TransactionSubmitted)
 where
  Chain{submitTx} = directChain

handleDecommit :: forall tx. FromJSON tx => (ClientInput tx -> IO ()) -> LBS.ByteString -> IO Response
handleDecommit putClientInput body =
  case Aeson.eitherDecode' body :: Either String tx of
    Left err ->
      pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right decommitTx -> do
      putClientInput Decommit{decommitTx}
      pure $ responseLBS status200 [] (Aeson.encode $ Aeson.String "OK")

badRequest :: IsChainState tx => PostTxError tx -> Response
badRequest = responseLBS status400 [] . Aeson.encode . toJSON

notFound :: Response
notFound = responseLBS status404 [] ""

okJSON :: ToJSON a => a -> Response
okJSON = responseLBS status200 [] . Aeson.encode
