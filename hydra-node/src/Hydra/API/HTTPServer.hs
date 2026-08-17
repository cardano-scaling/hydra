{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.HTTPServer where

import Hydra.Prelude

import Cardano.Ledger.Binary (encCBOR, toPlainEncoding)
import Cardano.Ledger.Core (PParams)
import Codec.CBOR.Write qualified as CBOR
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Data.Aeson (KeyValue ((.=)), object, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short ()
import Data.List qualified as List
import Data.Text (pack)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ApiEncoding (..), ClientMessage (..), CommitInfo (..), ServerOutput (..), TimedServerOutput (..), getConfirmedSnapshot, getSeenSnapshot, getSnapshotUtxo)
import Hydra.API.WireFormat (decodeWire, encodeWire)
import Hydra.CBOR.Orphans ()
import Hydra.Cardano.Api (AddressInEra, LedgerEra, SlotNo, Tx, ledgerEraVersion)
import Hydra.Chain (Chain (..), PostTxError (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger (ValidationError (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node.ApiTransactionTimeout (ApiTransactionTimeout (..))
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Tx (CommitBlueprintTx (..), ConfirmedSnapshot, IsTx (..), Snapshot (..), UTxOType)
import Hydra.Tx.DepositPeriod (toNominalDiffTime)
import Network.HTTP.Types (ResponseHeaders, Status, hAccept, hContentType, status200, status202, status400, status404, status500, status503)
import Network.Wai (Application, Request (pathInfo, requestMethod), Response, consumeRequestBodyStrict, rawPathInfo, requestHeaders, responseLBS)

newtype DraftCommitTxResponse tx = DraftCommitTxResponse
  { commitTx :: tx
  }
  deriving stock (Generic)

deriving stock instance Show tx => Show (DraftCommitTxResponse tx)

instance IsTx tx => ToJSON (DraftCommitTxResponse tx) where
  toJSON (DraftCommitTxResponse tx) = toJSON tx

instance IsTx tx => FromJSON (DraftCommitTxResponse tx) where
  parseJSON v = DraftCommitTxResponse <$> parseJSON v

instance IsTx tx => ToCBOR (DraftCommitTxResponse tx) where
  toCBOR (DraftCommitTxResponse tx) = toCBOR tx

instance IsTx tx => FromCBOR (DraftCommitTxResponse tx) where
  fromCBOR = DraftCommitTxResponse <$> fromCBOR

data DraftCommitTxRequest tx
  = SimpleCommitRequest
      { utxoToCommit :: UTxOType tx
      }
  | FullCommitRequest
      { blueprintTx :: tx
      , utxo :: UTxOType tx
      , changeAddress :: Maybe AddressInEra
      }
  deriving stock (Generic)

deriving stock instance (Eq tx, Eq (UTxOType tx)) => Eq (DraftCommitTxRequest tx)
deriving stock instance (Show tx, Show (UTxOType tx)) => Show (DraftCommitTxRequest tx)

instance (ToJSON tx, ToJSON (UTxOType tx)) => ToJSON (DraftCommitTxRequest tx) where
  toJSON = \case
    FullCommitRequest{blueprintTx, utxo, changeAddress} ->
      object
        [ "blueprintTx" .= toJSON blueprintTx
        , "utxo" .= toJSON utxo
        , "changeAddress" .= toJSON changeAddress
        ]
    SimpleCommitRequest{utxoToCommit} ->
      object
        [ "utxoToCommit" .= toJSON utxoToCommit
        ]

instance (FromJSON tx, FromJSON (UTxOType tx)) => FromJSON (DraftCommitTxRequest tx) where
  parseJSON v = fullVariant v <|> simpleVariant v <|> simpleDirectVariant v
   where
    fullVariant = withObject "FullCommitRequest" $ \o -> do
      blueprintTx :: tx <- o .: "blueprintTx"
      utxo <- o .: "utxo"
      changeAddress <- o .:? "changeAddress"
      pure FullCommitRequest{blueprintTx, utxo, changeAddress}

    simpleVariant = withObject "SimpleCommitRequest" $ \o -> do
      utxoToCommit <- o .: "utxoToCommit"
      pure SimpleCommitRequest{utxoToCommit}

    simpleDirectVariant :: Aeson.Value -> Parser (DraftCommitTxRequest tx)
    simpleDirectVariant val = SimpleCommitRequest <$> parseJSON val

instance IsTx tx => ToCBOR (DraftCommitTxRequest tx) where
  toCBOR = \case
    SimpleCommitRequest{utxoToCommit} ->
      toCBOR ("SimpleCommitRequest" :: Text) <> toCBOR utxoToCommit
    FullCommitRequest{blueprintTx, utxo, changeAddress} ->
      toCBOR ("FullCommitRequest" :: Text)
        <> toCBOR blueprintTx
        <> toCBOR utxo
        <> toCBOR changeAddress

instance IsTx tx => FromCBOR (DraftCommitTxRequest tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("SimpleCommitRequest" :: Text) -> SimpleCommitRequest <$> fromCBOR
      "FullCommitRequest" -> FullCommitRequest <$> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded DraftCommitTxRequest"

newtype SubmitTxRequest tx = SubmitTxRequest
  { txToSubmit :: tx
  }
  deriving newtype (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance (Typeable tx, ToCBOR tx) => ToCBOR (SubmitTxRequest tx)
deriving newtype instance (Typeable tx, FromCBOR tx) => FromCBOR (SubmitTxRequest tx)

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

instance ToCBOR TransactionSubmitted where
  toCBOR TransactionSubmitted = toCBOR ("TransactionSubmitted" :: Text)

instance FromCBOR TransactionSubmitted where
  fromCBOR =
    fromCBOR >>= \case
      ("TransactionSubmitted" :: Text) -> pure TransactionSubmitted
      tag -> fail $ show tag <> " is not a proper CBOR-encoded TransactionSubmitted"

newtype SideLoadSnapshotRequest tx = SideLoadSnapshotRequest
  { snapshot :: ConfirmedSnapshot tx
  }
  deriving newtype (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance IsTx tx => ToCBOR (SideLoadSnapshotRequest tx)
deriving newtype instance IsTx tx => FromCBOR (SideLoadSnapshotRequest tx)

-- | Request to submit a transaction to the head
newtype SubmitL2TxRequest tx = SubmitL2TxRequest
  { submitL2Tx :: tx
  }
  deriving newtype (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

deriving newtype instance (Typeable tx, ToCBOR tx) => ToCBOR (SubmitL2TxRequest tx)
deriving newtype instance (Typeable tx, FromCBOR tx) => FromCBOR (SubmitL2TxRequest tx)

-- | Response for transaction submission
data SubmitL2TxResponse
  = -- | Transaction was included in a confirmed snapshot
    SubmitTxConfirmed Integer
  | -- | Transaction was rejected due to validation errors
    SubmitTxInvalidResponse Text
  | -- | Transaction was rejected due to node out of sync
    SubmitTxRejectedResponse Text
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
    SubmitTxRejectedResponse reason ->
      object
        [ "tag" .= Aeson.String "SubmitTxRejected"
        , "reason" .= reason
        ]
    SubmitTxSubmitted -> object ["tag" .= Aeson.String "SubmitTxSubmitted"]

instance FromJSON SubmitL2TxResponse where
  parseJSON = withObject "SubmitTxResponse" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "SubmitTxConfirmed" -> SubmitTxConfirmed <$> o .: "snapshotNumber"
      "SubmitTxInvalid" -> SubmitTxInvalidResponse <$> o .: "validationError"
      "SubmitTxRejected" -> SubmitTxRejectedResponse <$> o .: "reason"
      "SubmitTxSubmitted" -> pure SubmitTxSubmitted
      _ -> fail "Expected tag to be SubmitTxConfirmed, SubmitTxInvalid, SubmitTxRejected, or SubmitTxSubmitted"

-- NOTE: Tags are kept consistent with the JSON encoding above.
instance ToCBOR SubmitL2TxResponse where
  toCBOR = \case
    SubmitTxConfirmed snapshotNumber ->
      toCBOR ("SubmitTxConfirmed" :: Text) <> toCBOR snapshotNumber
    SubmitTxInvalidResponse validationError ->
      toCBOR ("SubmitTxInvalid" :: Text) <> toCBOR validationError
    SubmitTxRejectedResponse reason ->
      toCBOR ("SubmitTxRejected" :: Text) <> toCBOR reason
    SubmitTxSubmitted ->
      toCBOR ("SubmitTxSubmitted" :: Text)

instance FromCBOR SubmitL2TxResponse where
  fromCBOR =
    fromCBOR >>= \case
      ("SubmitTxConfirmed" :: Text) -> SubmitTxConfirmed <$> fromCBOR
      "SubmitTxInvalid" -> SubmitTxInvalidResponse <$> fromCBOR
      "SubmitTxRejected" -> SubmitTxRejectedResponse <$> fromCBOR
      "SubmitTxSubmitted" -> pure SubmitTxSubmitted
      tag -> fail $ show tag <> " is not a proper CBOR-encoded SubmitL2TxResponse"

data HeadInitializationDetails
  = HeadInitializationDetails
  { time :: UTCTime
  , slot :: SlotNo
  }
  deriving stock (Eq, Show)

jsonContent :: ResponseHeaders
jsonContent = [(hContentType, "application/json")]

cborContent :: ResponseHeaders
cborContent = [(hContentType, "application/cbor")]

-- | Response body sent when an operation was accepted but did not finish
-- within the API transaction timeout.
data OperationTimedOut = OperationTimedOut
  { tag :: Text
  , timeoutMessage :: Text
  }
  deriving stock (Eq, Show, Generic)

-- NOTE: Encoded with a "timeout" key for backwards compatibility (the field
-- is named differently to avoid clashing with 'Hydra.Prelude.timeout').
instance ToJSON OperationTimedOut where
  toJSON OperationTimedOut{tag, timeoutMessage} =
    object ["tag" .= tag, "timeout" .= timeoutMessage]

instance FromJSON OperationTimedOut where
  parseJSON = withObject "OperationTimedOut" $ \o ->
    OperationTimedOut <$> o .: "tag" <*> o .: "timeout"

instance ToCBOR OperationTimedOut where
  toCBOR OperationTimedOut{tag, timeoutMessage} =
    toCBOR ("OperationTimedOut" :: Text) <> toCBOR tag <> toCBOR timeoutMessage

instance FromCBOR OperationTimedOut where
  fromCBOR =
    fromCBOR >>= \case
      ("OperationTimedOut" :: Text) -> OperationTimedOut <$> fromCBOR <*> fromCBOR
      other -> fail $ show other <> " is not a proper CBOR-encoded OperationTimedOut"

operationTimedOut :: Text -> ApiTransactionTimeout -> OperationTimedOut
operationTimedOut tag apiTransactionTimeout =
  OperationTimedOut
    { tag
    , timeoutMessage = "Operation timed out after " <> pack (show apiTransactionTimeout) <> " seconds"
    }

-- | Which encoding the client wants for the response, negotiated via the
-- @Accept@ header. Anything but @application/cbor@ (including no header)
-- yields JSON.
responseEncodingFor :: Request -> ApiEncoding
responseEncodingFor request =
  case List.lookup hAccept (requestHeaders request) of
    Just accept | "application/cbor" `BS.isInfixOf` accept -> CborEncoding
    _ -> JsonEncoding

-- | Which encoding the request body uses, negotiated via the @Content-Type@
-- header. Anything but @application/cbor@ (including no header) is treated
-- as JSON.
requestEncodingFor :: Request -> ApiEncoding
requestEncodingFor request =
  case List.lookup hContentType (requestHeaders request) of
    Just contentType | "application/cbor" `BS.isInfixOf` contentType -> CborEncoding
    _ -> JsonEncoding

-- | Respond in the given encoding, with matching @Content-Type@.
respondApi :: (ToJSON a, ToCBOR a) => ApiEncoding -> Status -> a -> Response
respondApi apiEncoding status a =
  responseLBS status contentType (encodeWire apiEncoding a)
 where
  contentType = case apiEncoding of
    JsonEncoding -> jsonContent
    CborEncoding -> cborContent

-- | Hydra HTTP server
httpApp ::
  forall tx.
  IsChainState tx =>
  Tracer IO APIServerLog ->
  -- | Pre-rendered effective configuration (served at GET /config).
  Aeson.Value ->
  Chain tx IO ->
  Environment ->
  PParams LedgerEra ->
  -- | Get latest 'NodeState'.
  IO (NodeState tx) ->
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
httpApp tracer configDoc directChain env pparams getNodeState getCommitInfo getPendingDeposits putClientInput apiTransactionTimeout responseChannel request respond = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod request
      , path = PathInfo $ rawPathInfo request
      }
  case (requestMethod request, pathInfo request) of
    ("GET", ["config"]) ->
      respond $ respondApi respEnc status200 configDoc
    ("GET", ["head"]) ->
      getNodeState >>= (respond . respondApi respEnc status200) . headState
    ("GET", ["snapshot"]) -> do
      hs <- headState <$> getNodeState
      case getConfirmedSnapshot hs of
        Just confirmedSnapshot -> respond $ respondApi respEnc status200 confirmedSnapshot
        Nothing -> respond $ notFound respEnc
    ("GET", ["snapshot", "utxo"]) -> do
      hs <- headState <$> getNodeState
      case getSnapshotUtxo hs of
        Just utxo -> respond $ respondApi respEnc status200 utxo
        _ -> respond $ notFound respEnc
    ("GET", ["snapshot", "last-seen"]) -> do
      hs <- headState <$> getNodeState
      respond . respondApi respEnc status200 $ getSeenSnapshot hs
    ("POST", ["snapshot"]) ->
      consumeRequestBodyStrict request
        >>= handleSideLoadSnapshot putClientInput apiTransactionTimeout responseChannel reqEnc respEnc
        >>= respond
    ("POST", ["commit"]) ->
      consumeRequestBodyStrict request
        >>= handleDraftCommitUtxo tracer env pparams directChain getNodeState getCommitInfo reqEnc respEnc
        >>= respond
    ("DELETE", ["commits", _]) ->
      consumeRequestBodyStrict request
        >>= handleRecoverCommitUtxo putClientInput apiTransactionTimeout responseChannel (last . fromList $ pathInfo request) respEnc
        >>= respond
    ("GET", ["commits"]) ->
      getPendingDeposits >>= respond . respondApi respEnc status200
    ("POST", ["decommit"]) ->
      consumeRequestBodyStrict request
        >>= handleDecommit putClientInput apiTransactionTimeout responseChannel reqEnc respEnc
        >>= respond
    ("GET", ["protocol-parameters"]) ->
      respond $ respondPParams respEnc pparams
    ("POST", ["cardano-transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitUserTx directChain reqEnc respEnc
        >>= respond
    ("POST", ["transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitL2Tx putClientInput apiTransactionTimeout responseChannel reqEnc respEnc
        >>= respond
    _ ->
      respond $ respondApi respEnc status400 ("Resource not found" :: Text)
 where
  reqEnc = requestEncodingFor request
  respEnc = responseEncodingFor request

-- | Respond with protocol parameters; the ledger 'PParams' have no plain
-- 'ToCBOR' instance, so the CBOR case goes through the ledger's 'EncCBOR'.
respondPParams :: ApiEncoding -> PParams LedgerEra -> Response
respondPParams apiEncoding pparams =
  case apiEncoding of
    JsonEncoding -> responseLBS status200 jsonContent (Aeson.encode pparams)
    CborEncoding ->
      responseLBS status200 cborContent $
        CBOR.toLazyByteString (toPlainEncoding ledgerEraVersion $ encCBOR pparams)

-- * Handlers

-- FIXME: Api specification for /commit is broken in the spec/docs.

-- | Handle request to obtain a draft commit tx.
handleDraftCommitUtxo ::
  forall tx.
  IsChainState tx =>
  Tracer IO APIServerLog ->
  Environment ->
  PParams LedgerEra ->
  Chain tx IO ->
  -- | Get latest 'NodeState'.
  IO (NodeState tx) ->
  -- | A means to get commit info.
  IO CommitInfo ->
  -- | Request body encoding.
  ApiEncoding ->
  -- | Response encoding.
  ApiEncoding ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleDraftCommitUtxo tracer env pparams directChain getNodeState getCommitInfo reqEnc respEnc body = do
  case decodeWire reqEnc body :: Either String (DraftCommitTxRequest tx) of
    Left err -> do
      traceWith tracer $
        APIInvalidInput
          { reason = "Failed to parse request to DraftCommitTxRequest: " <> show err
          , inputReceived = show body
          }
      pure $ respondApi respEnc status400 (pack err)
    Right someCommitRequest ->
      getCommitInfo >>= \case
        IncrementalCommit headId -> do
          case someCommitRequest of
            FullCommitRequest{blueprintTx, utxo, changeAddress} -> do
              deposit headId CommitBlueprintTx{blueprintTx, lookupUTxO = utxo} changeAddress
            SimpleCommitRequest{utxoToCommit} ->
              deposit headId CommitBlueprintTx{blueprintTx = txSpendingUTxO utxoToCommit, lookupUTxO = utxoToCommit} Nothing
        CannotCommit -> do
          traceWith tracer $
            APIInvalidInput
              { reason = "CannotCommit: Hydra node does not have an open Head."
              , inputReceived = show body
              }
          pure $ respondApi respEnc status400 ("Head is not open" :: Text)
 where
  deposit headId commitBlueprint changeAddress = do
    nodeState <- getNodeState
    case getConfirmedSnapshot (headState nodeState) of
      Nothing -> do
        traceWith tracer $
          APIInvalidInput
            { reason = "Cannot commit: Hydra node does not have an open Head."
            , inputReceived = show body
            }
        pure $ respondApi respEnc status400 ("Head is not open" :: Text)
      Just currentSnapshot -> do
        -- NOTE: The deadline splits into three independent windows: a deposit
        -- matures (becomes active) after 'depositActivation', stays active for one
        -- 'depositPeriod', and can be recovered one 'depositPeriod' before the
        -- deadline. Hence deadline = now + depositActivation + 2 x depositPeriod.
        deadline <-
          addUTCTime (toNominalDiffTime depositActivation + 2 * toNominalDiffTime depositPeriod)
            <$> getCurrentTime
        result <- draftDepositTx headId pparams currentSnapshot commitBlueprint deadline changeAddress
        case result of
          Left e ->
            case e of
              UnsupportedLegacyOutput _ -> pure $ badRequest respEnc e
              DepositTooLow _ _ -> pure $ badRequest respEnc e
              DepositTooLarge{} -> pure $ badRequest respEnc e
              FailedToConstructDepositTx _ -> pure $ badRequest respEnc e
              _ -> do
                traceWith tracer $
                  APIReturnedError
                    { reason = "Failed to draft deposit transaction: " <> show e
                    }
                pure $ respondApi respEnc status500 e
          Right depositTx -> pure $ respondApi respEnc status200 $ DraftCommitTxResponse depositTx

  Chain{draftDepositTx} = directChain

  Environment{depositPeriod, depositActivation} = env

-- | Handle request to recover a pending deposit.
handleRecoverCommitUtxo ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  Text ->
  ApiEncoding ->
  LBS.ByteString ->
  IO Response
handleRecoverCommitUtxo putClientInput apiTransactionTimeout responseChannel recoverPath respEnc _body = do
  case parseTxIdFromPath recoverPath of
    Left err -> pure err
    Right recoverTxId -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput Recover{recoverTxId}
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = CommitRecovered{}} ->
                pure $ respondApi respEnc status200 ("OK" :: Text)
              Right (CommandFailed{clientInput = Recover{}}) ->
                pure $ respondApi respEnc status400 ("Recover failed" :: Text)
              Right (RejectedInputBecauseUnsynced{clientInput = Recover{}, drift}) ->
                pure $ respondApi respEnc status503 ("Recover failed because node is out of sync with chain, drift: " <> show drift :: Text)
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $ respondApi respEnc status202 $ operationTimedOut "RecoverSubmitted" apiTransactionTimeout
 where
  parseTxIdFromPath :: Text -> Either Response (TxIdType tx)
  parseTxIdFromPath txIdStr =
    -- First try parsing as a raw JSON value (for backwards compatibility with numeric IDs)
    -- then fall back to parsing as a JSON String (for hex-encoded TxIds)
    case Aeson.eitherDecode (LBS.fromStrict $ encodeUtf8 txIdStr) of
      Right txid -> Right txid
      Left _ -> case parseEither parseJSON (Aeson.String txIdStr) of
        Right txid -> Right txid
        Left e -> Left $ respondApi respEnc status400 ("Cannot recover funds. Failed to parse TxId: " <> pack e)

-- | Handle request to submit a cardano transaction.
handleSubmitUserTx ::
  forall tx.
  (FromJSON tx, FromCBOR tx) =>
  Chain tx IO ->
  -- | Request body encoding.
  ApiEncoding ->
  -- | Response encoding.
  ApiEncoding ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleSubmitUserTx directChain reqEnc respEnc body = do
  case decodeWire reqEnc body of
    Left err ->
      pure $ respondApi respEnc status400 (pack err)
    Right txToSubmit -> do
      try (submitTx txToSubmit) <&> \case
        Left (e :: PostTxError Tx) -> badRequest respEnc e
        Right _ ->
          respondApi respEnc status200 TransactionSubmitted
 where
  Chain{submitTx} = directChain

handleDecommit ::
  forall tx.
  (FromJSON tx, FromCBOR tx) =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ApiEncoding ->
  ApiEncoding ->
  LBS.ByteString ->
  IO Response
handleDecommit putClientInput apiTransactionTimeout responseChannel reqEnc respEnc body =
  case decodeWire reqEnc body :: Either String tx of
    Left err ->
      pure $ respondApi respEnc status400 (pack err)
    Right decommitTx -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput Decommit{decommitTx}
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = DecommitFinalized{}} ->
                pure $ respondApi respEnc status200 ("OK" :: Text)
              Left TimedServerOutput{output = DecommitInvalid{}} ->
                pure $ respondApi respEnc status400 ("Decommit invalid" :: Text)
              Right (CommandFailed{clientInput = Decommit{}}) ->
                pure $ respondApi respEnc status400 ("Decommit failed" :: Text)
              Right (RejectedInputBecauseUnsynced{clientInput = Decommit{}, drift}) ->
                pure $ respondApi respEnc status503 ("Decommit failed because because node is out of sync with chain, drift: " <> show drift :: Text)
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $ respondApi respEnc status202 $ operationTimedOut "DecommitSubmitted" apiTransactionTimeout

-- | Handle request to side load confirmed snapshot.
handleSideLoadSnapshot ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ApiEncoding ->
  ApiEncoding ->
  LBS.ByteString ->
  IO Response
handleSideLoadSnapshot putClientInput apiTransactionTimeout responseChannel reqEnc respEnc body = do
  case decodeWire reqEnc body :: Either String (SideLoadSnapshotRequest tx) of
    Left err ->
      pure $ respondApi respEnc status400 (pack err)
    Right SideLoadSnapshotRequest{snapshot} -> do
      dupChannel <- atomically $ dupTChan responseChannel
      putClientInput $ SideLoadSnapshot snapshot
      let wait = do
            event <- atomically $ readTChan dupChannel
            case event of
              Left TimedServerOutput{output = SnapshotSideLoaded{}} ->
                pure $ respondApi respEnc status200 ("OK" :: Text)
              Right (SideLoadSnapshotRejected{clientInput = SideLoadSnapshot{}, requirementFailure}) ->
                pure $ respondApi respEnc status400 requirementFailure
              Right (CommandFailed{clientInput = SideLoadSnapshot{}}) ->
                pure $ respondApi respEnc status400 ("Side-load snapshot failed" :: Text)
              Right (RejectedInputBecauseUnsynced{clientInput = SideLoadSnapshot{}, drift}) ->
                pure $ respondApi respEnc status503 ("Side-load snapshot failed because node is out of sync with chain, drift: " <> show drift :: Text)
              _ -> wait
      timeout (realToFrac (apiTransactionTimeoutNominalDiffTime apiTransactionTimeout)) wait >>= \case
        Just r -> pure r
        Nothing ->
          pure $ respondApi respEnc status202 $ operationTimedOut "SideLoadSnapshotSubmitted" apiTransactionTimeout

-- | Handle request to submit a transaction to the head.
handleSubmitL2Tx ::
  forall tx.
  IsChainState tx =>
  (ClientInput tx -> IO ()) ->
  ApiTransactionTimeout ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ApiEncoding ->
  ApiEncoding ->
  LBS.ByteString ->
  IO Response
handleSubmitL2Tx putClientInput apiTransactionTimeout responseChannel reqEnc respEnc body = do
  case decodeWire @(SubmitL2TxRequest tx) reqEnc body of
    Left err ->
      pure $ respondApi respEnc status400 (pack err)
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
          pure $ respondApi respEnc status200 (SubmitTxConfirmed snapshotNumber)
        Just (SubmitTxInvalidResponse validationError) ->
          pure $ respondApi respEnc status400 (SubmitTxInvalidResponse validationError)
        Just (SubmitTxRejectedResponse reason) ->
          pure $ respondApi respEnc status503 (SubmitTxRejectedResponse reason)
        Just SubmitTxSubmitted ->
          pure $ respondApi respEnc status202 SubmitTxSubmitted
        Nothing ->
          -- Timeout occurred - return 202 Accepted with timeout info
          pure $
            respondApi respEnc status202 $
              OperationTimedOut
                { tag = "SubmitTxSubmitted"
                , timeoutMessage = "Transaction submission timed out after " <> pack (show apiTransactionTimeout) <> " seconds"
                }
 where
  --  Wait for transaction result by listening to events
  waitForTransactionResult :: TChan (Either (TimedServerOutput tx) (ClientMessage tx)) -> TxIdType tx -> IO SubmitL2TxResponse
  waitForTransactionResult dupChannel txid = go
   where
    go = do
      event <- atomically $ readTChan dupChannel
      case event of
        Right (RejectedInputBecauseUnsynced{clientInput = NewTx{}, drift}) -> do
          pure $ SubmitTxRejectedResponse $ "Node is out of sync with chain, drift: " <> show drift
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

badRequest :: IsChainState tx => ApiEncoding -> PostTxError tx -> Response
badRequest apiEncoding = respondApi apiEncoding status400

notFound :: ApiEncoding -> Response
notFound apiEncoding = respondApi apiEncoding status404 ("" :: Text)

okJSON :: ToJSON a => a -> Response
okJSON = responseLBS status200 jsonContent . Aeson.encode
