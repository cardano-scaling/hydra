{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.HTTPServer where

import Hydra.Prelude

import Cardano.Ledger.Core (PParams)
import Data.Aeson (KeyValue ((.=)), object, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short ()
import Data.Text (pack)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (CommitInfo (..))
import Hydra.Cardano.Api (
  LedgerEra,
  Tx,
  TxIn,
 )
import Hydra.Chain (Chain (..), PostTxError (..), draftCommitTx)
import Hydra.Chain.ChainState (
  IsChainState,
 )
import Hydra.Chain.Direct.State ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Tx (
  CommitBlueprintTx (..),
  IsTx (..),
  UTxOType,
 )
import Network.HTTP.Types (status200, status400, status404, status500)
import Network.Wai (
  Application,
  Request (pathInfo, requestMethod),
  Response,
  consumeRequestBodyStrict,
  rawPathInfo,
  responseLBS,
 )
import Test.QuickCheck (oneof)

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
    SimpleCommitRequest{utxoToCommit} ->
      toJSON utxoToCommit

instance (FromJSON tx, FromJSON (UTxOType tx)) => FromJSON (DraftCommitTxRequest tx) where
  parseJSON v = fullVariant v <|> simpleVariant v
   where
    fullVariant = withObject "FullCommitRequest" $ \o -> do
      blueprintTx :: tx <- o .: "blueprintTx"
      utxo <- o .: "utxo"
      pure FullCommitRequest{blueprintTx, utxo}

    simpleVariant val = SimpleCommitRequest <$> parseJSON val

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary =
    oneof
      [ FullCommitRequest <$> arbitrary <*> arbitrary
      , SimpleCommitRequest <$> arbitrary
      ]

  shrink = \case
    SimpleCommitRequest u -> SimpleCommitRequest <$> shrink u
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
    ("DELETE", ["commits", _]) ->
      consumeRequestBodyStrict request
        >>= handleRecoverCommitUtxo directChain putClientInput (last . fromList $ pathInfo request)
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
        IncrementalCommit headId -> do
          case someCommitRequest of
            FullCommitRequest{} -> do
              -- FIXME: deposit should work also with a blueprint tx!
              pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String "Cannot deposit with a blueprint tx.")
            SimpleCommitRequest{utxoToCommit} ->
              deposit headId utxoToCommit
        CannotCommit -> pure $ responseLBS status500 [] (Aeson.encode (FailedToDraftTxNotInitializing :: PostTxError tx))
 where
  deposit headId utxo = do
    -- TODO: How to make this configurable for testing? Right now this is just
    -- set to current time in order to have easier time testing the recover.
    -- Perhaps use contestation deadline to come up with a meaningful value?
    deadline <- getCurrentTime -- <&> addUTCTime 60
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
          CannotFindOwnInitial _ -> badRequest e
          _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
      Right commitTx ->
        okJSON $ DraftCommitTxResponse commitTx
  Chain{draftCommitTx, draftDepositTx} = directChain

-- | Handle request to recover a pending deposit.
handleRecoverCommitUtxo ::
  forall tx.
  IsChainState tx =>
  Chain tx IO ->
  (ClientInput tx -> IO ()) ->
  Text ->
  LBS.ByteString ->
  IO Response
handleRecoverCommitUtxo directChain putClientInput recoverPath _body = do
  case parseTxInFromPath recoverPath of
    Left err -> pure err
    Right recoverTxIn ->
      draftRecoverTx recoverTxIn >>= \case
        Left err -> pure $ responseLBS status500 [] (Aeson.encode $ toJSON err)
        Right recoverTx -> do
          putClientInput Recover{recoverTx}
          pure $ responseLBS status200 [] (Aeson.encode $ Aeson.String "OK")
 where
  parseTxInFromPath txInStr =
    case Aeson.eitherDecode (encodeUtf8 txInStr) :: Either String TxIn of
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
