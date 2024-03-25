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
import Hydra.Cardano.Api (
  HasTextEnvelope,
  LedgerEra,
  Tx,
  deserialiseFromTextEnvelope,
  fromLedgerPParams,
  proxyToAsType,
  serialiseToTextEnvelope,
  shelleyBasedEra,
  txSpendingUTxO,
 )
import Hydra.Chain (Chain (..), CommitBlueprintTx (..), IsChainState, PostTxError (..), draftCommitTx)
import Hydra.Chain.Direct.State ()
import Hydra.HeadId (HeadId)
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Tracer, traceWith)
import Network.HTTP.Types (status200, status400, status404, status500)
import Network.Wai (
  Application,
  Request (pathInfo, requestMethod),
  Response,
  consumeRequestBodyStrict,
  rawPathInfo,
  responseLBS,
 )

newtype DraftCommitTxResponse tx = DraftCommitTxResponse
  { commitTx :: tx
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Show (DraftCommitTxResponse tx)

instance HasTextEnvelope tx => ToJSON (DraftCommitTxResponse tx) where
  toJSON (DraftCommitTxResponse tx) =
    toJSON $ serialiseToTextEnvelope (Just "Hydra commit transaction") tx

instance HasTextEnvelope tx => FromJSON (DraftCommitTxResponse tx) where
  parseJSON v = do
    env <- parseJSON v
    case deserialiseFromTextEnvelope (proxyToAsType Proxy) env of
      Left e -> fail $ show e
      Right tx -> pure $ DraftCommitTxResponse tx

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

deriving stock instance IsTx tx => Eq (DraftCommitTxRequest tx)
deriving stock instance IsTx tx => Show (DraftCommitTxRequest tx)

instance IsTx tx => ToJSON (DraftCommitTxRequest tx) where
  toJSON = \case
    FullCommitRequest{blueprintTx, utxo} ->
      object
        [ "blueprintTx" .= toJSON blueprintTx
        , "utxo" .= toJSON utxo
        ]
    SimpleCommitRequest{utxoToCommit} ->
      toJSON utxoToCommit

instance IsTx tx => FromJSON (DraftCommitTxRequest tx) where
  parseJSON v = fullVariant v <|> simpleVariant v
   where
    fullVariant = withObject "FullCommitRequest" $ \o -> do
      blueprintTx :: tx <- o .: "blueprintTx"
      utxo <- o .: "utxo"
      pure FullCommitRequest{blueprintTx, utxo}

    simpleVariant val = SimpleCommitRequest <$> parseJSON val

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

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
  Tracer IO APIServerLog ->
  Chain Tx IO ->
  PParams LedgerEra ->
  -- | A means to get the 'HeadId' if initializing the Head.
  IO (Maybe HeadId) ->
  -- | Get latest confirmed UTxO snapshot.
  IO (Maybe (UTxOType Tx)) ->
  Application
httpApp tracer directChain pparams getInitializingHeadId getConfirmedUTxO request respond = do
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
        >>= handleDraftCommitUtxo directChain getInitializingHeadId
        >>= respond
    ("GET", ["protocol-parameters"]) ->
      respond . responseLBS status200 [] . Aeson.encode $
        fromLedgerPParams shelleyBasedEra pparams
    ("POST", ["cardano-transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitUserTx directChain
        >>= respond
    _ ->
      respond $ responseLBS status400 [] "Resource not found"

-- * Handlers

-- | Handle request to obtain a draft commit tx.
handleDraftCommitUtxo ::
  Chain Tx IO ->
  -- | A means to get the 'HeadId' if initializing the Head.
  IO (Maybe HeadId) ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleDraftCommitUtxo directChain getInitializingHeadId body = do
  getInitializingHeadId >>= \case
    Just headId -> do
      case Aeson.eitherDecode' body of
        Left err ->
          pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
        Right FullCommitRequest{blueprintTx, utxo} -> do
          draftCommit headId utxo blueprintTx
        Right SimpleCommitRequest{utxoToCommit} -> do
          let blueprintTx = txSpendingUTxO utxoToCommit
          draftCommit headId utxoToCommit blueprintTx
    -- XXX: This is not really an internal server error
    Nothing -> pure $ responseLBS status500 [] (Aeson.encode (FailedToDraftTxNotInitializing :: PostTxError Tx))
 where
  draftCommit headId lookupUTxO blueprintTx =
    draftCommitTx headId CommitBlueprintTx{lookupUTxO, blueprintTx} <&> \case
      Left e ->
        -- Distinguish between errors users can actually benefit from and
        -- other errors that are turned into 500 responses.
        case e of
          CommittedTooMuchADAForMainnet _ _ -> badRequest e
          UnsupportedLegacyOutput _ -> badRequest e
          walletUtxoErr@SpendingNodeUtxoForbidden -> badRequest walletUtxoErr
          _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
      Right commitTx ->
        okJSON $ DraftCommitTxResponse commitTx
  Chain{draftCommitTx} = directChain

-- | Handle request to submit a cardano transaction.
handleSubmitUserTx ::
  Chain Tx IO ->
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

badRequest :: IsChainState tx => PostTxError tx -> Response
badRequest = responseLBS status400 [] . Aeson.encode . toJSON

notFound :: Response
notFound = responseLBS status404 [] ""

okJSON :: ToJSON a => a -> Response
okJSON = responseLBS status200 [] . Aeson.encode
