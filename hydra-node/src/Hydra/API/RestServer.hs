{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Aeson (Value (Object), withObject, (.:?), object, KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short ()
import Data.Text (pack)
import Hydra.Cardano.Api (
  CtxUTxO,
  HashableScriptData,
  KeyWitnessInCtx (..),
  PlutusScript,
  ProtocolParameters,
  ScriptDatum (ScriptDatumForTxIn),
  ScriptWitnessInCtx (ScriptWitnessForSpending),
  Tx,
  TxOut,
  UTxO',
  deserialiseFromTextEnvelope,
  mkScriptWitness,
  proxyToAsType,
  serialiseToTextEnvelope,
  pattern KeyWitness,
  pattern ScriptWitness,
 )
import Hydra.Chain (Chain (..), PostTxError (..), draftCommitTx, IsChainState)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (PortNumber)
import Network.HTTP.Types (Method, status200, status400, status500)
import Network.Wai (Application, Request (pathInfo, requestMethod), Response, ResponseReceived, consumeRequestBodyStrict, responseLBS)
import Test.QuickCheck (oneof)

data APIServerLog
  = APIServerStarted {listeningPort :: PortNumber}
  | NewAPIConnection
  | APIOutputSent {sentOutput :: Aeson.Value}
  | APIInputReceived {receivedInput :: Aeson.Value}
  | APIInvalidInput {reason :: String, inputReceived :: Text}
  | APIConnectionError {reason :: String}
  | APIHandshakeError {reason :: String}
  | APIRestInputReceived
      { method :: Text
      , paths :: [Text]
      , requestInputBody :: Maybe Aeson.Value
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary APIServerLog where
  arbitrary =
    oneof
      [ APIServerStarted <$> arbitrary
      , pure NewAPIConnection
      , pure $ APIOutputSent (Aeson.Object mempty)
      , pure $ APIInputReceived (Aeson.Object mempty)
      , APIInvalidInput <$> arbitrary <*> arbitrary
      , APIConnectionError <$> arbitrary
      , APIHandshakeError <$> arbitrary
      , APIRestInputReceived
          <$> arbitrary
          <*> arbitrary
          <*> oneof [pure Nothing, pure $ Just (Aeson.Object mempty)]
      ]

newtype DraftCommitTxResponse = DraftCommitTxResponse
  { commitTx :: Tx
  }
  deriving (Show, Generic)

instance ToJSON DraftCommitTxResponse where
  toJSON (DraftCommitTxResponse tx) =
    toJSON $ serialiseToTextEnvelope (Just "Hydra commit transaction") tx

instance FromJSON DraftCommitTxResponse where
  parseJSON v = do
    env <- parseJSON v
    case deserialiseFromTextEnvelope (proxyToAsType Proxy) env of
      Left e -> fail $ show e
      Right tx -> pure $ DraftCommitTxResponse tx

instance Arbitrary DraftCommitTxResponse where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxResponse xs -> DraftCommitTxResponse <$> shrink xs

-- TODO: This should actually be isomorphic to ScriptWitness of cardano-api,
-- i.e. we should support also native scripts, other versions of plutus and
-- witnessing via reference inputs
data ScriptInfo = ScriptInfo
  { redeemer :: HashableScriptData
  , datum :: HashableScriptData
  , plutusV2Script :: PlutusScript
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ScriptInfo where
  arbitrary = genericArbitrary

data TxOutWithWitness = TxOutWithWitness
  { txOut :: TxOut CtxUTxO
  , witness :: Maybe ScriptInfo
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TxOutWithWitness where
  toJSON TxOutWithWitness{txOut, witness} =
    case toJSON txOut of
      Object km
        | isJust witness ->
            Object $ km & "witness" `KeyMap.insert` toJSON witness
      x -> x

instance FromJSON TxOutWithWitness where
  parseJSON v = do
    txOut <- parseJSON v
    flip (withObject "TxOutWithWitness") v $ \o -> do
      witness <- o .:? "witness"
      pure $ TxOutWithWitness{txOut, witness}

instance Arbitrary TxOutWithWitness where
  arbitrary = genericArbitrary

deriving newtype instance Arbitrary (UTxO' TxOutWithWitness)

newtype DraftCommitTxRequest = DraftCommitTxRequest
  { utxoToCommit :: UTxO' TxOutWithWitness
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

instance Arbitrary DraftCommitTxRequest where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest u -> DraftCommitTxRequest <$> shrink u

newtype SubmitTxRequest = SubmitTxRequest
  { txToSubmit :: Tx
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

instance Arbitrary SubmitTxRequest where
  arbitrary = genericArbitrary

  shrink = \case
    SubmitTxRequest u -> SubmitTxRequest <$> shrink u

data TransactionSubmitted = TransactionSubmitted
  deriving stock (Eq, Show, Generic)

instance ToJSON TransactionSubmitted where
  toJSON _ = object
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
  Chain tx IO ->
  ProtocolParameters ->
  Application
httpApp tracer directChain pparams req respond =
  case (requestMethod req, pathInfo req) of
    ("POST", ["commit"]) -> do
      body <- consumeRequestBodyStrict req
      handleDraftCommitUtxo directChain tracer body (requestMethod req) (pathInfo req) respond
    ("GET", ["protocol-parameters"]) ->
      respond $ responseLBS status200 [] (Aeson.encode pparams)
    ("POST", ["cardano-transaction"]) -> do
      body <- consumeRequestBodyStrict req
      handleSubmitUserTx directChain tracer body (requestMethod req) (pathInfo req) respond
    _ -> do
      traceWith tracer $
        APIRestInputReceived
          { method = decodeUtf8 $ requestMethod req
          , paths = pathInfo req
          , requestInputBody = Nothing
          }
      respond $ responseLBS status400 [] "Resource not found"

-- * Handlers

-- Handle user requests to obtain a draft commit tx
handleDraftCommitUtxo ::
  Chain tx IO ->
  Tracer IO APIServerLog ->
  LBS.ByteString ->
  Method ->
  [Text] ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
handleDraftCommitUtxo directChain tracer body reqMethod reqPaths respond = do
  case Aeson.eitherDecode' body :: Either String DraftCommitTxRequest of
    Left err ->
      respond $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right requestInput@DraftCommitTxRequest{utxoToCommit} -> do
      traceWith tracer $
        APIRestInputReceived
          { method = decodeUtf8 reqMethod
          , paths = reqPaths
          , requestInputBody = Just $ toJSON requestInput
          }
      eCommitTx <- draftCommitTx $ fromTxOutWithWitness <$> utxoToCommit
      respond $
        case eCommitTx of
          Left e ->
            -- Distinguish between errors users can actually benefit from and
            -- other errors that are turned into 500 responses.
            case e of
              CannotCommitReferenceScript -> return400 e
              CommittedTooMuchADAForMainnet _ _ -> return400 e
              UnsupportedLegacyOutput _ -> return400 e
              walletUtxoErr@SpendingNodeUtxoForbidden -> return400 walletUtxoErr
              _ -> responseLBS status500 [] (Aeson.encode $ toJSON e)
          Right commitTx ->
            responseLBS status200 [] (Aeson.encode $ DraftCommitTxResponse commitTx)
 where

  Chain{draftCommitTx} = directChain

  fromTxOutWithWitness TxOutWithWitness{txOut, witness} =
    (txOut, toScriptWitness witness)
   where
    toScriptWitness = \case
      Nothing ->
        KeyWitness KeyWitnessForSpending
      Just ScriptInfo{redeemer, datum, plutusV2Script} ->
        ScriptWitness ScriptWitnessForSpending $
          mkScriptWitness plutusV2Script (ScriptDatumForTxIn datum) redeemer

-- | Handle user requests to submit a signed tx
handleSubmitUserTx ::
  Chain tx IO ->
  Tracer IO APIServerLog ->
  LBS.ByteString ->
  Method ->
  [Text] ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
handleSubmitUserTx directChain tracer body reqMethod reqPaths respond = do
  case Aeson.eitherDecode' body :: Either String SubmitTxRequest of
    Left err ->
      respond $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right requestInput@SubmitTxRequest{txToSubmit} -> do
      traceWith tracer $
        APIRestInputReceived
          { method = decodeUtf8 reqMethod
          , paths = reqPaths
          , requestInputBody = Just $ toJSON requestInput
          }

      eresult <- try $ submitUserTx txToSubmit
      respond $
        case eresult of
          Left (e :: PostTxError Tx) -> return400 e
          Right _ ->
            responseLBS status200 [] (Aeson.encode TransactionSubmitted)
 where
  Chain{submitUserTx} = directChain

return400 :: IsChainState tx => PostTxError tx -> Response
return400 = responseLBS status400 [] . Aeson.encode . toJSON
