{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.API.HTTPServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Aeson (KeyValue ((.=)), Value (Object), object, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Short ()
import Data.Text (pack)
import Hydra.API.APIServerLog (APIServerLog (..), Method (..), PathInfo (..))
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
import Hydra.Chain (Chain (..), IsChainState, PostTxError (..), draftCommitTx)
import Hydra.Chain.Direct.State ()
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)
import Network.HTTP.Types (status200, status400, status500)
import Network.Wai (
  Application,
  Request (pathInfo, requestMethod),
  Response,
  consumeRequestBodyStrict,
  rawPathInfo,
  responseLBS,
 )

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
  Chain tx IO ->
  ProtocolParameters ->
  Application
httpApp tracer directChain pparams request respond = do
  traceWith tracer $
    APIHTTPRequestReceived
      { method = Method $ requestMethod request
      , path = PathInfo $ rawPathInfo request
      }
  case (requestMethod request, pathInfo request) of
    ("POST", ["commit"]) ->
      consumeRequestBodyStrict request
        >>= handleDraftCommitUtxo directChain
        >>= respond
    ("GET", ["protocol-parameters"]) ->
      respond $ responseLBS status200 [] (Aeson.encode pparams)
    ("POST", ["cardano-transaction"]) ->
      consumeRequestBodyStrict request
        >>= handleSubmitUserTx directChain
        >>= respond
    _ ->
      respond $ responseLBS status400 [] "Resource not found"

-- * Handlers

-- | Handle request to obtain a draft commit tx.
handleDraftCommitUtxo ::
  Chain tx IO ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleDraftCommitUtxo directChain body = do
  case Aeson.eitherDecode' body :: Either String DraftCommitTxRequest of
    Left err ->
      pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right requestInput@DraftCommitTxRequest{utxoToCommit} -> do
      draftCommitTx (fromTxOutWithWitness <$> utxoToCommit) <&> \case
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

-- | Handle request to submit a cardano transaction.
handleSubmitUserTx ::
  Chain tx IO ->
  -- | Request body.
  LBS.ByteString ->
  IO Response
handleSubmitUserTx directChain body = do
  case Aeson.eitherDecode' body :: Either String SubmitTxRequest of
    Left err ->
      pure $ responseLBS status400 [] (Aeson.encode $ Aeson.String $ pack err)
    Right requestInput@SubmitTxRequest{txToSubmit} -> do
      try (submitUserTx txToSubmit) <&> \case
        Left (e :: PostTxError Tx) -> return400 e
        Right _ ->
          responseLBS status200 [] (Aeson.encode TransactionSubmitted)
 where
  Chain{submitUserTx} = directChain

return400 :: IsChainState tx => PostTxError tx -> Response
return400 = responseLBS status400 [] . Aeson.encode . toJSON
