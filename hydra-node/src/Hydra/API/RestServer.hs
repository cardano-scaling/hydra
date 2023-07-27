{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Aeson (Value (Object, String), withObject, withText, (.:?))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString.Short ()
import Hydra.Cardano.Api (
  CtxUTxO,
  HashableScriptData,
  KeyWitnessInCtx (..),
  PlutusScript,
  ScriptDatum (ScriptDatumForTxIn),
  ScriptWitnessInCtx (ScriptWitnessForSpending),
  Tx,
  TxOut,
  UTxO',
  WitCtxTxIn,
  Witness,
  deserialiseFromTextEnvelope,
  mkScriptWitness,
  proxyToAsType,
  serialiseToTextEnvelope,
  pattern KeyWitness,
  pattern ScriptWitness,
 )
import Hydra.Ledger.Cardano ()
import Hydra.Network (PortNumber)
import qualified Data.Aeson as Aeson
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

data SubmittedTxResponse = SubmittedTxResponse
  deriving stock (Eq, Show, Generic)

submittedTxResponseConst :: Text
submittedTxResponseConst = "Transaction Submitted"

instance ToJSON SubmittedTxResponse where
  toJSON _ = String submittedTxResponseConst

instance FromJSON SubmittedTxResponse where
  parseJSON = withText "SubmittedTxResponse" $ \t ->
    if t == submittedTxResponseConst
      then pure SubmittedTxResponse
      else fail $ "Failed to parse: " <> show t <> " expected: " <> show submittedTxResponseConst

instance Arbitrary SubmittedTxResponse where
  arbitrary = genericArbitrary

fromTxOutWithWitness :: TxOutWithWitness -> (TxOut CtxUTxO, Witness WitCtxTxIn)
fromTxOutWithWitness TxOutWithWitness{txOut, witness} =
  (txOut, toScriptWitness witness)
 where
  toScriptWitness = \case
    Nothing ->
      KeyWitness KeyWitnessForSpending
    Just ScriptInfo{redeemer, datum, plutusV2Script} ->
      ScriptWitness ScriptWitnessForSpending $
        mkScriptWitness plutusV2Script (ScriptDatumForTxIn datum) redeemer
