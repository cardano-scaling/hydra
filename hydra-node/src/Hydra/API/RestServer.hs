{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Aeson (Value (Object), withObject, (.:?), (.:))
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
import Hydra.Ledger (IsTx)

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

newtype SubmitTxRequest tx = SubmitTxRequest
  { txToSubmit :: tx
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

instance IsTx tx => Arbitrary (SubmitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    SubmitTxRequest u -> SubmitTxRequest <$> shrink u

newtype SubmitTxResponse = SubmitTxResponse
  { submitTxResponse :: Text
  }
  deriving (Show, Generic)

instance ToJSON SubmitTxResponse where
  toJSON (SubmitTxResponse r) = toJSON r

instance FromJSON SubmitTxResponse where
  parseJSON = withObject "SubmitTxResponse" $ \o -> do
     r <- o .: "submitTxResponse"
     pure $ SubmitTxResponse r

instance Arbitrary SubmitTxResponse where
  arbitrary = genericArbitrary

  shrink = \case
    SubmitTxResponse xs -> SubmitTxResponse <$> shrink xs

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
