{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Data.Aeson (Value (Object, String), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short ()
import Hydra.Cardano.Api (
  BuildTx,
  BuildTxWith (..),
  CtxUTxO,
  HashableScriptData,
  PlutusScript,
  ScriptDatum (ScriptDatumForTxIn),
  ScriptWitnessInCtx (ScriptWitnessForSpending),
  SerialiseAsCBOR (deserialiseFromCBOR, serialiseToCBOR),
  Tx,
  TxIn,
  TxOut,
  UTxO',
  WitCtxTxIn,
  Witness,
  mkScriptWitness,
  proxyToAsType,
  pattern ScriptWitness,
 )
import Hydra.Cardano.Api.Prelude (Era, ScriptWitness)
import Hydra.Ledger.Cardano ()

newtype DraftCommitTxResponse = DraftCommitTxResponse
  { commitTx :: Tx
  }
  deriving (Show, Generic)

instance ToJSON DraftCommitTxResponse where
  toJSON (DraftCommitTxResponse tx) =
    object
      [ "commitTx" .= (String . decodeUtf8 . Base16.encode $ serialiseToCBOR tx)
      ]

instance FromJSON DraftCommitTxResponse where
  parseJSON = Aeson.withObject "DraftCommitTxResponse" $ \o -> do
    encodedTx :: Text <- o .: "commitTx"
    case Base16.decode $ encodeUtf8 encodedTx of
      Left e -> fail e
      Right bytes ->
        case deserialiseFromCBOR (proxyToAsType Proxy) bytes of
          Left err -> fail $ show err
          Right v -> pure $ DraftCommitTxResponse v

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
  { utxos :: UTxO' TxOutWithWitness
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary DraftCommitTxRequest where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest u -> DraftCommitTxRequest <$> shrink u

convertDraftUTxO ::
  UTxO' TxOutWithWitness ->
  UTxO' (TxOut CtxUTxO, Maybe (ScriptWitness WitCtxTxIn Era))
convertDraftUTxO utxo' =
  (\TxOutWithWitness{txOut, witness} -> (txOut, toScriptWitness <$> witness)) <$> utxo'
 where
  toScriptWitness ScriptInfo{redeemer, datum, plutusV2Script} =
    mkScriptWitness plutusV2Script (ScriptDatumForTxIn datum) redeemer

prepareCommitTxInputs ::
  UTxO' (TxOut CtxUTxO, Maybe (ScriptWitness WitCtxTxIn Era)) ->
  (UTxO.UTxO, UTxO.UTxO, [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn))])
prepareCommitTxInputs =
  foldl'
    ( \(regularUtxo, scriptUtxo, witnesses) (txIn, (txOut, maybeWitness)) ->
        case maybeWitness of
          Just w ->
            ( regularUtxo
            , scriptUtxo <> UTxO.singleton (txIn, txOut)
            , witnesses <> [(txIn, BuildTxWith . ScriptWitness ScriptWitnessForSpending $ w)]
            )
          Nothing ->
            ( regularUtxo <> UTxO.singleton (txIn, txOut)
            , scriptUtxo
            , witnesses
            )
    )
    (mempty, mempty, [])
    . UTxO.pairs
