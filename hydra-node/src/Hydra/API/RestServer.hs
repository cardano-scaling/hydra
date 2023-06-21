{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (Object, String), object, withObject, (.:), (.=))
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
  TxIn,
  TxOut,
  UTxO',
  WitCtxTxIn,
  Witness,
  mkScriptWitness,
  pattern ScriptWitness,
 )
import Hydra.Cardano.Api.Prelude (Era, ScriptWitness)
import Hydra.Ledger (IsTx)
import Hydra.Ledger.Cardano ()

newtype DraftCommitTxResponse tx = DraftCommitTxResponse
  { commitTx :: tx
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (DraftCommitTxResponse tx)
deriving stock instance IsTx tx => Show (DraftCommitTxResponse tx)

instance (IsTx tx, ToCBOR tx) => ToJSON (DraftCommitTxResponse tx) where
  toJSON (DraftCommitTxResponse tx) =
    object
      [ "commitTx" .= (String . decodeUtf8 . Base16.encode $ serialize' tx)
      ]

instance
  (IsTx tx, FromCBOR tx) =>
  FromJSON (DraftCommitTxResponse tx)
  where
  parseJSON = Aeson.withObject "DraftCommitTxResponse" $ \o -> do
    encodedTx :: Text <- o .: "commitTx"
    case Base16.decode $ encodeUtf8 encodedTx of
      Left e -> fail e
      Right commitTx ->
        case decodeFull' commitTx of
          Left err -> fail $ show err
          Right v -> pure $ DraftCommitTxResponse v

instance IsTx tx => Arbitrary (DraftCommitTxResponse tx) where
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

data DraftUTxO = DraftUTxO
  { txOut :: TxOut CtxUTxO
  , witness :: Maybe ScriptInfo
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON DraftUTxO where
  toJSON DraftUTxO{txOut, witness} =
    case toJSON txOut of
      Object km ->
        Object $ km & "witness" `KeyMap.insert` toJSON witness
      x -> x

instance FromJSON DraftUTxO where
  parseJSON v = do
    txOut <- parseJSON v
    flip (withObject "DraftUTxO") v $ \o -> do
      witness <- o .: "witness"
      pure $ DraftUTxO{txOut, witness}

instance Arbitrary DraftUTxO where
  arbitrary = genericArbitrary

deriving newtype instance Arbitrary (UTxO' DraftUTxO)

newtype DraftCommitTxRequest = DraftCommitTxRequest
  { utxos :: UTxO' DraftUTxO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary DraftCommitTxRequest where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest u -> DraftCommitTxRequest <$> shrink u

convertDraftUTxO ::
  UTxO' DraftUTxO ->
  UTxO' (TxOut CtxUTxO, Maybe (ScriptWitness WitCtxTxIn Era))
convertDraftUTxO utxo' =
  (\DraftUTxO{txOut, witness} -> (txOut, toScriptWitness <$> witness)) <$> utxo'
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
