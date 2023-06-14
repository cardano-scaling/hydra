{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.RestServer where

import Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Binary (decodeFull', serialize')
import Data.Aeson (Value (String), object, withObject, (.:), (.=))
import qualified Data.ByteString.Base16 as Base16
import Data.ByteString.Short ()
import Hydra.Cardano.Api (
  CtxUTxO,
  HashableScriptData,
  PlutusScript,
  TxIn,
  TxOut,
  WitCtxTxIn,
  mkScriptWitness,
 )
import Hydra.Cardano.Api.Prelude (Era, ScriptDatum (ScriptDatumForTxIn), ScriptWitness)
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
  parseJSON = withObject "DraftCommitTxResponse" $ \o -> do
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
  { draftTxIn :: TxIn
  , draftTxOut :: TxOut CtxUTxO
  , draftScriptInfo :: Maybe ScriptInfo
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary DraftUTxO where
  arbitrary = genericArbitrary

newtype DraftCommitTxRequest = DraftCommitTxRequest
  { utxos :: [DraftUTxO]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary DraftCommitTxRequest where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxRequest u -> DraftCommitTxRequest <$> shrink u

convertDraftUTxO ::
  DraftUTxO ->
  ( TxIn
  , TxOut CtxUTxO
  , Maybe (ScriptWitness WitCtxTxIn Era)
  )
convertDraftUTxO (DraftUTxO txin txout maybeScriptInfo) =
  case maybeScriptInfo of
    Just ScriptInfo{redeemer, datum, plutusV2Script} ->
      (txin, txout, Just (mkScriptWitness plutusV2Script (ScriptDatumForTxIn datum) redeemer))
    Nothing ->
      (txin, txout, Nothing)

prepareCommitTxInputs :: [(TxIn, TxOut CtxUTxO, Maybe (ScriptWitness WitCtxTxIn Era))] -> (UTxO.UTxO, UTxO.UTxO, [(TxIn, ScriptWitness WitCtxTxIn Era)])
prepareCommitTxInputs =
  foldl'
    ( \(regularUtxo, scriptUtxo, witnesses) (txin, txout, m) ->
        case m of
          Just w ->
            ( regularUtxo
            , scriptUtxo <> UTxO.singleton (txin, txout)
            , witnesses <> [(txin, w)]
            )
          Nothing -> (regularUtxo <> UTxO.singleton (txin, txout), scriptUtxo, witnesses)
    )
    (mempty, mempty, [])
