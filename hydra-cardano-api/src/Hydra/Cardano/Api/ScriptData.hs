{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ScriptData where

import "hydra-cardano-api" Hydra.Cardano.Api.Prelude hiding (left)

import "cardano-ledger-core" Cardano.Ledger.Core qualified as Ledger
import "cardano-ledger-core" Cardano.Ledger.Plutus.Data qualified as Ledger
import "plutus-ledger-api" PlutusLedgerApi.V3 qualified as Plutus

-- * Extras

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Serialise some type into a generic script data.
toScriptData :: ToScriptData a => a -> HashableScriptData
toScriptData =
  -- NOTE: Safe to use here as the data was not available in serialized form.
  unsafeHashableScriptData . fromPlutusData . Plutus.toData

-- | Deserialise some generic script data into some type.
fromScriptData :: FromScriptData a => HashableScriptData -> Maybe a
fromScriptData =
  Plutus.fromData . toPlutusData . getScriptData

-- | Get the 'HashableScriptData' associated to the a 'TxOut'. Note that this
-- requires the 'CtxTx' context.
txOutScriptData :: TxOut CtxTx era -> Maybe HashableScriptData
txOutScriptData (TxOut _ _ d _) =
  case d of
    TxOutDatumInline _ sd -> Just sd
    TxOutSupplementalDatum _ sd -> Just sd
    _ -> Nothing

-- * Type Conversions

-- | Convert a cardano-ledger script 'Data' into a cardano-api 'ScriptDatum'.
fromLedgerData :: Ledger.Data era -> HashableScriptData
fromLedgerData =
  fromAlonzoData

-- | Convert a cardano-api script data into a cardano-ledger script 'Data'.
-- XXX: This is a partial function. Ideally it would fall back to the
-- 'Plutus.Data' portion in 'HashableScriptData'.
toLedgerData :: Ledger.Era era => HashableScriptData -> Ledger.Data era
toLedgerData =
  toAlonzoData
