module Hydra.Cardano.Api.ScriptDatum where

import Hydra.Cardano.Api.Prelude

import Hydra.Cardano.Api.ScriptData (ToScriptData, toScriptData)

import qualified Cardano.Ledger.Alonzo.Data as Ledger

-- * Extras

-- | Construct a 'ScriptDatum' for use as transaction witness.
mkScriptDatum :: (ToScriptData a) => a -> ScriptDatum WitCtxTxIn
mkScriptDatum =
  ScriptDatumForTxIn . toScriptData

{-# DEPRECATED mkDatumForTxIn "use 'mkScriptDatum' instead." #-}
mkDatumForTxIn :: (ToScriptData a) => a -> ScriptDatum WitCtxTxIn
mkDatumForTxIn =
  mkScriptDatum

-- * Type Conversions

-- | Convert a cardano-ledger's script 'Data' into a cardano-api's 'ScriptDatum'.
fromLedgerData :: Ledger.Data LedgerEra -> ScriptDatum WitCtxTxIn
fromLedgerData =
  ScriptDatumForTxIn . fromAlonzoData
