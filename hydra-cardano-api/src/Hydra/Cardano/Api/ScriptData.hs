module Hydra.Cardano.Api.ScriptData where

import Hydra.Cardano.Api.Prelude

import qualified Plutus.V1.Ledger.Api as Plutus

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Serialise some type into a generic 'ScriptData' structure.
toScriptData :: (ToScriptData a) => a -> ScriptData
toScriptData =
  fromPlutusData . Plutus.toData

{-# DEPRECATED mkRedeemerForTxIn "use 'asScriptData' instead." #-}
mkRedeemerForTxIn :: (ToScriptData a) => a -> ScriptRedeemer
mkRedeemerForTxIn =
  toScriptData
