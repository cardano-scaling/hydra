module Hydra.Cardano.Api.ScriptData where

import Cardano.Api
import Hydra.Prelude

import Cardano.Api.Shelley (fromPlutusData)
import qualified Plutus.V1.Ledger.Api as Plutus

-- | Data-types that can be marshalled into a generic 'ScriptData' structure.
type ToScriptData a = Plutus.ToData a

-- | Data-types that can be unmarshalled from a generic 'ScriptData' structure.
type FromScriptData a = Plutus.FromData a

-- | Serialise some type into a generic 'ScriptData' structure.
asScriptData :: (ToScriptData a) => a -> ScriptData
asScriptData =
  fromPlutusData . Plutus.toData

{-# DEPRECATED mkRedeemerForTxIn "use 'asScriptData' instead." #-}
mkRedeemerForTxIn :: (ToScriptData a) => a -> ScriptRedeemer
mkRedeemerForTxIn =
  asScriptData
