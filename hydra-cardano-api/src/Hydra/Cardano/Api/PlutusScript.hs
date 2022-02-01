module Hydra.Cardano.Api.PlutusScript where

import Hydra.Cardano.Api.Prelude

import Codec.Serialise (serialise)
import qualified Plutus.V1.Ledger.Api as Plutus

-- * Type Conversions

-- | Convert a plutus' 'Script' into a cardano-api's 'PlutusScript'
fromPlutusScript :: Plutus.Script -> PlutusScript lang
fromPlutusScript =
  PlutusScriptSerialised . toShort . fromLazy . serialise
