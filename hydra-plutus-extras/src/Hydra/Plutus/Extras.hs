{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Plutus.Extras (
  module Hydra.Plutus.Extras,
  module Hydra.Plutus.Extras.Time,
) where

import Hydra.Prelude

import Hydra.Plutus.Extras.Time

import Cardano.Api (
  PlutusScriptVersion,
  SerialiseAsRawBytes (serialiseToRawBytes),
  hashScript,
  pattern PlutusScript,
 )
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.V2 (ScriptHash (..))
import PlutusTx (BuiltinData)
import PlutusTx.Prelude (BuiltinUnit, toBuiltin)

-- * Vendored from plutus-ledger

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> BuiltinUnit

-- * Similar utilities as plutus-ledger

-- | Compute the on-chain 'ScriptHash' for a given serialised plutus script. Use
-- this to refer to another validator script.
scriptValidatorHash :: PlutusScriptVersion lang -> SerialisedScript -> ScriptHash
scriptValidatorHash version =
  ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript version
    . PlutusScriptSerialised
