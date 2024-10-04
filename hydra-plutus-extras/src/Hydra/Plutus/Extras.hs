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
import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (BuiltinUnit, check, toBuiltin)

-- * Vendored from plutus-ledger

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> BuiltinUnit

-- | Wrap a typed validator to get the basic `ValidatorType` signature which can
-- be passed to `PlutusTx.compile`.
mkUntypedValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
mkUntypedValidator f d r c =
  check $ f datum redeemer context
 where
  datum = unsafeFromBuiltinData d
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE mkUntypedValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> BuiltinUnit

-- | Wrap a typed minting policy to get the basic `MintingPolicyType` signature
-- which can be passed to `PlutusTx.compile`.
mkUntypedMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
mkUntypedMintingPolicy f r c =
  check $ f redeemer context
 where
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE mkUntypedMintingPolicy #-}

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
