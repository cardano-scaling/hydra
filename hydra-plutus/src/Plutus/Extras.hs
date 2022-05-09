{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Plutus.Extras where

import Hydra.Prelude

import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Plutus.V1.Ledger.Api (Script, ValidatorHash (ValidatorHash))
import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (check, toBuiltin)

-- * Vendored from plutus-ledger

-- | Signature of an untyped validator script.
type ValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Wrap a typed validator to get the basic `ValidatorType` signature which can
-- be passed to `PlutusTx.compile`.
-- REVIEW: There might be better ways to name this than "wrap"
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer, UnsafeFromData context) =>
  (datum -> redeemer -> context -> Bool) ->
  ValidatorType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapValidator f d r p = check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> ()

-- | Wrap a typed minting policy to get the basic `MintintPolicyType` signature
-- which can be passed to `PlutusTx.compile`.
wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapMintingPolicy f r p = check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapMintingPolicy #-}

-- * Similar utilities as plutus-ledger

-- | Compute the 'ValidatorHash' for a given plutus 'Script'.
--
-- NOTE: Implemented using hydra-cardano-api (PlutusScript pattern)
scriptValidatorHash :: Script -> ValidatorHash
scriptValidatorHash =
  ValidatorHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . PlutusScript
    . fromPlutusScript
