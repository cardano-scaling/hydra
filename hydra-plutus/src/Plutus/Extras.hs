{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}

module Plutus.Extras where

import Hydra.Prelude hiding (fromMaybe)

import Hydra.Cardano.Api (
  SerialiseAsRawBytes (serialiseToRawBytes),
  fromPlutusScript,
  hashScript,
  pattern PlutusScript,
 )
import Plutus.V1.Ledger.Api (Script, UnsafeFromData (unsafeFromBuiltinData), ValidatorHash (ValidatorHash))
import PlutusTx (BuiltinData)
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
wrapValidator f d r c =
  check $ f datum redeemer context
 where
  datum = unsafeFromBuiltinData d
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
{-# INLINEABLE wrapValidator #-}

-- | Signature of an untyped minting policy script.
type MintingPolicyType = BuiltinData -> BuiltinData -> ()

-- | Wrap a typed minting policy to get the basic `MintingPolicyType` signature
-- which can be passed to `PlutusTx.compile`.
wrapMintingPolicy ::
  (UnsafeFromData redeemer, UnsafeFromData context) =>
  (redeemer -> context -> Bool) ->
  MintingPolicyType
wrapMintingPolicy f r c =
  check $ f redeemer context
 where
  redeemer = unsafeFromBuiltinData r
  context = unsafeFromBuiltinData c
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
