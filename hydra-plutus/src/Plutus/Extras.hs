{-# LANGUAGE DerivingStrategies #-}

module Plutus.Extras where

import Hydra.Prelude

import Plutus.V1.Ledger.Api (ScriptContext)
import PlutusTx (BuiltinData, UnsafeFromData (..))
import PlutusTx.Prelude (check)

-- * Vendored from plutus-ledger

-- | Wrap a typed validator to get the basic `WrappedValidatorType` signature
-- which can be passed to `PlutusTx.compile`.
-- REVIEW: There might be better ways to name this than "wrap"
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapValidator f d r p = check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapValidator #-}

type WrappedValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()

-- | Wrap a typed minting policy to get the basic `WrappedMintintPolicyType`
-- signature which can be passed to `PlutusTx.compile`.
wrapMintingPolicy ::
  UnsafeFromData redeemer =>
  (redeemer -> ScriptContext -> Bool) ->
  WrappedMintingPolicyType
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapMintingPolicy f r p = check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapMintingPolicy #-}

type WrappedMintingPolicyType = BuiltinData -> BuiltinData -> ()
