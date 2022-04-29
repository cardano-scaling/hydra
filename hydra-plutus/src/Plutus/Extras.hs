{-# LANGUAGE DerivingStrategies #-}

module Plutus.Extras where

import Hydra.Prelude

import Plutus.V1.Ledger.Api (ScriptContext)
import qualified Plutus.V1.Ledger.Scripts as Scripts
import PlutusTx (BuiltinData, CompiledCode, UnsafeFromData (..), applyCode)
import PlutusTx.Prelude (check)

-- * Vendored from plutus-ledger

-- | Wrap a typed validator to get the basic `WrappedValidatorType` signature
-- which can be passed to `Plutus.compile`. Vendored from `plutus-ledger`.
-- REVIEW: There might be better ways to name this than "wrap"
wrapValidator ::
  (UnsafeFromData datum, UnsafeFromData redeemer) =>
  (datum -> redeemer -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
wrapValidator f d r p = check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)
{-# INLINEABLE wrapValidator #-}

type WrappedValidatorType = BuiltinData -> BuiltinData -> BuiltinData -> ()
