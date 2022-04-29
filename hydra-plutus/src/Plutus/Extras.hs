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

-- ** Typed validator interface

-- TODO: maybe not needed!?

-- | A class that associates a type standing for a connection type with two types, the type of the redeemer
-- and the data script for that connection type.
class ValidatorTypes (a :: Type) where
  -- | The type of the redeemers of this connection type.
  type RedeemerType a :: Type

  -- | The type of the data of this connection type.
  type DatumType a :: Type

  -- Defaults
  type RedeemerType a = ()
  type DatumType a = ()

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> ScriptContext -> Bool

instance ValidatorTypes Void where
  type RedeemerType Void = Void
  type DatumType Void = Void

instance ValidatorTypes Any where
  type RedeemerType Any = BuiltinData
  type DatumType Any = BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) = TypedValidator
  { tvValidator :: Scripts.Validator
  , tvValidatorHash :: Scripts.ValidatorHash
  }
  deriving stock (Show, Eq, Generic)

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
  -- | Validator script (compiled)
  CompiledCode (ValidatorType a) ->
  -- | A wrapper for the compiled validator
  CompiledCode (ValidatorType a -> WrappedValidatorType) ->
  TypedValidator a
mkTypedValidator vc wrapper =
  let val = Scripts.mkValidatorScript $ wrapper `applyCode` vc
      hsh = undefined --Scripts.validatorHash val
   in TypedValidator
        { tvValidator = val
        , tvValidatorHash = hsh
        }
