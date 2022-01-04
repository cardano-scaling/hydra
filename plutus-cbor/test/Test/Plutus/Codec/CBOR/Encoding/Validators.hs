{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Test.Plutus.Codec.CBOR.Encoding.Validators where

import Hydra.Prelude hiding (label)

import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Codec.CBOR.Encoding (
  encodeInteger,
 )
import qualified PlutusTx as Plutus

-- | A baseline validator which does nothing but returning 'True'. We use it as
-- baseline to measure the deviation for cost execution of other validators.
data EmptyValidator

instance Scripts.ValidatorTypes EmptyValidator where
  type DatumType EmptyValidator = ()
  type RedeemerType EmptyValidator = ()

emptyValidator :: Scripts.TypedValidator EmptyValidator
emptyValidator =
  Scripts.mkTypedValidator @EmptyValidator
    $$(Plutus.compile [||\() () _ctx -> True||])
    $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @()

-- | A validator for measuring cost of encoding values. The validator is
-- parameterized by the type of value.
data EncodeValidator a

instance Scripts.ValidatorTypes (EncodeValidator a) where
  type DatumType (EncodeValidator a) = ()
  type RedeemerType (EncodeValidator a) = [a]

encodeIntegerValidator :: Scripts.TypedValidator (EncodeValidator Integer)
encodeIntegerValidator =
  Scripts.mkTypedValidator @(EncodeValidator Integer)
    $$(Plutus.compile [||\() xs _ctx -> let _bytes = encodeInteger <$> xs in True||])
    $$(Plutus.compile [||wrap||])
 where
  wrap = Scripts.wrapValidator @() @[Integer]
