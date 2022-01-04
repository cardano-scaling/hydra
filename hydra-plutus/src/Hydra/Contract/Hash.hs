{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | An experimental validator which simply hashes a bytestring stored in the
-- datum using one of three supported algorithms.
module Hydra.Contract.Hash where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Prelude (Arbitrary (arbitrary), Generic, Show, genericArbitrary)

import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Builtins (equalsByteString)
import PlutusTx.IsData.Class (ToData (..))

data Hash

data HashAlgorithm
  = Baseline
  | SHA2
  | SHA3
  -- Blake2b
  deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''HashAlgorithm

instance Arbitrary HashAlgorithm where
  arbitrary = genericArbitrary

instance Scripts.ValidatorTypes Hash where
  type DatumType Hash = BuiltinByteString
  type RedeemerType Hash = HashAlgorithm

-- NOTE: Plutus is strict, thus this still occurs cost for hashing
validator :: DatumType Hash -> RedeemerType Hash -> ScriptContext -> Bool
validator bytes algorithm _ctx =
  case algorithm of
    Baseline -> not $ equalsByteString "" "1"
    SHA2 -> not . equalsByteString "" $ sha2_256 bytes
    SHA3 -> not . equalsByteString "" $ sha3_256 bytes

-- Blake2b -> not . equalsByteString "" $ blake2b_256 bytes

compiledValidator :: CompiledCode (ValidatorType Hash)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Hash
typedValidator = Scripts.mkTypedValidator @Hash
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Hash) @(RedeemerType Hash)
{- ORMOLU_ENABLE -}

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Hash -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType Hash -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash
