{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | An experimental validator which simply hashes a bytestring stored in the
-- datum using one of three supported algorithms.
module Hydra.Contract.Hash where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.Builtins (blake2b_256)
import PlutusTx.IsData.Class (ToData (..))

data Hash

data HashAlgorithm
  = SHA2
  | SHA3
  | Blake2b

PlutusTx.unstableMakeIsData ''HashAlgorithm

instance Scripts.ValidatorTypes Hash where
  type DatumType Hash = BuiltinByteString
  type RedeemerType Hash = HashAlgorithm

-- NOTE: Plutus is strict, thus this still occurs cost for hashing
validator :: DatumType Hash -> RedeemerType Hash -> ScriptContext -> Bool
validator bytes algorithm _ctx =
  case algorithm of
    SHA2 -> let _x = sha2_256 bytes in True
    SHA3 -> let _x = sha3_256 bytes in True
    Blake2b -> let _x = blake2b_256 bytes in True

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
