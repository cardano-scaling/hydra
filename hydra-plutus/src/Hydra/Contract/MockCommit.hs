{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A mock script to focus on the off-chain datum types without having to worry
-- about on-chain validation.
module Hydra.Contract.MockCommit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Commit

instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = ()
  type RedeemerType Commit = ()

validator :: () -> () -> ScriptContext -> Bool
validator _datum _redeemer _ctx = True

compiledValidator :: CompiledCode (ValidatorType Commit)
compiledValidator = $$(PlutusTx.compile [||validator||])

{- ORMOLU_DISABLE -}
typedValidator :: TypedValidator Commit
typedValidator = Scripts.mkTypedValidator @Commit
  compiledValidator
  $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.wrapValidator @(DatumType Commit) @(RedeemerType Commit)
{- ORMOLU_ENABLE -}

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Commit -> Datum
datum a = Datum (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash
