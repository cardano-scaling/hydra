{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Contract for Hydra controlling the redemption of commits from participants.
module Hydra.Contract.Commit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx

data Commit

instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = TxOut
  type RedeemerType Commit = ()

validator ::
  TxOut ->
  () ->
  ScriptContext ->
  Bool
validator _commit () _ctx =
  True

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

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator
