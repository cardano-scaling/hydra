{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Data.Party (Party)
import Hydra.Data.Utxo (Utxo)
import qualified Ledger.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Commit

data CommitRedeemer
  = Abort
  | Collect

PlutusTx.unstableMakeIsData ''CommitRedeemer

validatorLogic :: BuiltinData -> BuiltinData -> BuiltinData -> ()
validatorLogic _datum _redeemer _ctx = ()

compiledValidator :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledValidator = $$(PlutusTx.compile [||validatorLogic||])

{- ORMOLU_DISABLE -}
validator :: Validator
validator = Scripts.mkValidatorScript compiledValidator
{- ORMOLU_ENABLE -}

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript validator

address :: Address
address = scriptHashAddress $ Scripts.validatorHash validator

datum :: (Party, Utxo) -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: CommitRedeemer -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
