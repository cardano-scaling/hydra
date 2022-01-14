{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The validator used to collect & open or abort a Head.
module Hydra.Contract.Commit where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Data.Party (Party)
import Hydra.Data.Utxo (Utxo)
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Commit

data CommitRedeemer
  = Abort
  | Collect

PlutusTx.unstableMakeIsData ''CommitRedeemer

instance Scripts.ValidatorTypes Commit where
  type DatumType Commit = (Party, Utxo)
  type RedeemerType Commit = Redeemer

validator :: DatumType Commit -> RedeemerType Commit -> ScriptContext -> Bool
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

address :: Address
address = scriptHashAddress $ Scripts.validatorHash typedValidator

datum :: DatumType Commit -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: CommitRedeemer -> RedeemerType Commit
redeemer a = Redeemer (toBuiltinData a)
