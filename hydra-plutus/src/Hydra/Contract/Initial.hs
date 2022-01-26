{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The Initial validator which allows participants to commit or abort. To
-- focus on the off-chain datum types this is currently 'const True'.
module Hydra.Contract.Initial where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import qualified Hydra.Contract.Commit as Commit
import Ledger.Typed.Scripts (TypedValidator, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Ada (fromValue, getLovelace, lovelaceValueOf)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..))

data Initial

data InitialRedeemer
  = Abort
  | Commit

PlutusTx.unstableMakeIsData ''InitialRedeemer

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = InitialRedeemer

validator ::
  -- | Commit validator
  ValidatorHash ->
  DatumType Initial ->
  RedeemerType Initial ->
  ScriptContext ->
  Bool
validator commitValidator _datum red context@ScriptContext{scriptContextTxInfo = txInfo} =
  case red of
    Abort -> True
    Commit -> checkOutputValue
 where
  checkOutputValue =
    traceIfFalse "commitLockedValue does not match" $
      traceIfFalse ("commitLockedValue: " `appendString` debugValue commitLockedValue) $
        traceIfFalse ("initialValue: " `appendString` debugValue initialValue) $
          traceIfFalse ("comittedValue: " `appendString` debugValue committedValue) $
            commitLockedValue == initialValue + committedValue

  initialValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput context

  committedValue = lovelaceValueOf 8_000_000

  commitLockedValue = valueLockedBy txInfo commitValidator

  debugValue = debugInteger . getLovelace . fromValue

-- | Show an 'Integer' as decimal number. This is very inefficient and only
-- should be used for debugging.
debugInteger :: Integer -> BuiltinString
debugInteger i
  | i == 0 = "0"
  | i == 1 = "1"
  | i == 2 = "2"
  | i == 3 = "3"
  | i == 4 = "4"
  | i == 5 = "5"
  | i == 6 = "6"
  | i == 7 = "7"
  | i == 8 = "8"
  | i == 9 = "9"
  | i >= 10 = debugInteger (i `quotient` 10) `appendString` "0"
  | otherwise = "-" `appendString` debugInteger (negate i)
{-# INLINEABLE debugInteger #-}

typedValidator :: TypedValidator Initial
typedValidator =
  Scripts.mkTypedValidator @Initial
    compiledValidator
    $$(PlutusTx.compile [||wrap||])
 where
  compiledValidator =
    $$(PlutusTx.compile [||validator||])
      `PlutusTx.applyCode` PlutusTx.liftCode Commit.validatorHash

  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)

-- | Get the actual plutus script. Mainly used to serialize and use in
-- transactions.
validatorScript :: Script
validatorScript = unValidatorScript $ Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

datum :: DatumType Initial -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType Initial -> Redeemer
redeemer a = Redeemer (toBuiltinData a)

address :: Address
address = scriptHashAddress validatorHash
