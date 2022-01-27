{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The Initial validator which allows participants to commit or abort. To
-- focus on the off-chain datum types this is currently 'const True'.
module Hydra.Contract.Initial where

import Ledger hiding (validatorHash)
import PlutusTx.Prelude

import Hydra.Contract.Commit (SerializedTxOut (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (encodeTxOut)
import Ledger.Typed.Scripts (TypedValidator, ValidatorType, ValidatorTypes (..))
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Codec.CBOR.Encoding (encodingToBuiltinByteString)
import Plutus.V1.Ledger.Ada (fromValue, getLovelace)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import PlutusTx.IsData.Class (ToData (..), fromBuiltinData)

data Initial

data InitialRedeemer
  = Abort
  | Commit
      { -- | Points to the committed Utxo.
        committedRef :: Maybe TxOutRef
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

instance Scripts.ValidatorTypes Initial where
  type DatumType Initial = PubKeyHash
  type RedeemerType Initial = InitialRedeemer

validator ::
  -- | Commit validator
  ValidatorHash ->
  -- | The Hydra party which committed
  PubKeyHash ->
  InitialRedeemer ->
  ScriptContext ->
  Bool
validator commitValidator _datum red context =
  case red of
    Abort -> True
    Commit{committedRef} -> checkCommit commitValidator committedRef context

checkCommit ::
  -- | Commit validator
  ValidatorHash ->
  Maybe TxOutRef ->
  ScriptContext ->
  Bool
checkCommit commitValidator committedRef context@ScriptContext{scriptContextTxInfo = txInfo} =
  checkCommittedValue && checkSerializedTxOut
 where
  checkCommittedValue =
    traceIfFalse "commitLockedValue does not match" $
      traceIfFalse ("commitLockedValue: " `appendString` debugValue commitLockedValue) $
        traceIfFalse ("initialValue: " `appendString` debugValue initialValue) $
          traceIfFalse ("comittedValue: " `appendString` debugValue committedValue) $
            commitLockedValue == initialValue + committedValue

  checkSerializedTxOut =
    case (committedTxOut, commitLockedSerializedTxOut) of
      (Nothing, Nothing) ->
        True
      (Nothing, Just{}) ->
        traceError "nothing committed, but TxOut in output datum"
      (Just{}, Nothing) ->
        traceError "committed TxOut, but nothing in output datum"
      (Just txOut, Just serializedTxOut) ->
        traceIfFalse "mismatch committed TxOut in datum" $
          SerializedTxOut (encodingToBuiltinByteString (encodeTxOut txOut)) == serializedTxOut

  initialValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput context

  committedValue =
    maybe mempty txOutValue committedTxOut

  committedTxOut = do
    ref <- committedRef
    txInInfoResolved <$> findTxInByTxOutRef ref txInfo

  commitLockedValue = valueLockedBy txInfo commitValidator

  commitLockedSerializedTxOut =
    case scriptOutputsAt commitValidator txInfo of
      [(dh, _)] ->
        case getDatum <$> findDatum dh txInfo of
          Nothing -> traceError "expected optional commit datum"
          (Just da) ->
            case fromBuiltinData @(DatumType Commit.Commit) da of
              Nothing -> traceError "expected commit datum type, got something else"
              Just (_party, _headScriptHash, mSerializedTxOut) ->
                mSerializedTxOut
      _ -> traceError "expected single commit output"

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
  wrap = Scripts.wrapValidator @(DatumType Initial) @(RedeemerType Initial)

compiledValidator :: CompiledCode (ValidatorType Initial)
compiledValidator =
  $$(PlutusTx.compile [||validator||])
    `PlutusTx.applyCode` PlutusTx.liftCode Commit.validatorHash

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
