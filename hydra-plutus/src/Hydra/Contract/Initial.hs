{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The initial validator which allows participants to commit or abort.
module Hydra.Contract.Initial where

import PlutusTx.Prelude

import Hydra.Contract.Commit (SerializedTxOut (..))
import qualified Hydra.Contract.Commit as Commit
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import Plutus.V2.Ledger.Api (
  Datum (..),
  FromData (fromBuiltinData),
  OutputDatum (..),
  PubKeyHash (getPubKeyHash),
  Redeemer (Redeemer),
  Script,
  ScriptContext (ScriptContext, scriptContextTxInfo),
  ToData (toBuiltinData),
  TokenName (unTokenName),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoSignatories),
  TxOut (txOutValue),
  TxOutRef,
  Validator (getValidator),
  ValidatorHash,
  Value (getValue),
  adaSymbol,
  adaToken,
  mkValidatorScript,
 )
import Plutus.V2.Ledger.Contexts (findDatum, findOwnInput, findTxInByTxOutRef, scriptOutputsAt, valueLockedBy)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins

data InitialRedeemer
  = Abort
  | Commit
      { -- | Points to the committed Utxo.
        committedRef :: Maybe TxOutRef
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

type DatumType = ()
type RedeemerType = InitialRedeemer

-- | The initial validator has two responsibilities:
--
--   * ensures the committed value is recorded correctly in the output datum
--
--   * ensures that the transaction was signed by the key corresponding to the
--     PubKeyHash encoded in the participation token name
--
-- NOTE: It does not need to ensure that the participation token is of some
-- specific Head currency.
validator ::
  -- | Commit validator
  ValidatorHash ->
  () ->
  InitialRedeemer ->
  ScriptContext ->
  Bool
validator commitValidator () red context =
  case red of
    Abort -> True
    Commit{committedRef} ->
      checkCommit commitValidator committedRef context
        && checkAuthor context

-- | Verifies that the commit is only done by the author
checkAuthor ::
  ScriptContext ->
  Bool
checkAuthor context@ScriptContext{scriptContextTxInfo = txInfo} =
  traceIfFalse "Missing or invalid commit author" $
    elem (unTokenName ourParticipationTokenName) (getPubKeyHash <$> txInfoSignatories txInfo)
 where
  -- NOTE: We don't check the currency symbol, only the well-formedness of the value that
  -- allows us to extract a token name, because this would be validated in other parts of the
  -- protocol.
  ourParticipationTokenName =
    case AssocMap.toList (getValue initialValue) of
      [_someAdas, (_headCurrencyHopefully, tokenMap)] ->
        case AssocMap.toList tokenMap of
          [(tk, q)] | q == 1 -> tk
          _ -> traceError "multiple head tokens or more than 1 PTs found"
      _ -> traceError "missing head tokens"

  -- TODO: DRY
  initialValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput context

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
          SerializedTxOut (Builtins.serialiseData $ toBuiltinData txOut) == serializedTxOut

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
      [(dat, _)] ->
        case dat of
          NoOutputDatum -> traceError "missing datum"
          OutputDatum _ -> traceError "unexpected inline datum"
          OutputDatumHash dh ->
            case findDatum dh txInfo of
              Nothing -> traceError "could not find datum"
              Just da ->
                case fromBuiltinData @Commit.DatumType $ getDatum da of
                  Nothing -> traceError "expected commit datum type, got something else"
                  Just (_party, _headScriptHash, mSerializedTxOut) ->
                    mSerializedTxOut
      _ -> traceError "expected single commit output"

  debugValue v =
    debugInteger . assetClassValueOf v $ assetClass adaSymbol adaToken

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

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap . validator||])
    `PlutusTx.applyCode` PlutusTx.liftCode Commit.validatorHash
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: Script
validatorScript = getValidator $ mkValidatorScript compiledValidator

validatorHash :: ValidatorHash
validatorHash = scriptValidatorHash validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
