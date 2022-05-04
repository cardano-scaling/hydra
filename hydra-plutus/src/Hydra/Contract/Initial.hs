{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The Initial validator which allows participants to commit or abort. To
-- focus on the off-chain datum types this is currently 'const True'.
module Hydra.Contract.Initial where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Encoding (encodeTxOut)
import Plutus.Codec.CBOR.Encoding (encodingToBuiltinByteString)
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V1.Ledger.Api (
  Datum (..),
  FromData (fromBuiltinData),
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
import Plutus.V1.Ledger.Contexts (findDatum, findOwnInput, findTxInByTxOutRef, scriptOutputsAt, valueLockedBy)
import Plutus.V1.Ledger.Value (assetClass, assetClassValueOf)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap

data InitialRedeemer
  = ViaAbort
  | ViaCommit
      { -- | Points to the committed Utxo.
        committedRef :: Maybe TxOutRef
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

type DatumType = ()
type RedeemerType = InitialRedeemer

-- | The v_initial validator verifies that:
--
--   * FIXME: spent in a transaction also consuming a v_head output
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
    ViaAbort -> True
    ViaCommit{committedRef} ->
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
  checkCommittedValue && checkLockedCommit
 where
  checkCommittedValue =
    traceIfFalse "lockedValue does not match" $
      traceIfFalse ("lockedValue: " `appendString` debugValue lockedValue) $
        traceIfFalse ("initialValue: " `appendString` debugValue initialValue) $
          traceIfFalse ("comittedValue: " `appendString` debugValue committedValue) $
            lockedValue == initialValue + committedValue

  checkLockedCommit =
    case (committedTxOut, lockedCommit) of
      (Nothing, Nothing) ->
        True
      (Nothing, Just{}) ->
        traceError "nothing committed, but TxOut in output datum"
      (Just{}, Nothing) ->
        traceError "committed TxOut, but nothing in output datum"
      (Just (ref, txOut), Just Commit{input, preSerializedOutput}) ->
        traceIfFalse "mismatch committed TxOut in datum" $
          encodingToBuiltinByteString (encodeTxOut txOut) == preSerializedOutput
            && ref == input

  initialValue =
    maybe mempty (txOutValue . txInInfoResolved) $ findOwnInput context

  committedValue =
    maybe mempty (txOutValue . snd) committedTxOut

  committedTxOut = do
    ref <- committedRef
    (ref,) . txInInfoResolved <$> findTxInByTxOutRef ref txInfo

  lockedValue = valueLockedBy txInfo commitValidator

  lockedCommit =
    case scriptOutputsAt commitValidator txInfo of
      [(dh, _)] ->
        case getDatum <$> findDatum dh txInfo of
          Nothing -> traceError "expected optional commit datum"
          (Just da) ->
            case fromBuiltinData @Commit.DatumType da of
              Nothing -> traceError "expected commit datum type, got something else"
              Just (_party, _headScriptHash, mCommit) ->
                mCommit
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
