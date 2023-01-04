{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The initial validator which allows participants to commit or abort.
module Hydra.Contract.Initial where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
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
  mkValidatorScript,
 )
import Plutus.V2.Ledger.Contexts (findDatum, findOwnInput, findTxInByTxOutRef, scriptOutputsAt, valueLockedBy)
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins

newtype InitialDatum = InitialDatum
  { headId :: CurrencySymbol
  }

PlutusTx.unstableMakeIsData ''InitialDatum

data InitialRedeemer
  = ViaAbort
  | ViaCommit
      { -- | Points to the committed Utxo.
        committedRef :: Maybe TxOutRef
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

type DatumType = InitialDatum
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
  InitialDatum ->
  InitialRedeemer ->
  ScriptContext ->
  Bool
validator commitValidator InitialDatum{headId} red context =
  case red of
    ViaAbort -> True
    ViaCommit{committedRef} ->
      checkCommit commitValidator committedRef context
        && checkAuthorAndHeadPolicy context headId

-- | Verifies that the commit is only done by the author
checkAuthorAndHeadPolicy ::
  ScriptContext ->
  CurrencySymbol ->
  Bool
checkAuthorAndHeadPolicy context@ScriptContext{scriptContextTxInfo = txInfo} headId =
  unTokenName ourParticipationTokenName `elem` (getPubKeyHash <$> txInfoSignatories txInfo)
    && policyId == headId
 where
  (policyId, ourParticipationTokenName) =
    case AssocMap.toList (getValue initialValue) of
      [_someAdas, (headCurrencyHopefully, tokenMap)] ->
        case AssocMap.toList tokenMap of
          [(tk, q)] | q == 1 -> (headCurrencyHopefully, tk)
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
        Builtins.serialiseData (toBuiltinData txOut) == preSerializedOutput
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
                  Just (_party, _headScriptHash, mCommit) ->
                    mCommit
      _ -> traceError "expected single commit output"

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
