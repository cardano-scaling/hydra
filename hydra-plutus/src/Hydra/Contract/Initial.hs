{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | The initial validator which allows participants to commit or abort.
module Hydra.Contract.Initial where

import PlutusTx.Prelude

import Hydra.Contract.Commit (Commit (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Error (errorCode)
import Hydra.Contract.InitialError (InitialError (..))
import Hydra.Contract.Util (mustBurnST, mustNotMintOrBurn)
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
  TxInfo (txInfoMint, txInfoSignatories),
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

data InitialRedeemer
  = ViaAbort
  | ViaCommit
      { -- | Points to the committed Utxo.
        committedRef :: Maybe TxOutRef
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

type DatumType = CurrencySymbol
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
  DatumType ->
  RedeemerType ->
  ScriptContext ->
  Bool
validator commitValidator headId red context =
  case red of
    ViaAbort ->
      traceIfFalse
        $(errorCode STNotBurned)
        (mustBurnST (txInfoMint $ scriptContextTxInfo context) headId)
    ViaCommit{committedRef} ->
      checkCommit commitValidator headId committedRef context

checkCommit ::
  -- | Commit validator
  ValidatorHash ->
  -- | Head id
  CurrencySymbol ->
  Maybe TxOutRef ->
  ScriptContext ->
  Bool
checkCommit commitValidator headId committedRef context =
  checkCommittedValue
    && checkLockedCommit
    && mustBeSignedByParticipant
    && mustNotMintOrBurn txInfo
 where
  checkCommittedValue =
    traceIfFalse $(errorCode LockedValueDoesNotMatch) $
      lockedValue == initialValue + committedValue

  checkLockedCommit =
    case (committedTxOut, lockedCommit) of
      (Nothing, Nothing) ->
        True
      (Nothing, Just{}) ->
        traceError $(errorCode NothingCommittedButTxOutInOutputDatum)
      (Just{}, Nothing) ->
        traceError $(errorCode CommittedTxOutButNothingInOutputDatum)
      (Just (ref, txOut), Just Commit{input, preSerializedOutput}) ->
        traceIfFalse $(errorCode MismatchCommittedTxOutInDatum) $
          Builtins.serialiseData (toBuiltinData txOut) == preSerializedOutput
            && ref == input

  mustBeSignedByParticipant =
    traceIfFalse $(errorCode MissingOrInvalidCommitAuthor) $
      unTokenName ourParticipationTokenName `elem` (getPubKeyHash <$> txInfoSignatories txInfo)

  ourParticipationTokenName =
    case AssocMap.lookup headId (getValue initialValue) of
      Nothing -> traceError $(errorCode CouldNotFindTheCorrectCurrencySymbolInTokens)
      Just tokenMap ->
        case AssocMap.toList tokenMap of
          [(tk, q)] | q == 1 -> tk
          _moreThanOneToken -> traceError $(errorCode MultipleHeadTokensOrMoreThan1PTsFound)

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
          NoOutputDatum -> traceError $(errorCode MissingDatum)
          OutputDatum _ -> traceError $(errorCode UnexpectedInlineDatum)
          OutputDatumHash dh ->
            case findDatum dh txInfo of
              Nothing -> traceError $(errorCode CouldNotFindDatum)
              Just da ->
                case fromBuiltinData @Commit.DatumType $ getDatum da of
                  Nothing -> traceError $(errorCode ExpectedCommitDatumTypeGotSomethingElse)
                  Just (_party, mCommit, _headId) ->
                    -- FIXME: headId in output not checked!
                    mCommit
      _ -> traceError $(errorCode ExpectedSingleCommitOutput)

  ScriptContext{scriptContextTxInfo = txInfo} = context

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
