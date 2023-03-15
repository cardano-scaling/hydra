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
import Hydra.Contract.Util (mustBurnST)
import Hydra.ScriptContext (
  ScriptContext (..),
  TxInfo (txInfoMint, txInfoSignatories),
  findDatum,
  findOwnInput,
  findTxInByTxOutRef,
  scriptOutputsAt,
  valueLockedBy,
 )
import Plutus.Extras (ValidatorType, scriptValidatorHash, wrapValidator)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Value (isZero)
import PlutusLedgerApi.V2 (
  CurrencySymbol,
  Datum (..),
  FromData (fromBuiltinData),
  OutputDatum (..),
  PubKeyHash (getPubKeyHash),
  Redeemer (Redeemer),
  ScriptHash,
  ToData (toBuiltinData),
  TokenName (unTokenName),
  TxInInfo (..),
  TxOut (txOutValue),
  TxOutRef,
  Value (getValue),
 )
import PlutusTx (CompiledCode)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins

data InitialRedeemer
  = ViaAbort
  | ViaCommit
      { committedRefs :: [TxOutRef]
      -- ^ Points to the committed Utxo.
      }

PlutusTx.unstableMakeIsData ''InitialRedeemer

type DatumType = CurrencySymbol
type RedeemerType = InitialRedeemer

-- | The v_initial validator verifies that:
--
--   * spent in a transaction also consuming a v_head output
--
--   * ensures the committed value is recorded correctly in the output datum
--
--   * ensures that the transaction was signed by the key corresponding to the
--     PubKeyHash encoded in the participation token name
--
-- NOTE: It does not need to ensure that the participation token is of some
-- specific Head currency.
validator ::
  -- | Hash of the commit validator
  ScriptHash ->
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
    ViaCommit{committedRefs} ->
      checkCommit commitValidator headId committedRefs context

checkCommit ::
  -- | Hash of the commit validator
  ScriptHash ->
  -- | Head id
  CurrencySymbol ->
  [TxOutRef] ->
  ScriptContext ->
  Bool
checkCommit commitValidator headId committedRefs context =
  checkCommittedValue
    && checkLockedCommit
    && checkHeadId
    && mustBeSignedByParticipant
    && mustNotMintOrBurn
 where
  checkCommittedValue =
    traceIfFalse $(errorCode LockedValueDoesNotMatch) $
      lockedValue == initialValue + committedValue

  checkLockedCommit =
    traceIfFalse $(errorCode MismatchCommittedTxOutInDatum) $
      go (committedUTxO, lockedCommits)
   where
    go = \case
      ([], []) ->
        True
      ([], (_ : _)) ->
        traceError $(errorCode NothingCommittedButTxOutInOutputDatum)
      ((_ : _), []) ->
        traceError $(errorCode CommittedTxOutButNothingInOutputDatum)
      (TxInInfo{txInInfoOutRef, txInInfoResolved} : restCommitted, Commit{input, preSerializedOutput} : restCommits) ->
        Builtins.serialiseData (toBuiltinData txInInfoResolved) == preSerializedOutput
          && txInInfoOutRef == input
          && go (restCommitted, restCommits)

  checkHeadId =
    traceIfFalse $(errorCode WrongHeadIdInCommitDatum) $
      headId' == headId

  mustBeSignedByParticipant =
    traceIfFalse $(errorCode MissingOrInvalidCommitAuthor) $
      unTokenName ourParticipationTokenName `elem` (getPubKeyHash <$> txInfoSignatories txInfo)

  mustNotMintOrBurn =
    traceIfFalse $(errorCode MintingOrBurningIsForbidden) $
      isZero $
        txInfoMint txInfo

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
    foldMap (txOutValue . txInInfoResolved) committedUTxO

  committedUTxO = do
    flip fmap committedRefs $ \ref ->
      case findTxInByTxOutRef ref txInfo of
        Nothing -> traceError "outref not found"
        Just txInInfo -> txInInfo

  lockedValue = valueLockedBy txInfo commitValidator

  (lockedCommits, headId') =
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
                  Just (_party, commits, hid) ->
                    (commits, hid)
      _ -> traceError $(errorCode ExpectedSingleCommitOutput)

  ScriptContext{scriptContextTxInfo = txInfo} = context

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap . validator||])
    `PlutusTx.applyCode` PlutusTx.liftCode Commit.validatorHash
 where
  wrap = wrapValidator @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
