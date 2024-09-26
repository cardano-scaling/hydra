{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-specialize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}
-- Plutus core version to compile to. In babbage era, that is Cardano protocol
-- version 7 and 8, only plutus-core version 1.0.0 is available.
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | The initial validator which allows participants to commit or abort.
module Hydra.Contract.Initial where

import PlutusTx.Prelude

import Hydra.Cardano.Api (PlutusScriptVersion (PlutusScriptV2))
import Hydra.Contract.Commit (Commit (..))
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Error (errorCode)
import Hydra.Contract.InitialError (InitialError (..))
import Hydra.Contract.Util (mustBurnST)
import Hydra.Plutus.Extras (ValidatorType, scriptValidatorHash)
import Hydra.ScriptContext (
  ScriptContext (..),
  TxInfo (txInfoMint, txInfoSignatories),
  findOwnInput,
  findTxInByTxOutRef,
  scriptOutputsAt,
  valueLockedBy,
 )
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.Common (SerialisedScript, serialiseCompiledCode)
import PlutusLedgerApi.V1.Value (geq, isZero)
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
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins

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
      -- NOTE: Ada in initialValue is usually lower than in the locked ADA due
      -- to higher deposit needed for commit output than for initial output
      lockedValue `geq` (initialValue + committedValue)

  checkLockedCommit =
    traceIfFalse $(errorCode MismatchCommittedTxOutInDatum) $
      go (committedUTxO, lockedCommits)
   where
    go = \case
      ([], []) ->
        True
      ([], _ : _) ->
        traceError $(errorCode MissingCommittedTxOutInOutputDatum)
      (_ : _, []) ->
        traceError $(errorCode CommittedTxOutMissingInOutputDatum)
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
        Nothing -> traceError $(errorCode OutRefNotFound)
        Just txInInfo -> txInInfo

  lockedValue = valueLockedBy txInfo commitValidator

  (lockedCommits, headId') =
    case scriptOutputsAt commitValidator txInfo of
      [(dat, _)] ->
        case dat of
          NoOutputDatum -> traceError $(errorCode MissingDatum)
          OutputDatumHash _dh ->
            traceError $(errorCode UnexpectedNonInlineDatum)
          OutputDatum da ->
            case fromBuiltinData @Commit.DatumType $ getDatum da of
              Nothing -> traceError $(errorCode ExpectedCommitDatumTypeGotSomethingElse)
              Just (_party, commits, hid) ->
                (commits, hid)
      _ -> traceError $(errorCode ExpectedSingleCommitOutput)

  ScriptContext{scriptContextTxInfo = txInfo} = context

compiledValidator :: CompiledCode ValidatorType
compiledValidator =
  $$(PlutusTx.compile [||wrap . validator||])
    `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 Commit.validatorHash
 where
  wrap = mkUntypedValidator @ScriptContext @DatumType @RedeemerType

validatorScript :: SerialisedScript
validatorScript = serialiseCompiledCode compiledValidator

validatorHash :: ScriptHash
validatorHash = scriptValidatorHash PlutusScriptV2 validatorScript

datum :: DatumType -> Datum
datum a = Datum (toBuiltinData a)

redeemer :: RedeemerType -> Redeemer
redeemer a = Redeemer (toBuiltinData a)
