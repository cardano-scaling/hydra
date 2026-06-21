-- | Differential test (Tier 2) for the νDeposit validator (@deposit.ak@): run the Agda-extracted
-- decidable reference checkers ('Hydra.Agda.Reference.checkRecover' / 'checkClaim') and the real
-- Plutus deposit validator on the same transactions, and assert they agree on the decidable layer.
--
-- Property (both arms): for the healthy tx and every generated mutation,
-- @reference-rejects ⇒ validator-rejects@. A reference reject means a spec-level decidable check
-- failed, hence the validator must reject too.
--
--   * Recover arm: the tx is posted strictly AFTER the recover deadline (@tRecover < validityLo@,
--     deposit.ak @after_deadline@ / @DepositPeriodNotReached@), proved to reflect @recoverValid@.
--   * Claim arm: the increment collecting the deposit is posted BEFORE the deadline
--     (@validityHi ≤ tRecover@, deposit.ak @before_deadline@ / @DepositPeriodSurpassed@), proved to
--     reflect @claimValid@. The own-head binding (@expect_increment_redeemer@) is mocked.
--
-- The proofs both live in spec/src/Hydra/Protocol/ReferenceBridge.agda. The recovered-outputs
-- serialisation-hash equality (Recover) and the own-head binding (Claim) are mocked (@const True@) on
-- the reference side, so the converse (a check the validator catches but the reference does not) is
-- expected and not asserted.
module Hydra.Tx.Contract.DepositDifferential (spec) where

import Hydra.Prelude

import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  SlotNo,
  Tx,
  UTxO,
  fromCtxUTxOTxOut,
  getTxBody,
  getTxBodyContent,
  resolveInputsUTxO,
  txValidityLowerBound,
  txValidityUpperBound,
  pattern TxValidityLowerBound,
  pattern TxValidityUpperBound,
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxOut (findTxOutByScript)
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx.Contract.Deposit (healthyDeadlineSlot)
import Hydra.Tx.Contract.Increment (genIncrementMutation, healthyIncrementTx)
import Hydra.Tx.Contract.Recover (genRecoverMutation, healthyRecoverTx, recoverSlotNo)
import PlutusLedgerApi.V3 (getPOSIXTime)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, slotLength, systemStart)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), applyMutation)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The extracted reference verdict on a recover @(tx, utxo)@, or 'Nothing' if the deposit
-- datum/script output could not be read or the tx has no finite lower validity bound — in which case
-- the reference abstains and the differential property imposes no constraint.
recoverRefVerdict :: (Tx, UTxO) -> Maybe Bool
recoverRefVerdict (tx, utxo) = do
  (_, depositOut) <- findTxOutByScript (resolveInputsUTxO utxo tx) depositValidatorScript
  dat <- txOutScriptData (fromCtxUTxOTxOut depositOut)
  (_, deadline, _) <- fromScriptData dat :: Maybe Deposit.DepositDatum
  validityLo <- txLowerBoundPOSIX tx
  pure
    ( Ref.checkRecover
        (Ref.mkOpsRecover (const True))
        (Ref.MkRecoverIO (getPOSIXTime deadline) validityLo)
    )

-- | The extracted reference verdict on a claim @(tx, utxo)@ (an increment collecting a deposit): the
-- before-deadline conjunct @validityHi ≤ tRecover@. 'Nothing' (abstains) if the spent deposit datum
-- cannot be read or the tx has no finite upper validity bound. The own-head binding is mocked.
claimRefVerdict :: (Tx, UTxO) -> Maybe Bool
claimRefVerdict (tx, utxo) = do
  (_, depositOut) <- findTxOutByScript (resolveInputsUTxO utxo tx) depositValidatorScript
  dat <- txOutScriptData (fromCtxUTxOTxOut depositOut)
  (_, deadline, _) <- fromScriptData dat :: Maybe Deposit.DepositDatum
  validityHi <- txUpperBoundPOSIX tx
  pure
    ( Ref.checkClaim
        (Ref.mkOpsClaim (const True))
        (Ref.MkClaimIO (getPOSIXTime deadline) validityHi)
    )

-- | The healthy recover tx with its lower validity bound pulled back onto the recover deadline slot,
-- so the "strictly after the deadline" check fails (@tRecover < validityLo@ becomes @tRec < tRec@,
-- false): both the reference and the validator (@DepositPeriodNotReached@) must reject. Demonstrates
-- the deadline conjunct is live (non-vacuous), not merely abstained.
deadlineNotReachedRecoverTx :: (Tx, UTxO)
deadlineNotReachedRecoverTx =
  applyMutation
    (ChangeValidityLowerBound (TxValidityLowerBound (recoverSlotNo - 1)))
    healthyRecoverTx

-- | The healthy claim (increment) tx with its upper validity bound pushed one slot PAST the deposit
-- deadline (@healthyDeadlineSlot@), so the before-deadline check @validityHi ≤ tRecover@ becomes false:
-- both the reference and the validator (@DepositPeriodSurpassed@) must reject. Demonstrates the claim
-- conjunct is live (non-vacuous), not merely abstained.
deadlineSurpassedClaimTx :: (Tx, UTxO)
deadlineSurpassedClaimTx =
  applyMutation
    (ChangeValidityUpperBound (TxValidityUpperBound (healthyDeadlineSlot + 1)))
    healthyIncrementTx

-- | The tx lower validity bound as POSIXTime milliseconds, matching the slot→time translation the
-- ledger applies for the Plutus context (fixture @systemStart@/@slotLength@; see the
-- 'Hydra.Tx.Contract.CloseDifferential' note on the linear 'fixedEpochInfo' assumption), or 'Nothing'
-- if the tx has no finite lower bound (the validator's @DepositNoLowerBoundDefined@).
txLowerBoundPOSIX :: Tx -> Maybe Integer
txLowerBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityLowerBound of
    TxValidityLowerBound lowerBound -> Just (slotToPOSIXMs lowerBound)
    _ -> Nothing

-- | The tx upper validity bound as POSIXTime milliseconds (same translation), or 'Nothing' if there is
-- no finite upper bound (the validator's @DepositNoUpperBoundDefined@).
txUpperBoundPOSIX :: Tx -> Maybe Integer
txUpperBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityUpperBound of
    TxValidityUpperBound upperBound -> Just (slotToPOSIXMs upperBound)
    _ -> Nothing

slotToPOSIXMs :: SlotNo -> Integer
slotToPOSIXMs = getPOSIXTime . posixFromUTCTime . slotNoToUTCTime systemStart slotLength

spec :: Spec
spec = parallel $ do
  prop "reference accepts the healthy recover tx" $
    recoverRefVerdict healthyRecoverTx === Just True

  prop "validator accepts the healthy recover tx" $
    validatorAccepts healthyRecoverTx === True

  prop "reference rejects a recover not strictly after the deadline" $
    recoverRefVerdict deadlineNotReachedRecoverTx === Just False

  prop "validator also rejects the not-after-deadline recover" $
    validatorAccepts deadlineNotReachedRecoverTx === False

  prop "differential: reference-reject ⇒ validator-reject (Recover mutations)" $
    forAll (genRecoverMutation healthyRecoverTx) $ \SomeMutation{mutation} ->
      differential recoverRefVerdict (applyMutation mutation healthyRecoverTx)

  prop "reference accepts the healthy claim (increment) tx" $
    claimRefVerdict healthyIncrementTx === Just True

  prop "validator accepts the healthy claim (increment) tx" $
    validatorAccepts healthyIncrementTx === True

  prop "reference rejects a claim posted after the deposit deadline" $
    claimRefVerdict deadlineSurpassedClaimTx === Just False

  prop "validator also rejects the after-deadline claim" $
    validatorAccepts deadlineSurpassedClaimTx === False

  prop "differential: reference-reject ⇒ validator-reject (Claim/Increment mutations)" $
    forAll (genIncrementMutation healthyIncrementTx) $ \SomeMutation{mutation} ->
      differential claimRefVerdict (applyMutation mutation healthyIncrementTx)
 where
  differential :: ((Tx, UTxO) -> Maybe Bool) -> (Tx, UTxO) -> Property
  differential verdict m =
    case verdict m of
      Just False -> validatorAccepts m === False
      _ -> property True
