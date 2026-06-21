-- | Differential test (Tier 2) for the νDeposit validator (@deposit.ak@): run the Agda-extracted
-- decidable reference checker ('Hydra.Agda.Reference.checkRecover') and the real Plutus deposit
-- validator on the same recover transactions, and assert they agree on the decidable layer.
--
-- Property: for the healthy recover tx and every generated recover mutation,
-- @reference-rejects ⇒ validator-rejects@. The reference mirrors the decidable Recover conjunct
-- (the tx is posted strictly after the recover deadline: @tRecover < validityLo@), proved to reflect
-- @recoverValid@ in spec/src/Hydra/Protocol/ReferenceBridge.agda, so a reference reject means a
-- spec-level decidable check failed, hence the validator must reject too. The recovered-outputs
-- serialisation-hash equality is mocked (@const True@) on the reference side, so the converse (a hash
-- mismatch the validator catches) is expected and not asserted.
--
-- Scope: this covers the νDeposit Recover arm. The Claim arm's decidable conjunct (before-deadline,
-- @hi ≤ tRecover@) is type-encoded in @claimValid@ and the increment differential already runs the
-- validator's Claim path end-to-end; mirroring it in the reference is a possible follow-up.
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
  pattern TxValidityLowerBound,
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxOut (findTxOutByScript)
import Hydra.Contract.Deposit qualified as Deposit
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
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

-- | The healthy recover tx with its lower validity bound pulled back onto the recover deadline slot,
-- so the "strictly after the deadline" check fails (@tRecover < validityLo@ becomes @tRec < tRec@,
-- false): both the reference and the validator (@DepositPeriodNotReached@) must reject. Demonstrates
-- the deadline conjunct is live (non-vacuous), not merely abstained.
deadlineNotReachedRecoverTx :: (Tx, UTxO)
deadlineNotReachedRecoverTx =
  applyMutation
    (ChangeValidityLowerBound (TxValidityLowerBound (recoverSlotNo - 1)))
    healthyRecoverTx

-- | The tx lower validity bound as POSIXTime milliseconds, matching the slot→time translation the
-- ledger applies for the Plutus context (fixture @systemStart@/@slotLength@; see the
-- 'Hydra.Tx.Contract.CloseDifferential' note on the linear 'fixedEpochInfo' assumption), or 'Nothing'
-- if the tx has no finite lower bound (the validator's @DepositNoLowerBoundDefined@).
txLowerBoundPOSIX :: Tx -> Maybe Integer
txLowerBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityLowerBound of
    TxValidityLowerBound lowerBound -> Just (slotToPOSIXMs lowerBound)
    _ -> Nothing
 where
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
      differential (applyMutation mutation healthyRecoverTx)
 where
  differential :: (Tx, UTxO) -> Property
  differential m =
    case recoverRefVerdict m of
      Just False -> validatorAccepts m === False
      _ -> property True
