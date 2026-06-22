{-# LANGUAGE OverloadedRecordDot #-}

-- | Differential test (Tier 2): run the Agda-extracted decidable reference checker
-- ('Hydra.Agda.Reference.checkClose') and the real Plutus validator on the same close
-- transactions, and assert they agree on the decidable layer.
--
-- Property: for the healthy close tx and every generated close mutation,
-- @reference-rejects ⇒ validator-rejects@ (the reference accepts everything spec-valid, proved
-- in spec/src/Hydra/Protocol/ReferenceBridge.agda, so a reference reject means a spec-level
-- decidable check failed, hence the validator must reject too). Crypto/value/deadline checks are
-- mocked (@const True@) on the reference side, so the converse (validator-rejects-while-reference-
-- accepts, e.g. a bad signature) is expected and not asserted.
module Hydra.Tx.Contract.CloseDifferential (spec) where

import Hydra.Prelude

import Data.Maybe (fromJust)
import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  SlotNo,
  Tx,
  UTxO,
  fromCtxUTxOTxOut,
  getTxBody,
  getTxBodyContent,
  txOuts',
  txValidityLowerBound,
  txValidityUpperBound,
  pattern TxValidityLowerBound,
  pattern TxValidityUpperBound,
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxBody (findRedeemerSpending)
import Hydra.Cardano.Api.TxOut (findTxOutByScript)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HS
import Hydra.Data.ContestationPeriod (milliseconds)
import Hydra.Ledger.Cardano.Time (slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Tx.Contract.Close.CloseUnused (genCloseCurrentMutation, healthyCloseCurrentTx)
import Hydra.Tx.Contract.Close.Healthy (healthyContestationDeadline)
import Hydra.Tx.Contract.Differential (participantSignedRef)
import PlutusLedgerApi.V3 (getPOSIXTime)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx, slotLength, systemStart)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), applyMutation, modifyInlineDatum, replaceContestationDeadline)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The extracted reference verdict on a close @(tx, utxo)@, or 'Nothing' if the head
-- datums/redeemer could not be read — in which case the reference abstains and the differential
-- property imposes no constraint.
closeRefVerdict :: (Tx, UTxO) -> Maybe Bool
closeRefVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  inSt <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  HS.Open od <- Just (inSt :: HS.State)
  headOut <- txOuts' tx !!? 0
  outSt <- fromScriptData =<< txOutScriptData headOut
  HS.Closed cd <- Just (outSt :: HS.State)
  HS.Close cr <- findRedeemerSpending tx headIn :: Maybe HS.Input
  -- The reference's deadline conjunct `tfinal == validityHi + cp` needs the tx upper validity
  -- bound as POSIXTime ms (`makeContestationDeadline`/`addContestationPeriod`). When the tx has
  -- no finite upper bound the validator rejects (`InfiniteUpperBound`) but the reference cannot
  -- represent an infinite bound, so it abstains (the differential imposes no constraint there).
  validityHi <- txUpperBoundPOSIX tx
  -- The bounded-validity conjunct `hi - lo <= cp` also needs the tx LOWER bound; with no finite lower
  -- bound the reference abstains (the validator's range-width check then has nothing to compare).
  validityLo <- txLowerBoundPOSIX tx
  let hsOpen =
        Ref.MkOpen
          od.version
          (toInteger (milliseconds od.contestationPeriod))
      hsClosed =
        Ref.MkClosed
          cd.version
          (toInteger (milliseconds cd.contestationPeriod))
          cd.snapshotNumber
          (fromIntegral (length cd.contesters))
          (getPOSIXTime cd.contestationDeadline)
  pure
    ( Ref.checkClose (Ref.mkOps (\_ _ _ -> True)) hsOpen hsClosed (tagOf cr) validityHi validityLo
        -- §5.6 mustBeSignedByParticipant: some signer holds a PT in the closed head output (shared checker).
        && participantSignedRef tx headOut
    )
 where
  tagOf = \case
    HS.CloseInitial -> Ref.CloseInitialT
    HS.CloseAny{} -> Ref.CloseAnyT
    HS.CloseUnused{} -> Ref.CloseUnusedT
    HS.CloseUsed{} -> Ref.CloseUsedT

-- | The healthy close tx with the output datum's recorded contestation deadline shifted by 1ms off
-- the correct `validityHi + cp`, so the reference's deadline conjunct (and the real validator's
-- `IncorrectClosedContestationDeadline`) must reject it.
deadlineDriftTx :: (Tx, UTxO)
deadlineDriftTx =
  applyMutation
    (ChangeOutput 0 (modifyInlineDatum (replaceContestationDeadline driftedDeadline) headTxOut))
    healthyCloseCurrentTx
 where
  headTxOut = fromJust $ txOuts' (fst healthyCloseCurrentTx) !!? 0
  driftedDeadline = posixFromUTCTime healthyContestationDeadline + 1

-- | The tx upper validity bound as POSIXTime milliseconds, matching the slot→time translation the
-- ledger applies for the Plutus context (fixture @systemStart@/@slotLength@), or 'Nothing' if the
-- tx has no finite upper bound. This equals the validator's @tMax@ bit-for-bit ONLY because
-- 'evaluateTx' here uses the linear 'fixedEpochInfo' (single era, @slotLength@=1s, @systemStart@=0):
-- under a non-linear 'EraHistory' this would diverge from the ledger's slot→POSIXTime map and could
-- false-reject, so do not reuse against txs evaluated under a multi-era history.
txUpperBoundPOSIX :: Tx -> Maybe Integer
txUpperBoundPOSIX tx =
  case tx & getTxBody & getTxBodyContent & txValidityUpperBound of
    TxValidityUpperBound upperBound -> Just (slotToPOSIXMs upperBound)
    _ -> Nothing
 where
  slotToPOSIXMs :: SlotNo -> Integer
  slotToPOSIXMs = getPOSIXTime . posixFromUTCTime . slotNoToUTCTime systemStart slotLength

-- | The tx LOWER validity bound as POSIXTime ms (same slot→time translation), or 'Nothing' if there
-- is no finite lower bound.
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
  prop "reference accepts the healthy close tx" $
    closeRefVerdict healthyCloseCurrentTx === Just True

  prop "validator accepts the healthy close tx" $
    validatorAccepts healthyCloseCurrentTx === True

  -- Non-vacuity of the C2 deadline conjunct: a close whose recorded deadline drifts off
  -- `validityHi + cp` must make the reference reject (not merely abstain), so the differential
  -- genuinely exercises the deadline check rather than mocking it.
  prop "reference rejects a close with a drifted contestation deadline" $
    closeRefVerdict deadlineDriftTx === Just False

  prop "validator also rejects the drifted-deadline close" $
    validatorAccepts deadlineDriftTx === False

  prop "differential: reference-reject ⇒ validator-reject (CloseUnused mutations)" $
    forAll (genCloseCurrentMutation healthyCloseCurrentTx) $ \SomeMutation{mutation} ->
      let m = applyMutation mutation healthyCloseCurrentTx
       in differential m
 where
  differential :: (Tx, UTxO) -> Property
  differential m =
    case closeRefVerdict m of
      Just False -> validatorAccepts m === False
      _ -> property True
