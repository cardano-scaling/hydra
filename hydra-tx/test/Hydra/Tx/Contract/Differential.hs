{-# LANGUAGE OverloadedRecordDot #-}

-- | Differential tests (Tier 2), generalizing 'Hydra.Tx.Contract.CloseDifferential' to the
-- increment, decrement, contest and fanout transaction families: run the Agda-extracted decidable
-- reference checkers ('Hydra.Agda.Reference') and the real Plutus validator on the same
-- transactions and assert @reference-rejects ⇒ validator-rejects@.
--
-- Each reference checker mirrors only the decidable conjuncts of the corresponding
-- @*Valid@ bundle (proved to reflect them in spec/src/Hydra/Protocol/ReferenceBridge.agda):
--
--   * increment\/decrement: produced version is @suc@ the input version (@VersionNotIncremented@);
--   * contest: version preserved, snapshot strictly increases (@TooOldSnapshot@), one contester
--     appended;
--   * fanout\/finalPartialFanout: @0 < m@ outputs (@FanoutZeroOutputs@, the §5.8 m>0 guard).
--
-- Crypto\/value\/accumulator\/deadline conjuncts are mocked (@const True@) on the reference side,
-- so the converse direction (validator-rejects-while-reference-accepts) is expected and not
-- asserted.
module Hydra.Tx.Contract.Differential (spec) where

import Hydra.Prelude

import Hydra.Agda.Reference qualified as Ref
import Hydra.Cardano.Api (
  Coin (..),
  Tx,
  TxIn,
  UTxO,
  fromCtxUTxOTxOut,
  selectLovelace,
  txOutValue,
  txOuts',
 )
import Hydra.Cardano.Api.ScriptData (fromScriptData, txOutScriptData)
import Hydra.Cardano.Api.TxBody (findRedeemerSpending)
import Hydra.Cardano.Api.TxOut (findTxOutByScript)
import Hydra.Contract.Head qualified as Head
import Hydra.Contract.HeadState qualified as HS
import Hydra.Plutus (depositValidatorScript)
import Hydra.Tx.Contract.Contest.ContestCurrent (genContestMutation)
import Hydra.Tx.Contract.Contest.Healthy (healthyContestTx)
import Hydra.Tx.Contract.Decrement (genDecrementMutation, healthyDecrementTx)
import Hydra.Tx.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Tx.Contract.Increment (genIncrementMutation, healthyIncrementTx)
import Test.Hydra.Ledger.Cardano.Fixtures (evaluateTx)
import Test.Hydra.Prelude
import Test.Hydra.Tx.Mutation (SomeMutation (..), applyMutation)
import Test.QuickCheck (Property, forAll, property, (===))

-- | Does the real Plutus validator accept @(tx, utxo)@ (phase-2 success, no script error)?
validatorAccepts :: (Tx, UTxO) -> Bool
validatorAccepts (tx, utxo) =
  case evaluateTx tx utxo of
    Right report -> all isRight report
    Left _ -> False

-- | The spent head datum (continuing input) for @(tx, utxo)@.
inputState :: (Tx, UTxO) -> Maybe (HS.State, TxIn)
inputState (_, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  st <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  pure (st, headIn)

-- | The produced (continuing) head datum at output index 0, if any.
outputState :: (Tx, UTxO) -> Maybe HS.State
outputState (tx, _) = do
  headOut <- txOuts' tx !!? 0
  fromScriptData =<< txOutScriptData headOut

-- ── increment / decrement ─────────────────────────────────────────────────────────────────
-- Increment steps Open→Open; the reference now checks the version bump AND lovelace value
-- conservation (adaIn + adaDelta == adaOut), reading the real head-input / deposit / head-output
-- lovelace off the tx — so a value-breaking validator change is caught, not just version drift.
incRefVerdict :: (Tx, UTxO) -> Maybe Bool
incRefVerdict (tx, utxo) = do
  (headIn, headInOut) <- findTxOutByScript utxo Head.validatorScript
  inSt <- fromScriptData =<< txOutScriptData (fromCtxUTxOTxOut headInOut)
  HS.Open od <- Just (inSt :: HS.State)
  headOut <- txOuts' tx !!? 0
  outSt <- fromScriptData =<< txOutScriptData headOut
  HS.Open od' <- Just (outSt :: HS.State)
  HS.Increment{} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  (_, depositOut) <- findTxOutByScript utxo depositValidatorScript
  pure $
    Ref.checkInc
      (Ref.mkOpsInc (const True))
      (Ref.MkIncIO od.version od'.version (lovelace headInOut) (lovelace depositOut) (lovelace headOut))
 where
  lovelace o = let Coin n = selectLovelace (txOutValue o) in n

-- Decrement steps Open→Open too; its decommit value is not yet supplied as an extractable lovelace,
-- so the reference checks the version discipline only (ada fields ignored by `checkDec`).
decRefVerdict :: (Tx, UTxO) -> Maybe Bool
decRefVerdict m@(tx, _) = do
  (inSt, headIn) <- inputState m
  HS.Open od <- Just inSt
  outSt <- outputState m
  HS.Open od' <- Just outSt
  HS.Decrement{} <- findRedeemerSpending tx headIn :: Maybe HS.Input
  pure (Ref.checkDec (Ref.mkOpsInc (const True)) (Ref.MkIncIO od.version od'.version 0 0 0))

-- ── contest ─────────────────────────────────────────────────────────────────────────────────
contestRefVerdict :: (Tx, UTxO) -> Maybe Bool
contestRefVerdict m = do
  (inSt, headIn) <- inputState m
  HS.Closed cd <- Just inSt
  outSt <- outputState m
  HS.Closed cd' <- Just outSt
  HS.Contest _ <- findRedeemerSpending (fst m) headIn :: Maybe HS.Input
  pure $
    Ref.checkContest
      (Ref.mkOpsContest (const True))
      ( Ref.MkContestIO
          cd.version
          cd'.version
          cd.snapshotNumber
          cd'.snapshotNumber
          (fromIntegral (length cd.contesters))
          (fromIntegral (length cd'.contesters))
      )

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
fanoutRefVerdict :: (Tx, UTxO) -> Maybe Bool
fanoutRefVerdict m = do
  (_, headIn) <- inputState m
  HS.Fanout{HS.numberOfFanoutOutputs} <- findRedeemerSpending (fst m) headIn :: Maybe HS.Input
  pure (Ref.checkFanout (Ref.mkOpsFanout (const True)) (Ref.MkFanout numberOfFanoutOutputs))

-- ── property assembly ─────────────────────────────────────────────────────────────────────────

-- | @reference-rejects ⇒ validator-rejects@; abstains (no constraint) when the reference can't
-- read the tx or accepts.
differential :: ((Tx, UTxO) -> Maybe Bool) -> (Tx, UTxO) -> Property
differential verdict m =
  case verdict m of
    Just False -> validatorAccepts m === False
    _ -> property True

mutated :: (Tx, UTxO) -> SomeMutation -> (Tx, UTxO)
mutated healthy SomeMutation{mutation} = applyMutation mutation healthy

familySpec ::
  String ->
  (Tx, UTxO) ->
  ((Tx, UTxO) -> Gen SomeMutation) ->
  ((Tx, UTxO) -> Maybe Bool) ->
  Spec
familySpec name healthy genMutation verdict = do
  prop (name <> ": reference accepts the healthy tx") $
    verdict healthy === Just True
  prop (name <> ": validator accepts the healthy tx") $
    validatorAccepts healthy === True
  prop (name <> ": reference-reject ⇒ validator-reject") $
    forAll (genMutation healthy) $ \sm ->
      differential verdict (mutated healthy sm)

spec :: Spec
spec = parallel $ do
  familySpec "increment" healthyIncrementTx genIncrementMutation incRefVerdict
  familySpec "decrement" healthyDecrementTx genDecrementMutation decRefVerdict
  familySpec "contest" healthyContestTx genContestMutation contestRefVerdict
  familySpec "fanout" healthyFanoutTx genFanoutMutation fanoutRefVerdict
