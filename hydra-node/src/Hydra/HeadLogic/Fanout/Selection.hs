{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Fanout.Selection where

import Hydra.Chain (PostChainTx (..))
import Hydra.HeadLogic.Fanout.Outputs (nullOutputs, sameOutputs)
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, confirmedSnapshot, version),
  FanoutMode (AutoDrain, AwaitingSelection, DistributingSelection),
  OnChainFanoutDatum (..),
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    contestationDeadline,
    fanoutDatum,
    headSeed,
    mode,
    remainingOutputs,
    version
  ),
 )
import Hydra.Prelude
import Hydra.Tx (HeadSeed, IsTx, UTxOType, combinedUTxO)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot (..),
  Snapshot (..),
  SnapshotVersion,
  getSnapshot,
 )

data NextFanoutStep tx
  = -- | The target is the whole remainder and the head is already in
    -- @FanoutProgress@ on chain: the final step, distributing the rest and
    -- burning the head tokens.
    FinalStep {stepDistribute :: UTxOType tx}
  | -- | The target is the whole remainder but the head output still carries the
    -- @Closed@ datum, which the final step is not valid against. Posted as a
    -- non-final step it would empty the head, which 'mustNotBeLastBatch' rejects,
    -- so the chunk search settles for one output less and the head needs a second
    -- transaction to finish — and cannot be drained at all when a single output
    -- is left. Covering everything is a full fanout, so post that (#2855).
    FullFanoutStep
  | -- | The target is a strict subset: a non-final chunk distributing it, proved
    -- against the set the head's current datum commits to.
    PartialStep {stepDistribute :: UTxOType tx, stepProof :: UTxOType tx}

nextFanoutStep ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  -- | Chunk source for the next step (the user selection, or the whole remaining
  --   set when auto-draining)
  UTxOType tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  OnChainFanoutDatum ->
  NextFanoutStep tx
nextFanoutStep confirmedSnapshot version target remaining onChainDatum
  | target `sameOutputs` remaining =
      case onChainDatum of
        DatumFanoutProgress -> FinalStep{stepDistribute = remaining}
        DatumClosed -> FullFanoutStep
  | otherwise = PartialStep{stepDistribute = target, stepProof = proofSet}
 where
  -- The set the on-chain datum commits to: the fan-out-able set from a @Closed@
  -- head, and the not-yet-distributed set once the head is in @FanoutProgress@,
  -- since every step removes exactly what it distributed.
  proofSet = case onChainDatum of
    DatumClosed -> fanoutUTxOFromSnapshot confirmedSnapshot version
    DatumFanoutProgress -> remaining

-- | Given the on-chain @version@ and a snapshot's own version, decide which of a
-- pending commit / decommit is still to be distributed on fanout. Equal versions
-- mean settlement has not advanced the Head: exclude an unabsorbed commit and
-- include an unpaid decommit. Once settlement advances the version, include the
-- absorbed commit and exclude the paid decommit. Shared by 'mkFullFanoutTx' and
-- 'fanoutUTxOFromSnapshot'.
effectiveCommitDecommit ::
  -- | On-chain version
  SnapshotVersion ->
  -- | Snapshot version
  SnapshotVersion ->
  -- | Pending commit
  Maybe (UTxOType tx) ->
  -- | Pending decommit
  Maybe (UTxOType tx) ->
  (Maybe (UTxOType tx), Maybe (UTxOType tx))
effectiveCommitDecommit onChainVersion snapshotVersion utxoToCommit utxoToDecommit
  | snapshotVersion == onChainVersion = (Nothing, utxoToDecommit)
  | otherwise = (utxoToCommit, Nothing)

-- | Build the full automatic 'FanoutTx' from a confirmed snapshot at the given
-- on-chain version. Shared by 'onClosedClientFanout' and the rollback re-post in
-- 'FanoutProgress' ('repostFanoutStep').
mkFullFanoutTx ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  PostChainTx tx
mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline =
  FanoutTx
    { utxo
    , utxoToCommit = effectiveCommit
    , utxoToDecommit = effectiveDecommit
    , headSeed
    , contestationDeadline
    }
 where
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot

computeFullFanoutUTxO ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx
computeFullFanoutUTxO ClosedState{confirmedSnapshot, version} =
  fanoutUTxOFromSnapshot confirmedSnapshot version

-- | The fan-out-able UTxO of a confirmed snapshot at the given on-chain version:
-- the snapshot UTxO plus a pending commit (if the increment landed on chain) or a
-- pending decommit (if the decrement has not landed yet).
fanoutUTxOFromSnapshot ::
  IsTx tx =>
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  UTxOType tx
fanoutUTxOFromSnapshot confirmedSnapshot version =
  combinedUTxO utxo effectiveCommit effectiveDecommit
 where
  Snapshot{utxo, utxoToCommit, utxoToDecommit, version = snapshotVersion} = getSnapshot confirmedSnapshot
  (effectiveCommit, effectiveDecommit) = effectiveCommitDecommit version snapshotVersion utxoToCommit utxoToDecommit

-- | The step this node is currently driving. 'Nothing' while a manual fanout
-- waits on the next selection, where nothing is pending.
currentFanoutStep :: IsTx tx => PartialFanoutState tx -> Maybe (NextFanoutStep tx)
currentFanoutStep PartialFanoutState{confirmedSnapshot, version, remainingOutputs, fanoutDatum, mode}
  | fanoutDatum == DatumFanoutProgress && nullOutputs remainingOutputs = Nothing
  | otherwise = case mode of
      AwaitingSelection -> Nothing
      AutoDrain -> Just (step remainingOutputs)
      DistributingSelection selection
        | nullOutputs selection -> Nothing
        | otherwise -> Just (step selection)
 where
  step target = nextFanoutStep confirmedSnapshot version target remainingOutputs fanoutDatum

-- | Whether a posted transaction is the one a given step posts, telling a failure
-- of the step this node is driving from one it has already moved past.
--
-- Compares the distributed set, not just the transaction shape, since two
-- selections can both be non-final steps. By content, because naming the same
-- outputs under other inputs is a valid way to ask for the same set.
matchesFanoutStep :: IsTx tx => PartialFanoutState tx -> NextFanoutStep tx -> PostChainTx tx -> Bool
matchesFanoutStep PartialFanoutState{confirmedSnapshot, version, headSeed, contestationDeadline} step postChainTx =
  case (step, postChainTx) of
    -- A contest can replace the deadline or snapshot before a failure arrives.
    (FullFanoutStep, FanoutTx{}) -> postChainTx == mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline
    (FinalStep{stepDistribute}, FinalPartialFanoutTx{utxoToDistribute, headSeed = postedSeed, contestationDeadline = postedDeadline}) ->
      sameContext postedSeed postedDeadline && utxoToDistribute `sameOutputs` stepDistribute
    (PartialStep{stepDistribute, stepProof}, PartialFanoutTx{utxoToDistribute, utxoForProof, headSeed = postedSeed, contestationDeadline = postedDeadline}) ->
      sameContext postedSeed postedDeadline && utxoToDistribute `sameOutputs` stepDistribute && utxoForProof `sameOutputs` stepProof
    _ -> False
 where
  sameContext postedSeed postedDeadline = postedSeed == headSeed && postedDeadline == contestationDeadline

-- | The mode a recorded selection leaves the driver in. A selection covering the
-- whole remainder with nothing distributed yet is not distributed as a selection
-- at all: 'nextFanoutStep' routes it to a full fanout and the driver auto-drains
-- the rest, so that is the mode it has to be recorded under.
--
-- 'fanoutStepStateChange' never pairs the two - such a step is a
-- 'FullFanoutStep', recorded as 'HeadFanoutInitiated' - so this only normalises
-- a selection recorded before that routing existed and replayed here. Without it
-- the re-posts ('repostFanoutStep') would post the full fanout that
-- 'nextFanoutStep' decides on while the mode kept claiming a selection is being
-- distributed, which is what every 'HeadPartiallyFannedOut' then reports.
recordedSelectionMode ::
  IsTx tx =>
  -- | Observed on-chain datum
  OnChainFanoutDatum ->
  -- | The head's full remaining set
  UTxOType tx ->
  -- | The recorded selection
  UTxOType tx ->
  FanoutMode tx
recordedSelectionMode datum remaining selection
  | datum == DatumClosed && selection `sameOutputs` remaining = AutoDrain
  | otherwise = DistributingSelection selection
