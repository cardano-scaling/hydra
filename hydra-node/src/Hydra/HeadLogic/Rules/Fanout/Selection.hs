{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Fanout.Selection where

import Hydra.Chain (PostChainTx (..))
import Hydra.HeadLogic.State (
  ClosedState (ClosedState, confirmedSnapshot, version),
  FanoutMode (AutoDrain, AwaitingSelection, DistributingSelection),
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    distributedOutputs,
    mode,
    remainingOutputs,
    version
  ),
 )
import Hydra.HeadLogic.State.Operations (
  nullOutputs,
  sameOutputs,
 )
import Hydra.Prelude
import Hydra.Tx (HeadSeed, IsTx, UTxOType, combinedUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotVersion, getSnapshot)

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

-- | Decide the next fanout step. The multiset comparison here is the only one
-- needed: callers pass the result on rather than re-deriving it.
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
-- pending commit / decommit is still to be distributed on fanout. When the
-- increment has landed on chain (versions match) the commit was already applied
-- (drop it) while a pending decommit still applies; otherwise the commit still
-- applies and the decommit was already paid out. Centralises the version check
-- shared by 'mkFullFanoutTx' and 'fanoutUTxOFromSnapshot'.
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
currentFanoutStep PartialFanoutState{confirmedSnapshot, version, remainingOutputs, distributedOutputs, mode} =
  case mode of
    AwaitingSelection -> Nothing
    AutoDrain -> Just (step remainingOutputs)
    DistributingSelection selection -> Just (step selection)
 where
  step target = nextFanoutStep confirmedSnapshot version target remainingOutputs onChainDatum

  onChainDatum = onChainFanoutDatum distributedOutputs

-- | Whether a posted transaction is the one a given step posts, telling a failure
-- of the step this node is driving from one it has already moved past.
--
-- Compares the distributed set, not just the transaction shape, since two
-- selections can both be non-final steps. By content, because naming the same
-- outputs under other inputs is a valid way to ask for the same set.
matchesFanoutStep :: IsTx tx => NextFanoutStep tx -> PostChainTx tx -> Bool
matchesFanoutStep step postChainTx =
  case (step, postChainTx) of
    -- Only one full fanout is ever in flight: it covers the whole head.
    (FullFanoutStep, FanoutTx{}) -> True
    (FinalStep{stepDistribute}, FinalPartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    (PartialStep{stepDistribute}, PartialFanoutTx{utxoToDistribute}) -> utxoToDistribute `sameOutputs` stepDistribute
    _ -> False

-- | Which on-chain head datum the next fanout step will be posted against:
-- still @Closed@ (no partial fanout has landed yet) or already @FanoutProgress@.
-- A 'FinalPartialFanoutTx' (which burns the head tokens) is only valid once the
-- datum is 'DatumFanoutProgress'.
data OnChainFanoutDatum = DatumClosed | DatumFanoutProgress

-- | The on-chain datum implied by how much has been distributed so far: still
-- @Closed@ while nothing has landed, @FanoutProgress@ once some has.
onChainFanoutDatum :: IsTx tx => UTxOType tx -> OnChainFanoutDatum
onChainFanoutDatum distributed
  | nullOutputs distributed = DatumClosed
  | otherwise = DatumFanoutProgress
