{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Fanout where

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput qualified as ServerOutput
import Hydra.Chain (PostChainTx (..))
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit)
import Hydra.HeadLogic.Rules.Fanout.Selection (
  NextFanoutStep (..),
  OnChainFanoutDatum (DatumClosed, DatumFanoutProgress),
  computeFullFanoutUTxO,
  currentFanoutStep,
  mkFullFanoutTx,
  nextFanoutStep,
  onChainFanoutDatum,
 )
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    confirmedSnapshot,
    contestationDeadline,
    headId,
    headSeed,
    version
  ),
  FanoutMode (AutoDrain, AwaitingSelection, DistributingSelection),
  HeadState (Closed, FanoutProgress),
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    contestationDeadline,
    distributedOutputs,
    headId,
    headSeed,
    mode,
    remainingOutputs,
    version
  ),
 )
import Hydra.HeadLogic.State.Operations (
  isSubMultisetOf,
  nullOutputs,
  removeDistributedOutputs,
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx (..), UTxOType, withoutUTxO)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, SnapshotVersion)

onClosedClientFanout ::
  IsTx tx =>
  ClosedState tx ->
  Program tx ()
onClosedClientFanout closedState =
  -- A plain 'Fanout' is a full fanout by definition, and is only reachable from
  -- 'Closed'.
  fanoutStepStateChange headId FullFanoutStep fullUTxO
    *> emitFanoutStep FullFanoutStep confirmedSnapshot version headSeed contestationDeadline
 where
  fullUTxO = computeFullFanoutUTxO closedState

  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | The state change that goes with a fanout step, for the callers that own the
-- head state: taking the driver role, or recording the selection being
-- distributed. Decided here once rather than in each handler.
--
-- The selection comes from the step rather than the caller, so a step can only
-- ever be recorded against the set it actually distributes.
fanoutStepStateChange ::
  HeadId ->
  NextFanoutStep tx ->
  -- | The head's full remaining set
  UTxOType tx ->
  Program tx ()
fanoutStepStateChange headId step remainingOutputs =
  case step of
    -- The selection is not distributed as one: it becomes a full fanout, and this
    -- node drives the rest of it.
    FullFanoutStep -> change HeadFanoutInitiated{headId, remainingOutputs}
    -- 'FinalStep' posts a transaction and nothing else, so the selection has to
    -- be recorded for it too: a rollback or restart before that transaction
    -- lands otherwise leaves the driver with no selection to resume from.
    FinalStep{stepDistribute} -> recordSelection stepDistribute
    PartialStep{stepDistribute} -> recordSelection stepDistribute
 where
  recordSelection selection = change HeadPartialFanoutSelected{headId, remainingOutputs, selection}

onClosedClientPartialFanout ::
  IsTx tx =>
  ClosedState tx ->
  UTxOType tx ->
  Program tx ()
onClosedClientPartialFanout closedState selection
  | nullOutputs selection || not (selection `isSubMultisetOf` fullUTxO) =
      emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (Closed closedState)
  | otherwise =
      -- A selection covering the whole head becomes a full fanout - what the user
      -- means by "fan out everything", and the only option for a head holding a
      -- single UTxO, since a non-final batch has to leave one behind.
      fanoutStepStateChange headId step fullUTxO
        *> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- Fresh head: the on-chain datum is still @Closed@.
  step = nextFanoutStep confirmedSnapshot version selection fullUTxO DatumClosed

  fullUTxO = computeFullFanoutUTxO closedState
  ClosedState{headId, confirmedSnapshot, version, headSeed, contestationDeadline} = closedState

-- | Client request to continue a selective partial fanout. Validates the
-- selection against the current 'remainingOutputs' and emits whichever step
-- 'nextFanoutStep' decides on, which is not always a recorded selection: one
-- covering the whole remainder finalizes the head, or, if nothing has been
-- distributed yet, becomes a full fanout that drains it automatically.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutClientPartialFanout ::
  IsTx tx =>
  PartialFanoutState tx ->
  UTxOType tx ->
  Program tx ()
onPartialFanoutClientPartialFanout pfs selection
  | nullOutputs selection || not (selection `isSubMultisetOf` remainingOutputs) =
      emit . ClientEffect $ ServerOutput.CommandFailed (PartialFanout selection) (FanoutProgress pfs)
  | otherwise =
      fanoutStepStateChange headId step remainingOutputs
        *> emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  -- The on-chain datum is only @FanoutProgress@ once a partial fanout has
  -- actually landed (some outputs distributed); until then it is still @Closed@
  -- and a 'FinalPartialFanoutTx' is not yet valid. Compute this the same way as
  -- 'repostFanoutStep' rather than assuming 'True'.
  step = nextFanoutStep confirmedSnapshot version selection remainingOutputs (onChainFanoutDatum distributedOutputs)

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, distributedOutputs} = pfs

-- | Observe a (full or final) fanout transaction, finalizing the head.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Program tx ()
onClosedChainFanoutTx closedState newChainState fanoutUTxO =
  change HeadFannedOut{headId, finalizedOutputs = fanoutUTxO, chainState = newChainState}
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while this node is still 'Closed' — i.e. a partial
-- fanout this node did NOT initiate (another party did). The fanout driver moved
-- to 'PartialFanout' when it issued its 'Fanout'/'PartialFanout' command, so this
-- handler is only reached by passive observers.
--
-- The observer transitions into 'PartialFanout' in 'AwaitingSelection' mode and
-- does __not__ auto-drive the rest: only the driver advances the fanout. This is
-- what makes selective partial fanout work in a multi-party head — observers must
-- not steamroll the remaining UTxO the driver deliberately left.
--
-- __Transition__: 'ClosedState' → 'PartialFanoutState'
onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Program tx ()
onClosedChainPartialFanoutTx closedState newChainState observedDistributed =
  let fullUTxO = computeFullFanoutUTxO closedState
      remaining = removeDistributedOutputs (outputsOfUTxO observedDistributed) fullUTxO
      distributedUTxO = withoutUTxO fullUTxO remaining
   in change
        HeadPartialFannedOut
          { headId
          , distributedOutputs = distributedUTxO
          , remainingOutputs = remaining
          , chainState = newChainState
          , mode = AwaitingSelection
          }
 where
  ClosedState{headId} = closedState

-- | Observe a partial fanout while in 'PartialFanout'. Updates the remaining and
-- distributed sets and, depending on the current 'FanoutMode', either continues
-- draining automatically (or within the active selection) or waits for the next
-- 'PartialFanout' command.
--
-- __Transition__: 'PartialFanoutState' → 'PartialFanoutState'
onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Program tx ()
onPartialFanoutChainPartialFanoutTx pfs newChainState observedDistributed =
  let observedOutputs = outputsOfUTxO observedDistributed
      remaining = removeDistributedOutputs observedOutputs remainingOutputs
      distributedUTxO = withoutUTxO remainingOutputs remaining
      newMode = case mode of
        AutoDrain -> AutoDrain
        AwaitingSelection -> AwaitingSelection
        DistributingSelection sel ->
          let sel' = removeDistributedOutputs observedOutputs sel
           in if nullOutputs sel' then AwaitingSelection else DistributingSelection sel'
      record =
        change
          HeadPartialFannedOut
            { headId
            , distributedOutputs = distributedUTxO
            , remainingOutputs = remaining
            , chainState = newChainState
            , mode = newMode
            }
      -- Already in 'FanoutProgress' on chain, so any continuation may finalize.
      finalize = emitStep (nextFanoutStep confirmedSnapshot version remaining remaining DatumFanoutProgress)
      continue
        -- The head's remaining set is now empty: emit the final (burning) step.
        -- This must happen regardless of mode — otherwise a selection that
        -- drains everything would stop at 'AwaitingSelection' and wedge the
        -- head, never burning the tokens.
        | nullOutputs remaining = finalize
        | otherwise = case newMode of
            AutoDrain -> finalize
            DistributingSelection sel' ->
              emitStep (nextFanoutStep confirmedSnapshot version sel' remaining DatumFanoutProgress)
            AwaitingSelection -> pure ()
   in record *> continue
 where
  emitStep step = emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline

  PartialFanoutState{headId, confirmedSnapshot, version, headSeed, contestationDeadline, remainingOutputs, mode} = pfs

-- | Observe the final fanout while in 'PartialFanout', finalizing the head with
-- the accumulated distributed outputs plus this final batch.
--
-- __Transition__: 'PartialFanoutState' → 'IdleState'
onPartialFanoutChainFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  UTxOType tx ->
  Program tx ()
onPartialFanoutChainFanoutTx pfs newChainState fanoutUTxO =
  change HeadFannedOut{headId, finalizedOutputs = distributedOutputs <> fanoutUTxO, chainState = newChainState}
 where
  PartialFanoutState{headId, distributedOutputs} = pfs

emitFanoutStep ::
  IsTx tx =>
  -- | The step to emit, from 'nextFanoutStep'.
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  Program tx ()
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  case step of
    FinalStep{stepDistribute} ->
      emit
        OnChainEffect
          { postChainTx =
              FinalPartialFanoutTx
                { utxoToDistribute = stepDistribute
                , headSeed
                , contestationDeadline
                }
          }
    FullFanoutStep ->
      emit OnChainEffect{postChainTx = mkFullFanoutTx confirmedSnapshot version headSeed contestationDeadline}
    PartialStep{stepDistribute, stepProof} ->
      emit
        OnChainEffect
          { postChainTx =
              PartialFanoutTx
                { utxoToDistribute = stepDistribute
                , utxoForProof = stepProof
                , headSeed
                , contestationDeadline
                }
          }

-- | Re-post the next fanout step after a chain rollback while in
-- 'FanoutProgress', so the fanout resumes instead of stalling with the
-- rolled-back transaction gone and nothing re-posted. This mirrors the
-- Increment/Decrement re-post on rollback ('maybeRepostIncrementTx' /
-- 'maybeRepostDecrementTx'): it uses the current best-effort bookkeeping and,
-- like those re-posts, assumes the rolled-back transactions re-appear — it does
-- not attempt to reconstruct fanout progress across a divergent rollback (the
-- same limitation the general rollback handling has).
--
-- Which step that is comes from 'currentFanoutStep', the same derivation the
-- revert guard uses to tell this step's failure from a superseded one's.
repostFanoutStep :: IsTx tx => PartialFanoutState tx -> Program tx ()
repostFanoutStep pfs =
  case currentFanoutStep pfs of
    -- Manual mode paused on the user: nothing to re-post, wait for the next
    -- 'PartialFanout' command.
    Nothing -> pure ()
    -- Only the transaction is re-posted: this node is the driver already, and
    -- re-posting is not a new decision to record. So this emits no state
    -- changes, which is what lets 'Hydra.Node.runHydraNode' apply its effects
    -- alone at startup.
    Just step ->
      emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline
 where
  PartialFanoutState{confirmedSnapshot, version, headSeed, contestationDeadline} = pfs
