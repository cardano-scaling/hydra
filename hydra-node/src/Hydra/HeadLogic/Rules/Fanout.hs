{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Fanout where

import Hydra.Chain (PostChainTx (..))
import Hydra.HeadLogic.Fanout.Selection (
  NextFanoutStep (..),
  currentFanoutStep,
  mkFullFanoutTx,
 )
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit)
import Hydra.HeadLogic.State (
  PartialFanoutState (
    PartialFanoutState,
    confirmedSnapshot,
    contestationDeadline,
    headSeed,
    version
  ),
 )
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx (UTxOType))
import Hydra.Tx.Snapshot (ConfirmedSnapshot, SnapshotVersion)

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
  PartialFanoutState{..} = pfs
