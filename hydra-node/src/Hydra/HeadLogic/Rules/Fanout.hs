{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Fanout where

import Hydra.HeadLogic.Fanout.Selection (
  NextFanoutStep (..),
  currentFanoutStep,
  fanoutTransaction,
 )
import Hydra.HeadLogic.Outcome (Effect (..))
import Hydra.HeadLogic.Program (Program, emit)
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
import Hydra.Tx (HeadSeed, IsTx)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, SnapshotVersion)

emitFanoutStep ::
  IsTx tx =>
  NextFanoutStep tx ->
  ConfirmedSnapshot tx ->
  SnapshotVersion ->
  HeadSeed ->
  UTCTime ->
  Program tx ()
emitFanoutStep step confirmedSnapshot version headSeed contestationDeadline =
  emit $
    OnChainEffect $
      fanoutTransaction step confirmedSnapshot version headSeed contestationDeadline

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
