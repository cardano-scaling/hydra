{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Synchronisation where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program, change)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (RuleEnvironment, unsyncedPeriod),
 )
import Hydra.Node.State (SyncedStatus (..))
import Hydra.Node.UnsyncedPeriod (UnsyncedPeriod (..))
import Hydra.Prelude

handleOutOfSync ::
  IsChainState tx =>
  RuleEnvironment ->
  -- | Current system time
  UTCTime ->
  -- | Latest Chain point observed
  ChainPointType tx ->
  -- | Latest Chain point time representation observed
  UTCTime ->
  SyncedStatus ->
  Program tx ()
handleOutOfSync RuleEnvironment{unsyncedPeriod} now chainPoint chainTime syncStatus =
  -- Emit only on an actual sync-status transition, rather than on every tick, so
  -- clients are not flooded (see issue #2749). The continuous drift value is
  -- exposed as a metric ('hydra_chain_drift_seconds') instead.
  case (syncStatus, newSyncStatus) of
    (InSync, CatchingUp) -> change NodeUnsynced{chainSlot, chainTime, drift}
    (CatchingUp, InSync) -> change NodeSynced{chainSlot, chainTime, drift}
    _ -> pure ()
 where
  plus = flip addUTCTime
  chainSlot = chainPointSlot chainPoint

  threshold = unsyncedPeriodToNominalDiffTime unsyncedPeriod
  drift = now `diffUTCTime` chainTime

  -- We consider the node out of sync when:
  -- the last observed chainTime plus the delta allowed by the unsyncedPeriod (threshold)
  -- falls behind the current system time (now).
  -- NOTE: this is the same as drift > threshold
  nodeOutOfSync = chainTime `plus` threshold < now
  newSyncStatus = if nodeOutOfSync then CatchingUp else InSync
