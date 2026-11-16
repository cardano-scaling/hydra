{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Tick where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, decide)
import Hydra.HeadLogic.Rules.Chain.Deposits (
  onChainTick,
  onOpenChainTick,
 )
import Hydra.HeadLogic.Rules.Chain.Synchronisation (
  handleOutOfSync,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Snapshot.Request (scopedDeposits)
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    contestationDeadline,
    headId,
    readyToFanoutSent
  ),
  HeadState (Open),
 )
import Hydra.HeadLogic.State.Fanout (getClosedState)
import Hydra.Node.State (PendingDeposits, SyncedStatus)
import Hydra.Prelude

-- | Common tick prefix for every head phase. Head-specific work comes last.
handleTick :: IsChainState tx => RuleEnvironment -> UTCTime -> PendingDeposits tx -> HeadState tx -> UTCTime -> ChainPointType tx -> SyncedStatus -> Program tx ()
handleTick env now pendingDeposits st chainTime chainPoint syncStatus = do
  change TickObserved{chainPoint, chainTime}
  handleOutOfSync env now chainPoint chainTime syncStatus
  depositsAtTick <- onChainTick env pendingDeposits chainTime
  case st of
    Open open -> onOpenChainTick env chainTime (scopedDeposits open depositsAtTick) open
    _ | Just ClosedState{contestationDeadline, readyToFanoutSent, headId} <- getClosedState st -> do
      ready <- decide "fanout deadline passed and notification is unsent" (chainTime > contestationDeadline && not readyToFanoutSent)
      when ready $ change HeadIsReadyToFanout{headId}
    _ -> pure ()
