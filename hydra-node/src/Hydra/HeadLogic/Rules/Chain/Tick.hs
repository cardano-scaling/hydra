{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Tick where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, decide)
import Hydra.HeadLogic.Rules.Chain.Deposits (
  maybeRequestDepositSnapshot,
  updateDepositStates,
 )
import Hydra.HeadLogic.Rules.Chain.Synchronisation (
  handleOutOfSync,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
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
import Hydra.Node.State (PendingDeposits, SyncedStatus, depositsForHeadWithoutFinalizedCommit)
import Hydra.Prelude

-- | Record chain progress, update node-wide status, then handle any work due
-- in the current Head. Protocol deadlines use chain time; wall-clock time is
-- only used to detect whether the node is falling behind the chain.
handleTick ::
  IsChainState tx =>
  RuleEnvironment ->
  -- | Current wall-clock time.
  UTCTime ->
  -- | Tracked deposits, including deposits from previous Heads.
  PendingDeposits tx ->
  -- | Head state before this tick.
  HeadState tx ->
  -- | Time of the observed chain point.
  UTCTime ->
  ChainPointType tx ->
  -- | Sync status before this tick.
  SyncedStatus ->
  Program tx ()
handleTick env now pendingDeposits headState chainTime chainPoint syncStatus = do
  change TickObserved{chainPoint, chainTime}
  handleOutOfSync env now chainPoint chainTime syncStatus
  -- Deposits remain recoverable after their Head closes, so update all of them.
  depositsAtTick <- updateDepositStates env pendingDeposits chainTime
  case headState of
    -- Snapshot selection uses this Head's deposits after activation and expiry.
    Open open ->
      maybeRequestDepositSnapshot
        env
        chainTime
        (depositsForHeadWithoutFinalizedCommit open depositsAtTick)
        open
    -- A pending local fanout still counts as Closed until observed on chain.
    _ -> forM_ (getClosedState headState) notifyReadyToFanout
 where
  notifyReadyToFanout ClosedState{..} = do
    shouldNotify <-
      decide
        "fanout deadline passed and notification is unsent"
        (chainTime > contestationDeadline && not readyToFanoutSent)

    when shouldNotify $ change HeadIsReadyToFanout{headId}
