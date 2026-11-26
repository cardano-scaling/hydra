{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Chain.Tick where

import Data.Map.Strict qualified as Map (elems)
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Outcome (Effect (NetworkRetransmission), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, emit, signSnapshot, traceB)
import Hydra.HeadLogic.Rules.Chain.Deposits (
  maybeRequestDepositSnapshot,
  updateDepositStates,
 )
import Hydra.HeadLogic.Rules.Chain.Synchronisation (
  handleOutOfSync,
 )
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment (party))
import Hydra.HeadLogic.Snapshot.Cached qualified as Cached
import Hydra.HeadLogic.Snapshot.Request (isLeader)
import Hydra.HeadLogic.Snapshot.Settlement (matchesSnapshotDecommit)
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    contestationDeadline,
    headId,
    readyToFanoutSent
  ),
  CoordinatedHeadState (..),
  HeadState (Open),
  OpenState (..),
  SeenSnapshot (..),
 )
import Hydra.HeadLogic.State.Fanout (getClosedState)
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (PendingDeposits, SyncedStatus (..), depositsForHeadWithoutFinalizedCommit)
import Hydra.Prelude
import Hydra.Tx (IsTx, txId)
import Hydra.Tx.Snapshot (Snapshot (..))

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
    Open open -> do
      case (syncStatus, open.coordinatedHeadState.seenSnapshot) of
        (InSync, SeenSnapshot{cachedSnapshot})
          | let snapshot = Cached.snapshot cachedSnapshot
          , isLeader open.parameters env.party snapshot.number
          , snapshot.version == open.coordinatedHeadState.version ->
              case snapshot.utxoToDecommit of
                Nothing -> retransmit snapshot Nothing
                Just _ ->
                  forM_
                    ( find (matchesSnapshotDecommit snapshot) open.coordinatedHeadState.decommitTx
                        <|> find (matchesSnapshotDecommit snapshot) (Map.elems open.coordinatedHeadState.allTxs)
                    )
                    (retransmit snapshot . Just)
        _ -> pure ()
      maybeRequestDepositSnapshot
        env
        chainTime
        (depositsForHeadWithoutFinalizedCommit open depositsAtTick)
        open
    -- A pending local fanout still counts as Closed until observed on chain.
    _ -> forM_ (getClosedState headState) notifyReadyToFanout
 where
  -- Headers pin bodies before receipt and retry validation after the batch.
  retransmit :: IsTx tx => Snapshot tx -> Maybe tx -> Program tx ()
  retransmit snapshot decommit = do
    signed <- signSnapshot snapshot
    let header = ReqSn snapshot.version snapshot.number (txId <$> snapshot.confirmed) decommit snapshot.depositTxId
    emit $ NetworkRetransmission (header : (ReqTx <$> snapshot.confirmed) <> [header, AckSn signed snapshot.number])

  notifyReadyToFanout ClosedState{..} = do
    shouldNotify <-
      traceB
        "fanout deadline passed and notification is unsent"
        (chainTime > contestationDeadline && not readyToFanoutSent)

    when shouldNotify $ change HeadIsReadyToFanout{headId}
