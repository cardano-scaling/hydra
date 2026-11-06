{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Deposits where

import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, decide, emit)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (
    RuleEnvironment,
    depositActivation,
    depositPeriod,
    party
  ),
 )
import Hydra.HeadLogic.Rules.SnapshotRequest (
  DecommitStatus (ReadyToDecommit),
  SettlingCommit (ContinueCommit, DropCommit),
  SnapshotRequest (
    SnapshotRequest,
    requestDecommitTx,
    requestDepositTxId,
    requestTxs
  ),
  decommitStatus,
  isLeader,
  nextSnapshotRequest,
  scopedDeposits,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Rules.Synchronisation (handleOutOfSync)
import Hydra.HeadLogic.State (
  ClosedState (
    ClosedState,
    contestationDeadline,
    headId,
    readyToFanoutSent
  ),
  CoordinatedHeadState (
    CoordinatedHeadState,
    confirmedSnapshot,
    currentDepositTxId,
    decommitTx,
    localTxs,
    seenSnapshot,
    version
  ),
  HeadState (Closed, Open),
  OpenState (OpenState, coordinatedHeadState, parameters),
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (settledUTxO)
import Hydra.Network.Message (Message (..))
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, SyncedStatus)
import Hydra.Prelude
import Hydra.Tx (IsTx, TxIdType, txId)
import Hydra.Tx.DepositPeriod (toNominalDiffTime)
import Hydra.Tx.Snapshot (Snapshot (..), getSnapshot)

determineNextDepositStatus :: RuleEnvironment -> PendingDeposits tx -> UTCTime -> PendingDeposits tx
determineNextDepositStatus env pendingDeposits chainTime =
  (\deposit -> deposit{status = determineStatus deposit}) <$> pendingDeposits
 where
  determineStatus Deposit{created, deadline}
    | chainTime > deadline `minusTime` toNominalDiffTime depositPeriod = Expired
    | chainTime > created `plusTime` toNominalDiffTime depositActivation = Active
    | otherwise = Inactive

  minusTime time dt = addUTCTime (-dt) time

  plusTime = flip addUTCTime

  RuleEnvironment{depositPeriod, depositActivation} = env

-- | Process the chain (and time) advancing in any head state.
--
-- __Transition__: 'AnyState' → 'AnyState'
--
-- This is primarily used to track deposits status changes.
onChainTick :: IsTx tx => RuleEnvironment -> PendingDeposits tx -> UTCTime -> Program tx ledger signing ()
onChainTick env pendingDeposits chainTime =
  mkDepositActivated newActive *> mkDepositExpired newExpired
 where
  -- XXX: This is a bit messy
  newActive = Map.difference nextActive pendingActive

  newExpired = Map.difference nextExpired pendingExpired

  pendingActive = Map.filter (\Deposit{status} -> status == Active) pendingDeposits

  pendingExpired = Map.filter (\Deposit{status} -> status == Expired) pendingDeposits

  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  nextActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  nextExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  mkDepositActivated m = forM_ (Map.toAscList m) $ \(depositTxId, deposit) ->
    change DepositActivated{depositTxId, chainTime, deposit}

  mkDepositExpired m = forM_ (Map.toAscList m) $ \(depositTxId, deposit) ->
    change DepositExpired{depositTxId, chainTime, deposit}

-- | Process the chain (and time) advancing in an open head.
--
-- __Transition__: 'OpenState' → 'OpenState'
--
-- This is primarily used to track deposits and either drop them or request
-- snapshots for inclusion.
onOpenChainTick :: forall tx ledger signing. IsChainState tx => RuleEnvironment -> UTCTime -> PendingDeposits tx -> OpenState tx -> Program tx ledger signing ()
onOpenChainTick env chainTime pendingDeposits st
  | snapshotInFlight seenSnapshot || not (isLeader parameters party nextSn) = pure ()
  | otherwise =
      let settling = settlingCommitAtPeriod env.depositPeriod version chainTime deposits confirmed
          built = nextSnapshotRequest (settledUTxO version (getSnapshot confirmedSnapshot)) localTxs deposits currentDepositTxId decommit settling
       in case (settling, built) of
            -- The commit is still settling; requests carrying txs continue it.
            (Just (ContinueCommit _), _) -> pure ()
            -- The commit can no longer land: drop it so other deposits can be claimed.
            (Just DropCommit, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId}) -> request requestTxs requestDecommitTx requestDepositTxId
            -- Spec: wait txω = ⊥ ∧ Uα = ∅.
            (Nothing, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId = Just depositTxId})
              | isNothing decommitTx -> request requestTxs requestDecommitTx (Just depositTxId)
            (Nothing, SnapshotRequest{requestTxs, requestDecommitTx, requestDepositTxId})
              | ReadyToDecommit{} <- decommit -> request requestTxs requestDecommitTx requestDepositTxId
            _ -> pure ()
 where
  decommit = decommitStatus version confirmed decommitTx
  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  -- All deposits, with their status updated to this tick's time. Empty
  -- deposits are ignored, because an empty increment cannot be posted.
  deposits = Map.filter (\Deposit{deposited} -> deposited /= mempty) (newActive <> newExpired <> pendingDeposits)

  request :: [tx] -> Maybe tx -> Maybe (TxIdType tx) -> Program tx ledger signing ()
  request txs mDecommitTx mDepositTxId =
    void (Coordinated.stage (Coordinated.start st) $ Coordinated.requestSnapshot nextSn)
      *> emit (NetworkEffect $ ReqSn version nextSn (txId <$> txs) mDecommitTx mDepositTxId)

  confirmed@Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

  nextSn = confirmedSn + 1

  RuleEnvironment{party} = env

  CoordinatedHeadState
    { localTxs
    , confirmedSnapshot
    , seenSnapshot
    , version
    , decommitTx
    , currentDepositTxId
    } = coordinatedHeadState

  OpenState{coordinatedHeadState, parameters} = st

-- | Common tick prefix for every head phase. Head-specific work comes last.
handleTick :: IsChainState tx => RuleEnvironment -> UTCTime -> PendingDeposits tx -> HeadState tx -> UTCTime -> ChainPointType tx -> SyncedStatus -> Program tx ledger signing ()
handleTick env now pendingDeposits st chainTime chainPoint syncStatus = do
  change TickObserved{chainPoint, chainTime}
  handleOutOfSync env now chainPoint chainTime syncStatus
  onChainTick env pendingDeposits chainTime
  case st of
    Open open -> onOpenChainTick env chainTime (scopedDeposits open pendingDeposits) open
    Closed ClosedState{contestationDeadline, readyToFanoutSent, headId} -> do
      ready <- decide "fanout deadline passed and notification is unsent" (chainTime > contestationDeadline && not readyToFanoutSent)
      when ready $ change HeadIsReadyToFanout{headId}
    _ -> pure ()
