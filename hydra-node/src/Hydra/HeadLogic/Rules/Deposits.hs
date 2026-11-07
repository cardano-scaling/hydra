{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.Rules.Deposits where

import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Machine.Coordinated qualified as Coordinated
import Hydra.HeadLogic.Outcome (StateChanged (..))
import Hydra.HeadLogic.Program (Program, change, decide)
import Hydra.HeadLogic.Rules.Environment (
  RuleEnvironment (
    RuleEnvironment,
    depositActivation,
    depositPeriod,
    party
  ),
 )
import Hydra.HeadLogic.Rules.SendSnapshot (sendSnapshot)
import Hydra.HeadLogic.Rules.SnapshotRequest (
  DecommitStatus (ReadyToDecommit),
  SettlingCommit (ContinueCommit, DropCommit),
  decommitStatus,
  isLeader,
  nextSnapshotRequest,
  requestDepositTxId,
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
import Hydra.Node.State (Deposit (..), DepositStatus (..), PendingDeposits, SyncedStatus)
import Hydra.Prelude
import Hydra.Tx (IsTx)
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
          built = nextSnapshotRequest version confirmed localTxs deposits currentDepositTxId decommit settling
       in case (settling, built) of
            -- The commit is still settling; requests carrying txs continue it.
            (Just (ContinueCommit _), _) -> pure ()
            -- The commit can no longer land: drop it so other deposits can be claimed.
            (Just DropCommit, Just request) -> sendSnapshot (Coordinated.start st) request
            -- Spec: wait txω = ⊥ ∧ Uα = ∅.
            (Nothing, Just request)
              | isJust (requestDepositTxId request), isNothing decommitTx -> sendSnapshot (Coordinated.start st) request
            (Nothing, Just request)
              | ReadyToDecommit{} <- decommit -> sendSnapshot (Coordinated.start st) request
            _ -> pure ()
 where
  decommit = decommitStatus version confirmed decommitTx
  nextDeposits = determineNextDepositStatus env pendingDeposits chainTime

  newActive = Map.filter (\Deposit{status} -> status == Active) nextDeposits

  newExpired = Map.filter (\Deposit{status} -> status == Expired) nextDeposits

  -- All deposits, with their status updated to this tick's time. Empty
  -- deposits are ignored, because an empty increment cannot be posted.
  deposits = Map.filter (\Deposit{deposited} -> deposited /= mempty) (newActive <> newExpired <> pendingDeposits)

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
