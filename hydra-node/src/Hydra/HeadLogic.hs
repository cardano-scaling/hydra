{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Pure input programs and retained-event aggregation. Runtime entry points
-- and Cardano configuration live in Hydra.Node.HeadLogic.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.State.Operations,
  module Hydra.HeadLogic.Outcome,
  module Hydra.HeadLogic.Rules.Environment,
  module Hydra.HeadLogic.Rules.SnapshotRequest,
  module Hydra.HeadLogic.Rules.Transactions,
  module Hydra.HeadLogic.Rules.Fanout.Selection,
  openFinalizedDeposit,
) where

import Hydra.Chain (ChainStateHistory, initHistory, pushNewState, rollbackHistory, setLastKnown)
import Hydra.Chain.ChainState (IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  newState,
  noop,
 )
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Rules.AckSn qualified as AckSn
import Hydra.HeadLogic.Rules.Client (openFinalizedDeposit)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Fanout.Selection (
  NextFanoutStep (..),
  OnChainFanoutDatum (..),
  computeFullFanoutUTxO,
  currentFanoutStep,
  effectiveCommitDecommit,
  fanoutUTxOFromSnapshot,
  matchesFanoutStep,
  mkFullFanoutTx,
  nextFanoutStep,
  onChainFanoutDatum,
 )
import Hydra.HeadLogic.Rules.Input qualified as Input
import Hydra.HeadLogic.Rules.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SettlingCommit (..),
  SnapshotRequest,
  buildSnapshotRequest,
  chainedSnapshotRequest,
  existingDeposit,
  isLeader,
  nextActiveDepositId,
  nextSnapshotRequest,
  requestDecommitTx,
  requestDepositTxId,
  requestTxs,
  scopedDeposits,
  selectNextDeposit,
  settlingCommitAtPeriod,
 )
import Hydra.HeadLogic.Rules.Transactions (applyTransactionsProgram, applyTransactionsWithoutCollision, checkTransactionOutputs)
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FinalizedSnapshot (..),
  HeadState (..),
  IdleState (..),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  closedStateCBORTag,
  closedStateCBORTagV1,
  coordinatedHeadStateCBORTag,
  coordinatedHeadStateCBORTagV1,
  getChainState,
  getHeadParameters,
  getOpenStateConfirmedSnapshot,
  isCollectingAcks,
  mkSeenSnapshot,
  seenSnapshotNumber,
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  eventHeadId,
  fanoutProgressToClosed,
  finalizedDepositTxId,
  headIdOf,
  isSubMultisetOf,
  maxTxsPerSnapshot,
  namesNoneOf,
  nullOutputs,
  recordedSelectionMode,
  removeDistributedOutputs,
  retainFinalizedCommit,
  retainFinalizedDecommit,
  sameOutputs,
  settledUTxO,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Node.State (ChainPointTime, NodeState (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (TxIdType)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot, SnapshotNumber, SnapshotVersion)

updateProgram :: IsChainState tx => RuleEnvironment -> UTCTime -> NodeState tx -> Input tx -> Program tx ()
updateProgram env now st input = Workflow.run $ Input.machine env now st input

-- | The actual transaction rule as an inspectable program. Interpreters supply
-- ledger application; collision checks, retry decisions, and snapshot selection
-- belong to this rule. The event format remains compatible with existing replay.
onOpenNetworkReqTxProgram ::
  forall tx.
  IsChainState tx =>
  RuleEnvironment ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  tx ->
  Program tx ()
onOpenNetworkReqTxProgram env chainTime st ttl pendingDeposits tx =
  Workflow.run $ ReqTx.machine env chainTime st ttl pendingDeposits tx
{-# INLINE onOpenNetworkReqTxProgram #-}

-- | Snapshot selection, validation order, pruning and effect selection remain in
-- the rule. The interpreter only supplies ledger and cryptographic primitives.
onOpenNetworkReqSnProgram ::
  forall tx.
  IsChainState tx =>
  RuleEnvironment ->
  PendingDeposits tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot version.
  SnapshotVersion ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [TxIdType tx] ->
  -- | Optional decommit transaction of removing funds from the head.
  Maybe tx ->
  -- | Optional commit of additional funds into the head.
  Maybe (TxIdType tx) ->
  Program tx ()
onOpenNetworkReqSnProgram env pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit =
  Workflow.run $ ReqSn.machine env pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit
{-# INLINE onOpenNetworkReqSnProgram #-}

onOpenNetworkAckSnProgram ::
  IsChainState tx =>
  RuleEnvironment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Program tx ()
onOpenNetworkAckSnProgram env chainTime pendingDeposits st otherParty signature sn =
  Workflow.run $ AckSn.machine env chainTime pendingDeposits st otherParty signature sn
{-# INLINE onOpenNetworkAckSnProgram #-}

aggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
aggregateNodeState = Machine.apply

aggregateState ::
  IsChainState tx =>
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState s outcome =
  Machine.applyEvents s $ collectStateChanged outcome
 where
  collectStateChanged :: Outcome tx -> [StateChanged tx]
  collectStateChanged = \case
    Error{} -> []
    Wait{stateChanges} -> stateChanges
    Continue{stateChanges} -> stateChanges

aggregateChainStateHistory :: IsChainState tx => ChainStateHistory tx -> StateChanged tx -> ChainStateHistory tx
aggregateChainStateHistory history = \case
  NetworkConnected -> history
  NetworkDisconnected -> history
  NetworkVersionMismatch{} -> history
  NetworkClusterIDMismatch{} -> history
  NetworkBroadcastStalled{} -> history
  NetworkBroadcastResumed -> history
  PeerConnected{} -> history
  PeerDisconnected{} -> history
  HeadOpened{chainState} -> pushNewState chainState history
  TransactionAppliedToLocalUTxO{} -> history
  SnapshotRequestDecided{} -> history
  SnapshotRequested{} -> history
  TransactionReceived{} -> history
  PartySignedSnapshot{} -> history
  SnapshotConfirmed{} -> history
  DepositRecorded{chainState} -> pushNewState chainState history
  DepositActivated{} -> history
  DepositExpired{} -> history
  DepositRecovered{chainState} -> pushNewState chainState history
  CommitFinalized{chainState} -> pushNewState chainState history
  DecommitRecorded{} -> history
  DecommitFinalized{chainState} -> pushNewState chainState history
  HeadClosed{chainState} -> pushNewState chainState history
  HeadContested{chainState} -> pushNewState chainState history
  HeadIsReadyToFanout{} -> history
  HeadFannedOut{chainState} -> pushNewState chainState history
  HeadPartialFannedOut{chainState} -> pushNewState chainState history
  HeadFanoutInitiated{} -> history
  HeadPartialFanoutSelected{} -> history
  HeadFanoutReverted{} -> history
  ChainRolledBack{chainState} -> rollbackHistory (chainStateSlot chainState) history
  TickObserved{chainPoint} -> setLastKnown chainPoint history
  CommitApproved{} -> history
  DecommitApproved{} -> history
  DecommitInvalid{} -> history
  IgnoredHeadInitializing{} -> history
  TxInvalid{} -> history
  LocalStateCleared{} -> history
  -- FIXME: This makes chain sync starting after rollbacks past the chain state impossible
  Checkpoint nodeState -> initHistory $ getChainState nodeState.headState
  NodeUnsynced{} -> history
  NodeSynced{} -> history
