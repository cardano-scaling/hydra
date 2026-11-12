{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Pure input programs and retained-event aggregation. Runtime entry points
-- and Cardano configuration live in Hydra.Node.HeadLogic.
module Hydra.HeadLogic (
  module Hydra.HeadLogic,
  aggregateChainStateHistory,
  module Hydra.HeadLogic.Input,
  module Hydra.HeadLogic.Error,
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Snapshot.Settlement,
  module Hydra.HeadLogic.State.Fanout,
  module Hydra.HeadLogic.Fanout.Outputs,
  module Hydra.HeadLogic.Transaction.Outputs,
  module Hydra.HeadLogic.Outcome,
  module Hydra.HeadLogic.Rules.Environment,
  module Hydra.HeadLogic.Snapshot.Request,
  module Hydra.HeadLogic.Rules.Transactions,
  module Hydra.HeadLogic.Fanout.Selection,
  openFinalizedDeposit,
) where

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Fanout.Outputs (isSubMultisetOf, nullOutputs, removeDistributedOutputs, sameOutputs)
import Hydra.HeadLogic.Fanout.Selection (NextFanoutStep (..), computeFullFanoutUTxO, currentFanoutStep, effectiveCommitDecommit, fanoutUTxOFromSnapshot, matchesFanoutStep, mkFullFanoutTx, nextFanoutStep, recordedSelectionMode)
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Outcome (Effect (..), Outcome (..), StateChanged (..), WaitReason (..), cause, eventHeadId, newState, noop)
import Hydra.HeadLogic.Program (Program)
import Hydra.HeadLogic.Rules.Client (openFinalizedDeposit)
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment)
import Hydra.HeadLogic.Rules.Input qualified as Input
import Hydra.HeadLogic.Rules.Transactions (applyTransactionsProgram, applyTransactionsWithoutCollision, checkTransactionOutputs)
import Hydra.HeadLogic.Snapshot.Request (SettlingCommit (..), SnapshotRequest, buildSnapshotRequest, chainedSnapshotRequest, existingDeposit, isLeader, maxTxsPerSnapshot, nextActiveDepositId, nextSnapshotRequest, requestDecommitTx, requestDepositTxId, requestTxs, scopedDeposits, selectNextDeposit, settlingCommitAtPeriod)
import Hydra.HeadLogic.Snapshot.Settlement (finalizedDepositTxId, retainFinalizedCommit, retainFinalizedDecommit, settledUTxO)
import Hydra.HeadLogic.State (ClosedState (..), CoordinatedHeadState (..), FanoutMode (..), FinalizedSnapshot (..), FinalizedState (..), HeadState (..), IdleState (..), OnChainFanoutDatum (..), OpenState (..), PartialFanoutState (..), SeenSnapshot (..), closedStateCBORTag, closedStateCBORTagV1, coordinatedHeadStateCBORTag, coordinatedHeadStateCBORTagV1, getChainState, getHeadParameters, getOpenStateConfirmedSnapshot, headIdOf, isCollectingAcks, mkSeenSnapshot, seenSnapshotNumber, snapshotInFlight)
import Hydra.HeadLogic.State.ChainHistory (aggregateChainStateHistory)
import Hydra.HeadLogic.State.Fanout (fanoutProgressToClosed)
import Hydra.HeadLogic.State.Node qualified as Machine
import Hydra.HeadLogic.Transaction.Outputs (absorbable, namesNoneOf)
import Hydra.Node.State (ChainPointTime, NodeState (..), PendingDeposits)
import Hydra.HeadLogic.Rules.Network.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Rules.Network.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.Network.AckSn qualified as AckSn
import Hydra.Tx (TxIdType)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (Snapshot, SnapshotNumber, SnapshotVersion)
import Hydra.Prelude

updateProgram :: IsChainState tx => RuleEnvironment -> UTCTime -> NodeState tx -> Input tx -> Program tx ()
updateProgram = Input.handle

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
onOpenNetworkReqTxProgram = ReqTx.handle
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
onOpenNetworkReqSnProgram = ReqSn.handle
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
onOpenNetworkAckSnProgram = AckSn.handle
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
