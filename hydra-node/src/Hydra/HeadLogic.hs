{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- | Interpret input workflows and apply their retained events. Rules live in
-- Hydra.HeadLogic.Rules; this module keeps the node and handler entry points.
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
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Error (
  LogicError (..),
  RequirementFailure (..),
  SideLoadRequirementFailure (..),
 )
import Hydra.HeadLogic.Input (Input (..), TTL)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Machine qualified as Machine
import Hydra.HeadLogic.Outcome (
  Effect (..),
  Outcome (..),
  StateChanged (..),
  WaitReason (..),
  cause,
  causes,
  changes,
  newState,
  noop,
  wait,
 )
import Hydra.HeadLogic.Program (AcknowledgementProgram, SnapshotProgram, TransactionProgram)
import Hydra.HeadLogic.Rules.AckSn qualified as AckSn
import Hydra.HeadLogic.Rules.Client (openFinalizedDeposit)
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Environment (RuleEnvironment, ruleEnvironment)
import Hydra.HeadLogic.Rules.Fanout qualified as Fanout
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
import Hydra.HeadLogic.Rules.Recover qualified as Recover
import Hydra.HeadLogic.Rules.Repost qualified as Repost
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Rules.ReqSn qualified as ReqSn
import Hydra.HeadLogic.Rules.ReqTx qualified as ReqTx
import Hydra.HeadLogic.Rules.Settlement qualified as Settlement
import Hydra.HeadLogic.Rules.SnapshotRequest (
  SettlingCommit (..),
  SnapshotRequest (..),
  buildSnapshotRequest,
  chainedSnapshotRequest,
  existingDeposit,
  isLeader,
  nextActiveDepositId,
  nextSnapshotRequest,
  scopedDeposits,
  selectNextDeposit,
  settlingCommitAt,
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
  setChainState,
  snapshotInFlight,
 )
import Hydra.HeadLogic.State.Operations (
  absorbable,
  closedToFanoutProgress,
  eventHeadId,
  fanoutProgressToClosed,
  finalizedDepositTxId,
  headIdOf,
  isReobservation,
  isSubMultisetOf,
  maxTxsPerSnapshot,
  namesNoneOf,
  nextRequestUTxO,
  nullOutputs,
  recordedSelectionMode,
  removeDistributedOutputs,
  retainFinalizedCommit,
  retainFinalizedDecommit,
  sameOutputs,
  seenSnapshotAfterVersionBump,
  settledUTxO,
 )
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.State (ChainPointTime, NodeState (..), PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot, SnapshotNumber, SnapshotVersion)

-- | The node and tools execute the same admission checks and rule programs.
update :: IsChainState tx => Environment -> Ledger tx -> UTCTime -> NodeState tx -> Input tx -> Outcome tx
update env ledger now st input =
  Interpreter.runProgramWith (Interpreter.snapshotConfig env ledger) $ updateProgram (ruleEnvironment env) now st input

updateProgram :: IsChainState tx => RuleEnvironment -> UTCTime -> NodeState tx -> Input tx -> SnapshotProgram tx ()
updateProgram env now st input = Workflow.run $ Input.machine env now st input

-- | Handler entry points for focused tests and integrations. Production uses
-- 'update', including its size and synchronisation checks.
onOpenNetworkReqTx ::
  forall tx.
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  ChainPointTime ->
  OpenState tx ->
  TTL ->
  PendingDeposits tx ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger chainTime st ttl pendingDeposits tx =
  Interpreter.runProgram ledger $ onOpenNetworkReqTxProgram (ruleEnvironment env) chainTime st ttl pendingDeposits tx

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
  TransactionProgram tx ()
onOpenNetworkReqTxProgram env chainTime st ttl pendingDeposits tx =
  Workflow.run $ ReqTx.machine env chainTime st ttl pendingDeposits tx
{-# INLINE onOpenNetworkReqTxProgram #-}

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  forall tx.
  IsChainState tx =>
  Environment ->
  Ledger tx ->
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
  Outcome tx
onOpenNetworkReqSn env ledger pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit =
  Interpreter.runProgramWith (Interpreter.snapshotConfig env ledger) $
    onOpenNetworkReqSnProgram (ruleEnvironment env) pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit

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
  SnapshotProgram tx ()
onOpenNetworkReqSnProgram env pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit =
  Workflow.run $ ReqSn.machine env pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit
{-# INLINE onOpenNetworkReqSnProgram #-}

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- The acknowledgement must refer to the last seen or next expected snapshot. We
-- may wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsChainState tx =>
  Environment ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn env chainTime pendingDeposits st otherParty signature sn =
  Interpreter.runProgramWith Interpreter.basicConfig $
    onOpenNetworkAckSnProgram (ruleEnvironment env) chainTime pendingDeposits st otherParty signature sn

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
  AcknowledgementProgram tx ()
onOpenNetworkAckSnProgram env chainTime pendingDeposits st otherParty signature sn =
  Workflow.run $ AckSn.machine env chainTime pendingDeposits st otherParty signature sn
{-# INLINE onOpenNetworkAckSnProgram #-}

-- | Client request to recover deposited UTxO.
--
-- __Transition__: 'OpenState' → 'OpenState'
-- Client request to recover a deposit by posting a recover transaction on-chain.
-- Works in any head state (Open, Closed, or Idle after fanout). Deposits from a
-- previous head are never cleared from 'pendingDeposits' on fanout, so recovery
-- remains available after a head closes. A new head only sees its own deposits via
-- 'depositsForHead', so old deposits are never accidentally ingested into L2.
-- On-chain, the deposit validator only enforces that the deadline has passed and
-- that the recovered outputs match the originals — it does not require the head to
-- still be active.
onClientRecover ::
  IsTx tx =>
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposit claimed by the retained finalized increment of an open head, if
  -- any (see 'openFinalizedDeposit').
  Maybe (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover currentSlot pendingDeposits finalizedDeposit recoverTxId =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Workflow.run $
      Recover.machine currentSlot pendingDeposits finalizedDeposit recoverTxId

onOpenClientDecommit ::
  IsTx tx =>
  HeadId ->
  Ledger tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  Outcome tx
onOpenClientDecommit hid ledger slot st tx =
  Interpreter.runProgram ledger $
    Workflow.run $
      Decommit.machine hid slot st tx

onOpenNetworkReqDec ::
  IsChainState tx =>
  Environment ->
  Ledger tx ->
  TTL ->
  ChainPointTime ->
  PendingDeposits tx ->
  OpenState tx ->
  tx ->
  Outcome tx
onOpenNetworkReqDec env ledger ttl chainTime pendingDeposits openState decommitTx =
  Interpreter.runProgram ledger $
    Workflow.run $
      ReqDec.machine (ruleEnvironment env) ttl chainTime pendingDeposits openState decommitTx

onOpenChainIncrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Deposit TxId
  TxIdType tx ->
  SnapshotNumber ->
  MultiSignature (Snapshot tx) ->
  Outcome tx
onOpenChainIncrementTx env deposits st cs version txId sn signatures =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Workflow.run $
      Settlement.machine (ruleEnvironment env) deposits st cs version (Settlement.Increment txId) sn signatures

onOpenChainDecrementTx ::
  IsChainState tx =>
  Environment ->
  PendingDeposits tx ->
  OpenState tx ->
  ChainStateType tx ->
  -- | New open state version
  SnapshotVersion ->
  -- | Outputs removed by the decrement
  UTxOType tx ->
  -- | Number of the snapshot the decrement settled
  SnapshotNumber ->
  -- | Multisignature carried by the decrement redeemer
  MultiSignature (Snapshot tx) ->
  Outcome tx
onOpenChainDecrementTx env deposits st cs version utxo sn signatures =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Workflow.run $
      Settlement.machine (ruleEnvironment env) deposits st cs version (Settlement.Decrement utxo) sn signatures

maybeRepostDecrementTx ::
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  Maybe tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  Outcome tx
maybeRepostDecrementTx seed hid params decommit snapshot finalized slot =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Repost.maybeRepostDecrementTx seed hid params decommit snapshot finalized slot

onClosedChainPartialFanoutTx ::
  IsTx tx =>
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Outcome tx
onClosedChainPartialFanoutTx st cs utxo =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Fanout.onClosedChainPartialFanoutTx st cs utxo

onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Outcome tx
onPartialFanoutChainPartialFanoutTx st cs utxo =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Fanout.onPartialFanoutChainPartialFanoutTx st cs utxo

repostFanoutStep :: IsTx tx => PartialFanoutState tx -> Outcome tx
repostFanoutStep st =
  Interpreter.runProgramWith Interpreter.basicConfig $
    Fanout.repostFanoutStep st

aggregateNodeState :: IsChainState tx => NodeState tx -> StateChanged tx -> NodeState tx
aggregateNodeState = Machine.apply

aggregateState ::
  IsChainState tx =>
  NodeState tx ->
  Outcome tx ->
  NodeState tx
aggregateState s outcome =
  foldl' aggregateNodeState s $ collectStateChanged outcome
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
