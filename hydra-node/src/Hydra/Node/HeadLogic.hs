{-# LANGUAGE DuplicateRecordFields #-}

-- | Cardano runtime wiring and compatibility entry points for the pure core.
module Hydra.Node.HeadLogic (
  module Hydra.Node.HeadLogic,
  module Hydra.HeadLogic,
  module Hydra.Node.HeadLogic.Configuration,
) where

import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.HeadLogic
import Hydra.HeadLogic.Cardano (cardanoSnapshotLimits)
import Hydra.HeadLogic.Interpreter qualified as Interpreter
import Hydra.HeadLogic.Rules.Decommit qualified as Decommit
import Hydra.HeadLogic.Rules.Fanout qualified as Fanout
import Hydra.HeadLogic.Rules.Recover qualified as Recover
import Hydra.HeadLogic.Rules.Repost qualified as Repost
import Hydra.HeadLogic.Rules.ReqDec qualified as ReqDec
import Hydra.HeadLogic.Rules.Settlement qualified as Settlement
import Hydra.HeadLogic.Workflow qualified as Workflow
import Hydra.Ledger (Ledger)
import Hydra.Node.Environment (Environment)
import Hydra.Node.HeadLogic.Configuration
import Hydra.Node.State (ChainPointTime, NodeState, PendingDeposits)
import Hydra.Prelude
import Hydra.Tx (HeadId, HeadSeed, IsTx, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.HeadParameters (HeadParameters)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot, SnapshotNumber, SnapshotVersion)

-- | The node and tools execute the same admission checks and rule programs.
update :: IsChainState tx => Environment -> Ledger tx -> UTCTime -> NodeState tx -> Input tx -> Outcome tx
update env ledger now st input =
  Interpreter.runProgram (interpreterConfig env ledger) $ updateProgram (ruleEnvironment env) now st input

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
  Interpreter.runProgram (interpreterConfig env ledger) $ onOpenNetworkReqTxProgram (ruleEnvironment env) chainTime st ttl pendingDeposits tx

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
  Interpreter.runProgram (interpreterConfig env ledger) $
    onOpenNetworkReqSnProgram (ruleEnvironment env) pendingDeposits chainTime st ttl otherParty sv sn txs decommit deposit

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
  Interpreter.InterpreterConfig tx ->
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
onOpenNetworkAckSn config env chainTime pendingDeposits st otherParty signature sn =
  Interpreter.runProgram config $
    onOpenNetworkAckSnProgram (ruleEnvironment env) chainTime pendingDeposits st otherParty signature sn

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
  Interpreter.InterpreterConfig tx ->
  ChainSlot ->
  PendingDeposits tx ->
  -- | Deposit claimed by the retained finalized increment of an open head, if
  -- any (see 'openFinalizedDeposit').
  Maybe (TxIdType tx) ->
  TxIdType tx ->
  Outcome tx
onClientRecover config currentSlot pendingDeposits finalizedDeposit recoverTxId =
  Interpreter.runProgram config $
    Workflow.run $
      Recover.machine currentSlot pendingDeposits finalizedDeposit recoverTxId

onOpenClientDecommit ::
  IsTx tx =>
  HeadId ->
  Interpreter.InterpreterConfig tx ->
  ChainSlot ->
  CoordinatedHeadState tx ->
  -- | Decommit transaction.
  tx ->
  Outcome tx
onOpenClientDecommit hid config slot st tx =
  Interpreter.runProgram config $
    Workflow.run $
      Decommit.machine cardanoSnapshotLimits hid slot st tx

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
  Interpreter.runProgram (interpreterConfig env ledger) $
    Workflow.run $
      ReqDec.machine (ruleEnvironment env) ttl chainTime pendingDeposits openState decommitTx

onOpenChainIncrementTx ::
  IsChainState tx =>
  Interpreter.InterpreterConfig tx ->
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
onOpenChainIncrementTx config env deposits st cs version txId sn signatures =
  Interpreter.runProgram config $
    Workflow.run $
      Settlement.machine (ruleEnvironment env) deposits st cs version (Settlement.Increment txId) sn signatures

onOpenChainDecrementTx ::
  IsChainState tx =>
  Interpreter.InterpreterConfig tx ->
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
onOpenChainDecrementTx config env deposits st cs version utxo sn signatures =
  Interpreter.runProgram config $
    Workflow.run $
      Settlement.machine (ruleEnvironment env) deposits st cs version (Settlement.Decrement utxo) sn signatures

maybeRepostDecrementTx ::
  Interpreter.InterpreterConfig tx ->
  HeadSeed ->
  HeadId ->
  HeadParameters ->
  Maybe tx ->
  ConfirmedSnapshot tx ->
  Maybe (FinalizedSnapshot tx) ->
  -- | Slot rolled back to
  ChainSlot ->
  Outcome tx
maybeRepostDecrementTx config seed hid params decommit snapshot finalized slot =
  Interpreter.runProgram config $
    Repost.maybeRepostDecrementTx seed hid params decommit snapshot finalized slot

onClosedChainPartialFanoutTx ::
  IsTx tx =>
  Interpreter.InterpreterConfig tx ->
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout (keyed by new TxIn; values preserve duplicates)
  UTxOType tx ->
  Outcome tx
onClosedChainPartialFanoutTx config st cs utxo =
  Interpreter.runProgram config $
    Fanout.onClosedChainPartialFanoutTx st cs utxo

onPartialFanoutChainPartialFanoutTx ::
  IsTx tx =>
  Interpreter.InterpreterConfig tx ->
  PartialFanoutState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | UTxO distributed in this partial fanout
  UTxOType tx ->
  Outcome tx
onPartialFanoutChainPartialFanoutTx config st cs utxo =
  Interpreter.runProgram config $
    Fanout.onPartialFanoutChainPartialFanoutTx st cs utxo

repostFanoutStep :: IsTx tx => Interpreter.InterpreterConfig tx -> PartialFanoutState tx -> Outcome tx
repostFanoutStep config st =
  Interpreter.runProgram config $
    Fanout.repostFanoutStep st
