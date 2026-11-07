-- | Diagnostic descriptions of one execution. These values neither determine
-- protocol behavior nor prescribe a rendering format.
module Hydra.HeadLogic.Trace (TraceStep (..), traceLabel, changeLabel, effectLabel) where

import Hydra.Chain (PostChainTx (..))
import Hydra.HeadLogic.Outcome (Effect (..), StateChanged (..))
import Hydra.Network.Message (Message (..))
import Hydra.Prelude

data TraceStep
  = LedgerApplied Bool
  | PrimitiveCalled Text
  | Changed Text
  | Emitted Text
  | Decided Text Bool
  | Finished
  | Suspended
  | Rejected
  deriving stock (Eq, Show)

-- | Labels inspect constructors only; tracing never forces payloads or caches.
changeLabel :: StateChanged tx -> Text
changeLabel = \case
  TransactionReceived{} -> "remember transaction"
  TransactionAppliedToLocalUTxO{} -> "apply transaction to local state"
  SnapshotRequestDecided{} -> "record snapshot request"
  SnapshotRequested{} -> "record requested snapshot and prune local transactions"
  PartySignedSnapshot{} -> "record snapshot signature"
  SnapshotConfirmed{} -> "confirm snapshot"
  CommitApproved{} -> "approve deposit"
  DecommitApproved{} -> "approve decommit"
  TxInvalid{} -> "record invalid transaction"
  HeadOpened{} -> "open head"
  IgnoredHeadInitializing{} -> "ignore incompatible init"
  LocalStateCleared{} -> "clear optimistic local state"
  DepositRecorded{} -> "record deposit"
  DepositActivated{} -> "activate deposit"
  DepositExpired{} -> "expire deposit"
  DepositRecovered{} -> "consume recovered deposit"
  CommitFinalized{} -> "finalize increment"
  DecommitRecorded{} -> "reserve decommit"
  DecommitInvalid{} -> "record invalid decommit"
  DecommitFinalized{} -> "finalize decrement"
  HeadClosed{} -> "close head"
  HeadContested{} -> "record contest"
  HeadIsReadyToFanout{} -> "notify ready to fanout"
  HeadFanoutInitiated{} -> "start automatic fanout"
  HeadPartialFanoutSelected{} -> "select fanout outputs"
  HeadPartialFannedOut{} -> "record distributed outputs"
  HeadFannedOut{} -> "finish fanout"
  HeadFanoutReverted{} -> "revert failed fanout initiation"
  TickObserved{} -> "advance chain time"
  ChainRolledBack{} -> "record chain rollback"
  NodeSynced{} -> "mark node in sync"
  NodeUnsynced{} -> "mark node catching up"
  Checkpoint{} -> "restore checkpoint"
  NetworkConnected -> "record network connected"
  NetworkDisconnected -> "record network disconnected"
  NetworkVersionMismatch{} -> "record network version mismatch"
  NetworkClusterIDMismatch{} -> "record network cluster mismatch"
  NetworkBroadcastStalled{} -> "record broadcast stalled"
  NetworkBroadcastResumed -> "record broadcast resumed"
  PeerConnected{} -> "record peer connected"
  PeerDisconnected{} -> "record peer disconnected"

effectLabel :: Effect tx -> Text
effectLabel = \case
  NetworkEffect AckSn{} -> "broadcast AckSn"
  NetworkEffect ReqSn{} -> "broadcast ReqSn"
  NetworkEffect ReqTx{} -> "broadcast ReqTx"
  NetworkEffect ReqDec{} -> "broadcast ReqDec"
  ClientEffect{} -> "client effect"
  OnChainEffect{postChainTx} -> case postChainTx of
    InitTx{} -> "post init"
    CloseTx{} -> "post close"
    ContestTx{} -> "post contest"
    IncrementTx{} -> "post increment"
    DecrementTx{} -> "post decrement"
    RecoverTx{} -> "post recovery"
    FanoutTx{} -> "post full fanout"
    PartialFanoutTx{} -> "post partial fanout"
    FinalPartialFanoutTx{} -> "post final fanout"

traceLabel :: TraceStep -> Text
traceLabel = \case
  LedgerApplied ok -> "ledger application: " <> if ok then "valid" else "invalid"
  PrimitiveCalled name -> "primitive: " <> name
  Changed name -> "stage: " <> name
  Emitted name -> "buffer: " <> name
  Decided name answer -> name <> ": " <> if answer then "yes" else "no"
  Finished -> "finish: commit changes and release effects"
  Suspended -> "wait: commit changes and discard buffered effects"
  Rejected -> "reject: discard changes and effects"
