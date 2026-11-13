-- | Diagnostic descriptions of one execution. These values neither determine
-- protocol behavior nor prescribe a rendering format.
module Hydra.HeadLogic.Trace (TraceStep (..), changeLabel, effectLabel) where

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
  TransactionReceived{} -> "transaction received"
  TransactionAppliedToLocalUTxO{} -> "transaction applied to local state"
  SnapshotRequestDecided{} -> "snapshot request decided"
  SnapshotRequested{} -> "snapshot requested"
  PartySignedSnapshot{} -> "party signed snapshot"
  SnapshotConfirmed{} -> "snapshot confirmed"
  CommitApproved{} -> "commit approved"
  DecommitApproved{} -> "decommit approved"
  TxInvalid{} -> "tx invalid"
  HeadOpened{} -> "head opened"
  IgnoredHeadInitializing{} -> "ignored head initializing"
  LocalStateCleared{} -> "local state cleared"
  DepositRecorded{} -> "deposit recorded"
  DepositActivated{} -> "deposit activated"
  DepositExpired{} -> "deposit expired"
  DepositRecovered{} -> "deposit recovered"
  CommitFinalized{} -> "commit finalized"
  DecommitRecorded{} -> "decommit recorded"
  DecommitInvalid{} -> "decommit invalid"
  DecommitFinalized{} -> "decrement finalized"
  HeadClosed{} -> "head closed"
  HeadContested{} -> "head contested"
  HeadIsReadyToFanout{} -> "head is ready to fanout"
  HeadFanoutInitiated{} -> "head fanout initiated"
  HeadPartialFanoutSelected{} -> "partial fanout selected"
  HeadPartialFannedOut{} -> "head partially fanned out"
  HeadFannedOut{} -> "head fanned out"
  HeadFanoutReverted{} -> "head fanout reverted"
  TickObserved{} -> "tick observed"
  ChainRolledBack{} -> "chain rolled back"
  NodeSynced{} -> "node in sync"
  NodeUnsynced{} -> "node out of sync"
  Checkpoint{} -> "checkpoint restored"
  NetworkConnected -> "network connected"
  NetworkDisconnected -> "network disconnected"
  NetworkVersionMismatch{} -> "network version mismatch"
  NetworkClusterIDMismatch{} -> "network cluster ID mismatch"
  NetworkBroadcastStalled{} -> "network broadcast stalled"
  NetworkBroadcastResumed -> "network broadcast resumed"
  PeerConnected{} -> "peer connected"
  PeerDisconnected{} -> "peer disconnected"

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
