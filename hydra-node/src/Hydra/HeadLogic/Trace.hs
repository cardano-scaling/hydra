-- | Diagnostic descriptions of one execution. These values neither determine
-- protocol behavior nor prescribe a rendering format.
module Hydra.HeadLogic.Trace (TraceStep (..), changeLabel, effectLabel) where

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

-- | Compact labels for the first migrated rule. Other instructions remain
-- visible without forcing potentially large payloads or cryptographic caches.
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
  _ -> "state change"

effectLabel :: Effect tx -> Text
effectLabel = \case
  NetworkEffect AckSn{} -> "broadcast AckSn"
  NetworkEffect ReqSn{} -> "broadcast ReqSn"
  NetworkEffect{} -> "network effect"
  ClientEffect{} -> "client effect"
  OnChainEffect{} -> "settlement effect"
