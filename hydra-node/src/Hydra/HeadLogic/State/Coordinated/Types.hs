module Hydra.HeadLogic.State.Coordinated.Types where

import Hydra.Chain.ChainState (
  ChainSlot,
  IsChainState (ChainStateType),
 )
import Hydra.HeadLogic.Deposit (Deposit)
import Hydra.HeadLogic.Snapshot.Candidate (Candidate)
import Hydra.HeadLogic.Snapshot.Request (SnapshotRequest)
import Hydra.HeadLogic.Snapshot.Verification (
  VerifiedCandidate,
  VerifiedSnapshot,
 )
import Hydra.HeadLogic.State (FinalizedSnapshot, SeenSnapshot)
import Hydra.Ledger (ValidationError)
import Hydra.Prelude
import Hydra.Tx (HeadId, TxIdType, UTxOType)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot,
  Snapshot,
  SnapshotNumber,
  SnapshotVersion,
 )

-- | All the things that can occur in relation to a snapshot.
data Command tx
  = Transactions (LocalTransactionCommand tx)
  | RequestSnapshot (SnapshotRequest tx)
  | AcceptSnapshot (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | RecordSignature (Candidate tx) Party (Signature (Snapshot tx))
  | ConfirmSnapshot (VerifiedCandidate tx)
  | AdoptSnapshot (VerifiedSnapshot tx)
  | ClearLocalState SnapshotNumber
  | ActivateDeposit
      (TxIdType tx)
      UTCTime
      (Deposit tx)
  | RecordDepositRecovery
      (ChainStateType tx)
      HeadId
      (TxIdType tx)
      ~(UTxOType tx)
  | RecordCommitFinalization
      (ChainStateType tx)
      SnapshotVersion
      (TxIdType tx)
      ~(UTxOType tx)
  | RecordDecommitFinalization
      (ChainStateType tx)
      SnapshotVersion
      ~(UTxOType tx)

-- | Operations on locally tracked transactions, including decommit payloads.
-- Deposit selection and settlement arrive through the other coordinated commands.
data LocalTransactionCommand tx
  = ReceiveTransaction tx
  | RetainSnapshotDecommit tx
  | ApplyValidatedTransaction tx ~(UTxOType tx) ~(UTxOType tx)
  | MarkTransactionInvalid (UTxOType tx) tx ValidationError
  | RecordPending tx

data TransactionUpdate tx
  = TransactionsUnchanged
  | RunLocalTransactionCommand (LocalTransactionCommand tx)
  | RebuildFromSnapshot (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | ReconcileConfirmedSnapshot
  | ResetTransactionState
  | TrySelectDeposit (TxIdType tx) (Deposit tx)
  | ClearDepositSelection (TxIdType tx)
  | CommitSettled VersionChange (TxIdType tx) ~(UTxOType tx)
  | DecommitSettled VersionChange
  | ReplayTransactionApplication tx
  | ReplayPendingDecommit tx

-- | Whether a settlement observation advances the working snapshot version.
data VersionChange
  = VersionAdvanced
  | VersionNotAdvanced

data CommitUpdate tx
  = CommitUnchanged
  | RecomputeCommitPhase
  | SelectDepositIfUnselected (TxIdType tx)
  | SetDepositSelection (Maybe (TxIdType tx))
  | DeselectDeposit (TxIdType tx)

data SnapshotState tx = SnapshotState
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , seen :: SeenSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }
