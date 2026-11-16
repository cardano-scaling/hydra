-- | Internal ports between the snapshot, transaction and commit owners. These
-- ports carry no NodeState and cannot assign another component's fields.
module Hydra.HeadLogic.Machine.Coordinated.Types where

import Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot, IsChainState (ChainStateType))
import Hydra.HeadLogic.Machine.Coordinated.Candidate (Candidate)
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedCandidate, VerifiedSnapshot)
import Hydra.HeadLogic.State (FinalizedSnapshot, SeenSnapshot)
import Hydra.Ledger (ValidationError)
import Hydra.Node.Deposit (Deposit)
import Hydra.Tx (HeadId, TxIdType, UTxOType)
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot, SnapshotNumber, SnapshotVersion)

data Command tx
  = Transactions (TransactionCommand tx)
  | RequestSnapshot (SnapshotRequest tx)
  | AcceptSnapshot (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | RecordSignature (Candidate tx) Party (Signature (Snapshot tx))
  | ConfirmSnapshot (VerifiedCandidate tx)
  | AdoptSnapshot (VerifiedSnapshot tx)
  | ClearLocalState SnapshotNumber
  | ActivateDeposit (TxIdType tx) UTCTime (Deposit tx)
  | -- | Record a deposit recovery observed on-chain.
    RecordDepositRecovery (ChainStateType tx) HeadId (TxIdType tx) ~(UTxOType tx)
  | -- | Record an observed increment that finalizes a commit.
    RecordCommitFinalization (ChainStateType tx) SnapshotVersion (TxIdType tx) ~(UTxOType tx)
  | -- | Record an observed decrement that finalizes a decommit.
    RecordDecommitFinalization (ChainStateType tx) SnapshotVersion ~(UTxOType tx)

data TransactionCommand tx
  = ReceiveTransaction tx
  | RetainSnapshotDecommit tx
  | ApplyValidatedTransaction tx ~(UTxOType tx) ~(UTxOType tx)
  | -- | Report invalidity and remove the transaction from the registry.
    MarkTransactionInvalid (UTxOType tx) tx ValidationError
  | ReserveDecommit tx

-- | Updates for the transaction owner's registry, local ledger and reservation.
-- Only the snapshot owner constructs a 'Rebase'; the transaction owner chooses
-- how to rebuild its UTxO, including any reservation it retains. Selection
-- updates are passed to the commit owner after the transaction owner's checks.
data TransactionUpdate tx
  = Unchanged
  | Transaction (TransactionCommand tx)
  | Rebase (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | -- | Reconcile reservations and commit state after confirmation or adoption.
    ReconcileConfirmedSnapshot
  | ClearTransactions
  | -- | Check whether the deposit fits before attempting selection.
    TrySelectDeposit (TxIdType tx) (Deposit tx)
  | -- | Clear the selection only if it names this deposit.
    ClearDepositSelection (TxIdType tx)
  | CommitSettled VersionChange (TxIdType tx) ~(UTxOType tx)
  | DecommitSettled VersionChange
  | ReplayTransaction tx
  | ReplayDecommit tx

-- | Whether a settlement observation advances the working snapshot version.
data VersionChange
  = VersionAdvanced
  | -- | The observed version is equal to or older than the working version.
    VersionNotAdvanced

-- | The transaction owner checks whether a deposit fits the local ledger; the
-- commit owner alone changes the selection and tracks the signed claim's phase.
-- Every update except 'CommitUnchanged' recomputes the phase from the supplied
-- confirmed snapshot and head version.
data CommitUpdate tx
  = -- | Preserve both the selection and the phase.
    CommitUnchanged
  | -- | Preserve the selection and recompute the phase.
    RecomputeCommitPhase
  | -- | Select this deposit only when no deposit is selected.
    SelectDepositIfUnselected (TxIdType tx)
  | -- | Replace the selection, or clear it with 'Nothing'.
    SetDepositSelection (Maybe (TxIdType tx))
  | -- | Clear the selection only if it names this deposit.
    DeselectDeposit (TxIdType tx)

data SnapshotState tx = SnapshotState
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , seen :: SeenSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }
