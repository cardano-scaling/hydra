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

-- | All the things that can occur in relation to a snapshot.
data Command tx
  = Transactions (LocalTransactionCommand tx)
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

-- | Operations on locally tracked transactions, including decommit payloads.
-- Deposit selection and settlement arrive through the other coordinated commands.
data LocalTransactionCommand tx
  = ReceiveTransaction tx
  | RetainSnapshotDecommit tx
  | ApplyValidatedTransaction tx ~(UTxOType tx) ~(UTxOType tx)
  | -- | Report invalidity and remove the transaction from the registry.
    MarkTransactionInvalid (UTxOType tx) tx ValidationError
  | -- | Record a pending decommit and exclude its inputs from the local UTxO.
    -- The ReqDec workflow checks validity and capacity before issuing this command.
    RecordPending tx

-- | Updates for the transaction owner's registry, local ledger and reservation.
-- Only the snapshot owner constructs a 'RebuildFromSnapshot'; the transaction
-- owner chooses how to rebuild its UTxO, including any reservation it retains.
-- Selection updates are passed to the commit owner after the transaction
-- owner's checks.
data TransactionUpdate tx
  = -- | Preserve transaction and commit state, even if snapshot state changes.
    TransactionsUnchanged
  | -- | Execute a command in the transaction owner.
    RunLocalTransactionCommand (LocalTransactionCommand tx)
  | -- | Rebuild the local ledger while preserving reserved decommit inputs.
    -- Arguments: requested snapshot, already-pruned retained transactions in
    -- application order, and selected deposit identifier. Also replace the
    -- local transaction sequence, remove the snapshot's transactions from the
    -- registry and update deposit selection. The snapshot can be unconfirmed.
    RebuildFromSnapshot (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | -- | Reconcile reservations and commit state after confirmation or adoption.
    ReconcileConfirmedSnapshot
  | -- | Discard the local transaction sequence and registry. With a confirmed
    -- snapshot, restore its spendable base, clear deposit selection and keep
    -- only its approved decommit. With an initial snapshot, empty the local
    -- ledger while leaving selection and reservation unchanged.
    ResetTransactionState
  | -- | Check whether the deposit fits before attempting selection.
    TrySelectDeposit (TxIdType tx) (Deposit tx)
  | -- | Clear the selection only if it names this deposit.
    ClearDepositSelection (TxIdType tx)
  | -- | Apply an observed commit settlement.
    -- Arguments: version change, deposit identifier and deposited UTxO.
    -- 'VersionAdvanced' adds the UTxO and clears selection;
    -- 'VersionNotAdvanced' only clears a matching selection.
    CommitSettled VersionChange (TxIdType tx) ~(UTxOType tx)
  | -- | Apply an observed decommit settlement. 'VersionAdvanced' releases the
    -- reservation; 'VersionNotAdvanced' leaves transaction state unchanged.
    DecommitSettled VersionChange
  | -- | Restore a recorded transaction application to the local ledger and
    -- transaction sequence, without repeating ledger validation.
    ReplayTransactionApplication tx
  | -- | Restore a recorded pending decommit and its effect on the local
    -- UTxO, without repeating live command admission.
    ReplayPendingDecommit tx

-- | Whether a settlement observation advances the working snapshot version.
data VersionChange
  = VersionAdvanced
  | -- | The observed version is equal to or older than the working version.
    VersionNotAdvanced

-- | Instructions from the transaction owner for updating the commit owner's
-- selected deposit and commit phase.
--
-- 'CommitUnchanged' keeps both as they are. Every other update applies its
-- selection change (if any), then derives the phase from the resulting selection
-- and snapshot state. A commit approved by the confirmed snapshot at the current
-- head version awaits settlement, even without a selected deposit. Otherwise,
-- a selected deposit awaits approval; no selection means no pending commit.
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
