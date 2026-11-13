-- | Internal ports between the snapshot and transaction owners. Neither port
-- carries a NodeState or permits assigning another component's fields.
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
  | RecoverDeposit (ChainStateType tx) HeadId (TxIdType tx) ~(UTxOType tx)
  | SettleCommit (ChainStateType tx) SnapshotVersion (TxIdType tx) ~(UTxOType tx)
  | SettleDecommit (ChainStateType tx) SnapshotVersion ~(UTxOType tx)

data TransactionCommand tx
  = ReceiveTransaction tx
  | RetainSnapshotDecommit tx
  | ApplyValidatedTransaction tx ~(UTxOType tx) ~(UTxOType tx)
  | ForgetTransaction (UTxOType tx) tx ValidationError
  | ReserveDecommit tx

-- | Only the snapshot owner constructs a Rebase. The transaction owner chooses
-- how to rebuild its UTxO, including any reservation it retains.
data LedgerUpdate tx
  = Unchanged
  | Transaction (TransactionCommand tx)
  | Rebase (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | ConfirmTransactions
  | ClearTransactions
  | SelectDeposit (TxIdType tx) (Deposit tx)
  | ReleaseDeposit (TxIdType tx)
  | CommitSettled Bool (TxIdType tx) ~(UTxOType tx)
  | DecommitSettled Bool
  | ReplayTransaction tx
  | ReplayDecommit tx

-- | The transaction owner checks whether a deposit fits the local ledger; the
-- commit owner alone changes the selection and tracks the signed claim's phase.
data CommitUpdate tx
  = KeepCommit
  | RefreshCommit
  | SelectCommit (TxIdType tx)
  | SetCommitSelection (Maybe (TxIdType tx))
  | ReleaseCommit (TxIdType tx)

data SnapshotState tx = SnapshotState
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , seen :: SeenSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }
