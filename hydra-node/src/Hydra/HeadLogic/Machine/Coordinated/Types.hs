-- | Internal ports between the snapshot and transaction owners. Neither port
-- carries a NodeState or permits assigning another component's fields.
module Hydra.HeadLogic.Machine.Coordinated.Types where

import Hydra.Prelude

import Hydra.Chain.ChainState (ChainSlot, IsChainState (ChainStateType))
import Hydra.HeadLogic.Outcome (StateChanged)
import Hydra.HeadLogic.Rules.SnapshotRequest (SnapshotRequest)
import Hydra.HeadLogic.Rules.SnapshotVerification (VerifiedSnapshot)
import Hydra.HeadLogic.State (FinalizedSnapshot, SeenSnapshot)
import Hydra.Ledger (ValidationError)
import Hydra.Node.Deposit (Deposit)
import Hydra.Tx (HeadId, TxIdType, UTxOType)
import Hydra.Tx.Crypto (MultiSignature, Signature)
import Hydra.Tx.Party (Party)
import Hydra.Tx.Snapshot (ConfirmedSnapshot, Snapshot, SnapshotNumber, SnapshotVersion)

data Command tx
  = Transactions (TransactionCommand tx)
  | RequestSnapshot (SnapshotRequest tx)
  | ReplaySnapshotRequest SnapshotNumber
  | AcceptSnapshot (Snapshot tx) (Seq tx) (Maybe (TxIdType tx))
  | RecordSignature SnapshotNumber Party (Signature (Snapshot tx))
  | ConfirmSnapshot (VerifiedSnapshot tx)
  | ReplayConfirmation (MultiSignature (Snapshot tx))
  | AdoptSnapshot (VerifiedSnapshot tx)
  | ReplayAdoption (Snapshot tx) (MultiSignature (Snapshot tx))
  | ClearLocalState SnapshotNumber
  | ActivateDeposit (TxIdType tx) UTCTime (Deposit tx)
  | RecoverDeposit (ChainStateType tx) HeadId (TxIdType tx) ~(UTxOType tx)
  | SettleCommit (ChainStateType tx) SnapshotVersion (TxIdType tx) ~(UTxOType tx)
  | SettleDecommit (ChainStateType tx) SnapshotVersion ~(UTxOType tx)
  | Rollback ChainSlot

data TransactionCommand tx
  = ReceiveTransaction tx
  | RetainSnapshotDecommit tx
  | ApplyTransaction tx
  | ApplyValidatedTransaction tx ~(UTxOType tx) ~(UTxOType tx)
  | ForgetTransaction (UTxOType tx) tx ValidationError
  | ReserveDecommit tx
  | ReplayDecommit tx

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

data SnapshotState tx = SnapshotState
  { version :: SnapshotVersion
  , chainSlot :: ChainSlot
  , confirmed :: ConfirmedSnapshot tx
  , seen :: SeenSnapshot tx
  , finalizedCommit :: Maybe (FinalizedSnapshot tx)
  , finalizedDecommit :: Maybe (FinalizedSnapshot tx)
  }

data SnapshotOutput tx = SnapshotOutput
  { snapshots :: SnapshotState tx
  , ledgerUpdate :: LedgerUpdate tx
  , events :: [StateChanged tx]
  , refusal :: Maybe Text
  }
