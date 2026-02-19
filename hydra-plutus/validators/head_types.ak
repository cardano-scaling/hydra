use aiken/crypto.{VerificationKeyHash}
use cardano/transaction.{OutputReference}

// Type aliases matching Haskell definitions
pub type Hash =
  ByteArray

pub type Signature =
  ByteArray

pub type SnapshotNumber =
  Int

pub type SnapshotVersion =
  Int

pub type ContestationPeriod =
  Int

pub type CurrencySymbol =
  ByteArray

// Party type - simplified on-chain representation
pub type Party =
  ByteArray

// OpenDatum - sub-type for the open state
pub type OpenDatum {
  head_id: CurrencySymbol,
  parties: List<Party>,
  contestation_period: ContestationPeriod,
  version: SnapshotVersion,
  utxo_hash: Hash,
}

// ClosedDatum - sub-type for the closed state
pub type ClosedDatum {
  head_id: CurrencySymbol,
  parties: List<Party>,
  contestation_period: ContestationPeriod,
  version: SnapshotVersion,
  snapshot_number: SnapshotNumber,
  utxo_hash: Hash,
  alpha_utxo_hash: Hash,
  omega_utxo_hash: Hash,
  contesters: List<VerificationKeyHash>,
  contestation_deadline: Int,
}

// Main State type for the Head validator
pub type State {
  Initial {
    contestation_period: ContestationPeriod,
    parties: List<Party>,
    head_id: CurrencySymbol,
    seed: OutputReference,
  }
  Open(OpenDatum)
  Closed(ClosedDatum)
  Final
}

// CloseRedeemer - auxiliary data for close transition
pub type CloseRedeemer {
  CloseInitial
  CloseAny { signature: List<Signature> }
  CloseUnusedDec { signature: List<Signature> }
  CloseUsedDec {
    signature: List<Signature>,
    already_decommitted_utxo_hash: Hash,
  }
  CloseUnusedInc {
    signature: List<Signature>,
    already_committed_utxo_hash: Hash,
  }
  CloseUsedInc {
    signature: List<Signature>,
    already_committed_utxo_hash: Hash,
  }
}

// ContestRedeemer - auxiliary data for contest transition
pub type ContestRedeemer {
  ContestCurrent { signature: List<Signature> }
  ContestUsedDec {
    signature: List<Signature>,
    already_decommitted_utxo_hash: Hash,
  }
  ContestUnusedDec { signature: List<Signature> }
  ContestUnusedInc {
    signature: List<Signature>,
    already_committed_utxo_hash: Hash,
  }
  ContestUsedInc { signature: List<Signature> }
}

// IncrementRedeemer - auxiliary data for increment transition
pub type IncrementRedeemer {
  signature: List<Signature>,
  snapshot_number: SnapshotNumber,
  increment: OutputReference,
}

// DecrementRedeemer - auxiliary data for decrement transition
pub type DecrementRedeemer {
  signature: List<Signature>,
  snapshot_number: SnapshotNumber,
  number_of_decommit_outputs: Int,
}

// Main redeemer type (Input in Haskell)
pub type HeadRedeemer {
  CollectCom
  Increment(IncrementRedeemer)
  Decrement(DecrementRedeemer)
  Close(CloseRedeemer)
  Contest(ContestRedeemer)
  Abort
  Fanout {
    number_of_fanout_outputs: Int,
    number_of_commit_outputs: Int,
    number_of_decommit_outputs: Int,
  }
}
