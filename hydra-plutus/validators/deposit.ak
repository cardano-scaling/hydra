use aiken/builtin
use aiken/collection/list
use aiken/crypto.{Hash, Sha2_256, sha2_256}
use aiken/interval.{Finite, Interval, IntervalBound}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Output, OutputReference, Transaction, ValidityRange}

pub type Commit {
  Commit {
     input: OutputReference,
     preSerializedOutput: ByteArray
  }
}

pub type Datum {
  head_id: PolicyId,
  deadline: Int,
  commits: Data,
}

pub type Redeemer {
  Claim(PolicyId)
  Recover(Int)
}

type DepositError {
  DepositDeadlineSurpassed
  DepositNoUpperBoundDefined
  DepositNoLowerBoundDefined
  DepositDeadlineNotReached
  IncorrectDepositHash
  WrongHeadIdInDepositDatum
}

fn toErrorCode(err: DepositError) -> String {
  when err is {
    DepositDeadlineSurpassed -> @"D01"
    DepositNoUpperBoundDefined -> @"D02"
    DepositNoLowerBoundDefined -> @"D03"
    DepositDeadlineNotReached -> @"D04"
    IncorrectDepositHash -> @"D05"
    WrongHeadIdInDepositDatum -> @"D06"
  }
}

validator deposit {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    _utxo: OutputReference,
    self: Transaction,
  ) {
    expect Some(datum) = datum
    when redeemer is {
      Claim(currencySymbol) ->
       traceIfFalse(
         toErrorCode(WrongHeadIdInDepositDatum),
         check_head_id(datum.head_id, currencySymbol),
       ) &&
       traceIfFalse(
          toErrorCode(DepositDeadlineSurpassed),
          before_deadline(self.validity_range, datum.deadline),
        )
      Recover(n) ->
        traceIfFalse(
          toErrorCode(DepositDeadlineNotReached),
          after_deadline(self.validity_range, datum.deadline),
        ) && recover_outputs(n, datum.commits, self.outputs)
    }
  }

  else(_) {
    fail
  }
}

// Helpers

fn check_head_id(datumCS: ByteArray, redeemerCS: ByteArray) {
  datumCS == redeemerCS
}

fn before_deadline(range: ValidityRange, dl) {
  when range.upper_bound.bound_type is {
    Finite(tx_upper_validity) -> tx_upper_validity <= dl
    _ -> traceIfFalse(toErrorCode(DepositNoUpperBoundDefined), False)
  }
}

fn after_deadline(range: ValidityRange, dl) {
  when range.lower_bound.bound_type is {
    Finite(tx_lower_validity) -> tx_lower_validity > dl
    _ -> traceIfFalse(toErrorCode(DepositNoLowerBoundDefined), False)
  }
}

fn recover_outputs(
  n: Int,
  commits: Data,
  outputs: List<Output>,
) {
  let hashOfOutputs = outputs |> list.take(n) |> hash_tx_outs
  traceIfFalse(
    toErrorCode(IncorrectDepositHash),
    hashOfOutputs == hashPreSerializedCommits(commits),
  )
}

// Hash a potentially unordered list of commits
fn hashPreSerializedCommits(commits: Data) -> Hash<Sha2_256, a> {
  commits
    |> list.sort(output_reference.compare)
    |> list.map(fn(commit) { builtin.serialise_data(commit) })
    |> list.reduce(#"", bytearray.concat)
    |> sha2_256
}

// Hash a pre-ordered list of transaction outputs
fn hash_tx_outs(outputs: List<Output>) -> Hash<Sha2_256, a> {
  outputs
    |> list.map(fn(output) { builtin.serialise_data(output) })
    |> list.reduce(#"", bytearray.concat)
    |> sha2_256
}

fn traceIfFalse(traceLog: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace traceLog
    False
  }
}
