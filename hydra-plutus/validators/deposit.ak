use aiken/builtin
use aiken/collection/list
use aiken/crypto.{Hash, Sha2_256, sha2_256}
use aiken/interval.{Finite, Interval, IntervalBound}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Output, OutputReference, Transaction, ValidityRange}
use cardano/transaction/output_reference

pub type Commit {
  Commit {
     input: OutputReference,
     preSerializedOutput: ByteArray
  }
}

pub type Datum {
  head_id: PolicyId,
  deadline: Int,
  commits: List<Commit>,
}

pub type Redeemer {
  Claim(PolicyId)
  Recover(Int)
}

type DepositError {
  DepositPeriodSurpassed
  DepositNoUpperBoundDefined
  DepositNoLowerBoundDefined
  DepositPeriodNotReached
  IncorrectDepositHash
  WrongHeadIdInDepositDatum
}

fn toErrorCode(err: DepositError) -> String {
  when err is {
    DepositPeriodSurpassed -> @"D01"
    DepositNoUpperBoundDefined -> @"D02"
    DepositNoLowerBoundDefined -> @"D03"
    DepositPeriodNotReached -> @"D04"
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
          toErrorCode(DepositPeriodSurpassed),
          before_deadline(self.validity_range, datum.deadline),
        )
      Recover(n) ->
        traceIfFalse(
          toErrorCode(DepositPeriodNotReached),
          after_deadline(self.validity_range, datum.deadline),
        ) && recover_outputs(n, datum.commits, self.outputs)
    }
  }

  else(_) {
    fail
  }
}


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
  commits: List<Commit>,
  outputs: List<Output>,
) {
  let depositOutputs = outputs |> list.take(n)
  let deposited = commits // |> list.map(fn(commit){ commit.input})

  let hashOfOutputs = hash_tx_outs(depositOutputs)
  let preSerializedCommits = hashPreSerializedCommits(deposited)

  traceIfFalse(
    toErrorCode(IncorrectDepositHash),
    hashOfOutputs == preSerializedCommits,
  )
}

// Helpers

// Hash a potentially unordered list of commits
fn hashPreSerializedCommits(commits: List<Commit>) -> Hash<Sha2_256, a> {
  commits
    |> list.sort(
           fn(commit, commit2){
             output_reference.compare(commit.input, commit2.input)
             }
           )
    |> list.map(fn(commit) { commit.preSerializedOutput })
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
