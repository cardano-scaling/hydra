use aiken/builtin
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{Hash, Sha2_256, sha2_256}
use aiken/interval.{Finite, Interval, IntervalBound}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{Input, Output, OutputReference, Spend, Transaction, ValidityRange}
use cardano/transaction/output_reference
use util.{hydra_head_v2}

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
  Claim
  Recover(Int)
}

type DepositError {
  DepositPeriodSurpassed
  DepositNoUpperBoundDefined
  DepositNoLowerBoundDefined
  DepositPeriodNotReached
  IncorrectDepositHash
  DepositHeadInputNotFound
  HeadInputRedeemerNotFound
  HeadRedeemerNotIncrement
  DepositNotClaimedByHead
  MultipleDepositsRecovered
}

fn toErrorCode(err: DepositError) -> String {
  when err is {
    DepositPeriodSurpassed -> @"D01"
    DepositNoUpperBoundDefined -> @"D02"
    DepositNoLowerBoundDefined -> @"D03"
    DepositPeriodNotReached -> @"D04"
    IncorrectDepositHash -> @"D05"
    DepositHeadInputNotFound -> @"D06"
    HeadInputRedeemerNotFound -> @"D07"
    HeadRedeemerNotIncrement -> @"D08"
    DepositNotClaimedByHead -> @"D09"
    MultipleDepositsRecovered -> @"D10"
  }
}

validator deposit {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    own_ref: OutputReference,
    self: Transaction,
  ) {
    expect Some(datum) = datum
    when redeemer is {
      Claim ->
        traceIfFalse(
          toErrorCode(DepositPeriodSurpassed),
          before_deadline(self.validity_range, datum.deadline),
        ) && expect_increment_redeemer(self, datum.head_id, own_ref)
      Recover(n) ->
        traceIfFalse(
          toErrorCode(DepositPeriodNotReached),
          after_deadline(self.validity_range, datum.deadline),
        ) && only_own_deposit_spent(self.inputs, own_ref) && recover_outputs(n, datum.commits, self.outputs)
    }
  }

  else(_) {
    fail
  }
}

fn expect_increment_redeemer(self: Transaction, head_id: PolicyId, own_ref: OutputReference) -> Bool {
  when
    list.find(
      self.inputs,
      fn(i) { assets.quantity_of(i.output.value, head_id, hydra_head_v2) == 1 },
    )
  is {
    None -> {
      trace toErrorCode(DepositHeadInputNotFound)
      False
    }
    Some(head_input) ->
      when
        pairs.get_first(self.redeemers, Spend(head_input.output_reference))
      is {
        None -> {
          trace toErrorCode(HeadInputRedeemerNotFound)
          False
        }
        Some(head_redeemer) ->
          traceIfFalse(
            toErrorCode(HeadRedeemerNotIncrement),
            is_head_increment(head_redeemer),
          ) && traceIfFalse(
            toErrorCode(DepositNotClaimedByHead),
            is_claimed_deposit(head_redeemer, own_ref),
          )
      }
  }
}

// True if the redeemer spending the head input is the Increment variant of
// the head validator's Input type.
fn is_head_increment(d: Data) -> Bool {
  builtin.un_constr_data(d).1st == 0 // Matches the index pinned for `Increment` in HeadState.hs's `makeIsDataIndexed ''Input` list.
}

// True if the head's Increment redeemer claims exactly this deposit.
//
// The head only checks that the deposit it names is spent, and adds the value of
// every non-head script input to its output. Without this check any other pending
// deposit could be attached as an extra input to a legitimate increment: its value
// would land in the head while the multi-signed snapshot credits nothing for it,
// leaving the head over-funded and unable to satisfy the strict value
// conservation of (partial) fanout.
fn is_claimed_deposit(d: Data, own_ref: OutputReference) -> Bool {
  // `Increment` wraps a single `IncrementRedeemer` whose fields are
  // [signature, snapshotNumber, increment, decommitOutputsHash]; `increment` is the
  // claimed deposit's output reference. Keep in sync with HeadState.hs.
  expect [increment_redeemer] = builtin.un_constr_data(d).2nd
  expect [_signature, _snapshot_number, claimed, ..] = builtin.un_constr_data(increment_redeemer).2nd
  expect claimed_ref: OutputReference = claimed
  output_reference.compare(claimed_ref, own_ref) == Equal
}

// Only this deposit may be spent by a recover transaction.
//
// `recover_outputs` checks the commits of the deposit being spent against the
// FIRST n outputs of the transaction, which are shared by every deposit input in
// it. Two content-identical deposits recovered together are therefore both
// satisfied by one set of outputs, and everything past index n is unconstrained
// and can be directed anywhere by whoever builds the transaction. Recovering one
// deposit at a time removes that double satisfaction.
fn only_own_deposit_spent(inputs: List<Input>, own_ref: OutputReference) -> Bool {
  expect Some(own_input) =
    list.find(inputs, fn(i) { output_reference.compare(i.output_reference, own_ref) == Equal })
  traceIfFalse(
    toErrorCode(MultipleDepositsRecovered),
    list.count(inputs, fn(i) { i.output.address == own_input.output.address }) == 1,
  )
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
