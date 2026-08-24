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
// This is the only rule keeping other deposits out of an increment. The head
// grows by exactly the deposit its redeemer names and checks nothing about the
// remaining inputs, so a second pending deposit attached to an otherwise
// legitimate increment would have its value unaccounted for: the transaction
// author could route it to an address of their own, or push it into the head
// where no snapshot credits it and (partial) fanout's strict value conservation
// can never be satisfied.
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
// it. Two deposits whose commits hash alike are therefore both satisfied by one
// set of outputs, leaving the second deposit's value free for whoever builds the
// transaction. Recovering one deposit at a time removes that double satisfaction.
//
// Hashing alike is weaker than being identical: `preSerializedOutput` is raw
// bytes this validator never parses, so one commit carrying two concatenated
// output encodings collides with two commits carrying one each. Nothing on-chain
// ties a deposit's value to the commits it declares either — only
// `Hydra.Tx.Deposit.observeDepositTx` does, off-chain, before any party signs.
// Both are why double satisfaction has to be ruled out structurally here.
//
// Only the payment credential is compared. Every UTxO whose payment credential is
// this validator's hash is governed by it, whatever its staking part, so matching
// the whole address would let an attacker sidestep this check by parking a copied
// deposit at the same script with a stake credential attached.
fn only_own_deposit_spent(inputs: List<Input>, own_ref: OutputReference) -> Bool {
  expect Some(own_input) =
    list.find(inputs, fn(i) { output_reference.compare(i.output_reference, own_ref) == Equal })
  let own_credential = own_input.output.address.payment_credential
  traceIfFalse(
    toErrorCode(MultipleDepositsRecovered),
    list.count(inputs, fn(i) { i.output.address.payment_credential == own_credential }) == 1,
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
