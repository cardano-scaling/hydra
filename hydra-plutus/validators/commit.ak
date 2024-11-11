use aiken/collection/list
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{OutputReference, Transaction}
use util.{hydra_head_v1, must_burn_st, trace_if_false}

// TODO: Party is not used on-chain but is needed off-chain while it's still
// based on mock crypto. When we move to real crypto we could simply use
// the PT's token name to identify the committing party
pub type Datum {
  party: Data,
  commits: Data,
  head_id: PolicyId,
}

pub type Redeemer {
  ViaCollectCom
  ViaAbort
}

type CommitError {
  STNotBurnedError
  STIsMissingInTheOutput
}

fn toErrorCode(err: CommitError) -> String {
  when err is {
    STNotBurnedError -> @"C01"
    STIsMissingInTheOutput -> @"C02"
  }
}

// | The validator used to collect & open or abort a Head.
// | The v_commit validator verifies that:
//
//   * spent in a transaction also consuming a v_head output
//
//   * ST is burned if the redeemer is 'ViaAbort'
//
//   * ST is present in the output if the redeemer is 'ViaCollectCom'
validator commit {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    _utxo: OutputReference,
    self: Transaction,
  ) {
    expect Some(datum) = datum
    when redeemer is {
      // NOTE: The reimbursement of the committed output 'commit' is
      // delegated to the 'head' script who has more information to do it.
      ViaAbort ->
        trace_if_false(
          toErrorCode(STNotBurnedError),
          must_burn_st(datum.head_id, self.mint)?,
        )
      ViaCollectCom -> {
        expect Some(head_output) = self.outputs |> list.head
        trace_if_false(
          toErrorCode(STIsMissingInTheOutput),
          has_st(datum.head_id, head_output.value)?,
        )
      }
    }
  }

  else(_) {
    fail
  }
}

fn has_st(head_id: PolicyId, value: Value) -> Bool {
  assets.quantity_of(value, head_id, hydra_head_v1) == 1
}
