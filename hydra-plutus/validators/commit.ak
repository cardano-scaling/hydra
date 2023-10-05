use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/value.{MintedValue, PolicyId, Value}

// TODO: Party is not used on-chain but is needed off-chain while it's still
// based on mock crypto. When we move to real crypto we could simply use
// the PT's token name to identify the committing party
type Datum {
  party: Data,
  commits: Data,
  head_id: PolicyId
}

type Redeemer {
  ViaCollectCom
  ViaAbort
}

const hydra_head_v1: ByteArray = "HydraHeadV1"

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
validator {
  fn commit(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    when redeemer is {
      // NOTE: The reimbursement of the committed output 'commit' is
      // delegated to the 'head' script who has more information to do it.
      ViaAbort ->
        traceIfFalse(
          toErrorCode(STNotBurnedError),
          must_burn_st(datum.head_id, context.transaction.mint)?,
        )
      ViaCollectCom -> {
        expect Some(head_output) = context.transaction.outputs |> list.head
        traceIfFalse(
          toErrorCode(STIsMissingInTheOutput),
          has_st(datum.head_id, head_output.value)?,
        )
      }
    }
  }
}

fn has_st(head_id: PolicyId, value: Value) -> Bool {
  value.quantity_of(value, head_id, hydra_head_v1) == 1
}

fn must_burn_st(head_id: PolicyId, mint: MintedValue) -> Bool {
  let quantity =
    mint
      |> value.from_minted_value
      |> value.quantity_of(head_id, hydra_head_v1)

  quantity == -1
}

// Helpers

fn traceIfFalse(traceLog: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace traceLog
    False
  }
}
