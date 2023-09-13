use aiken/list
use aiken/transaction.{ScriptContext}
use aiken/transaction/value.{MintedValue, PolicyId, Value}

type Datum =
  (Data, Data, PolicyId)

type Redeemer {
  ViaCollectCom
  ViaAbort
}

const hydra_head_v1: ByteArray = "HydraHeadV1"

validator {
  fn commit(datum: Datum, redeemer: Redeemer, context: ScriptContext) -> Bool {
    let (_, _, head_id) = datum
    when redeemer is {
      ViaAbort -> must_burn_st(head_id, context.transaction.mint)?
      ViaCollectCom -> {
        expect Some(head_output) = context.transaction.outputs |> list.head
        has_st(head_id, head_output.value)?
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
