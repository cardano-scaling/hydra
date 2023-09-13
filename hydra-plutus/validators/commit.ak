use aiken/list
use aiken/transaction.{OutputReference, ScriptContext}
use aiken/transaction/value.{PolicyId, Value}

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
      ViaAbort -> todo
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
