use cardano/assets.{PolicyId, Value}

pub const hydra_head_v1: ByteArray = "HydraHeadV1"

pub fn must_burn_st(head_id: PolicyId, mint: Value) -> Bool {
  let quantity =
    mint
      |> assets.quantity_of(head_id, hydra_head_v1)

  quantity == -1
}

// Helpers

pub fn trace_if_false(trace_log: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace_fail(trace_log)
  }
}

pub fn trace_fail(trace_log: String) -> a {
  trace trace_log
  fail
}
