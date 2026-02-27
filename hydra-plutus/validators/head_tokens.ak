use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, flatten, quantity_of, tokens}
use cardano/transaction.{InlineDatum, Mint, OutputReference, Transaction}
use head_types.{Closed, Final, Initial, Open, State}

// Error codes matching Haskell HeadTokensError
const error_seed_not_spent = @"M01"

const error_wrong_number_of_tokens_minted = @"M02"

const error_missing_st = @"M03"

const error_wrong_number_of_initial_outputs = @"M04"

const error_wrong_datum = @"M05"

const error_minting_not_allowed = @"M06"

const error_no_pt = @"M07"

const error_wrong_quantity = @"M08"

const error_expected_head_datum_type = @"M09"

const error_expected_inline_datum = @"M10"

const error_multiple_head_output = @"M11"

const error_wrong_initial_datum = @"M12"

// MintAction type
pub type MintAction {
  Mint
  Burn
}

// Parameters for the minting policy
pub type HeadTokensParams {
  initial_validator: ScriptHash,
  head_validator: ScriptHash,
  seed_input: OutputReference,
}

// Main minting policy
validator head_tokens(params: HeadTokensParams) {
  mint(redeemer: MintAction, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Mint ->
        validate_tokens_minting(
          params.initial_validator,
          params.head_validator,
          params.seed_input,
          policy_id,
          self,
        )
      Burn -> validate_tokens_burning(policy_id, self)
    }
  }

  else(_) {
    fail
  }
}

// Validate token minting
fn validate_tokens_minting(
  initial_validator: ScriptHash,
  head_validator: ScriptHash,
  seed_input: OutputReference,
  policy_id: PolicyId,
  tx: Transaction,
) -> Bool {
  // 1. Seed input must be consumed
  let seed_consumed =
    list.any(
      tx.inputs,
      fn(input) { input.output_reference == seed_input },
    )
  expect seed_consumed

  // 2. Find the head output
  let head_address = Address { payment_credential: Script(head_validator), stake_credential: None }
  let head_outputs =
    list.filter(
      tx.outputs,
      fn(output) { output.address == head_address },
    )
  expect [head_output] = head_outputs

  // 3. Get datum from head output
  expect InlineDatum(head_datum_data) = head_output.datum
  expect head_state: State = head_datum_data

  // 4. Extract head parameters
  let (head_id, seed, n_parties) =
    when head_state is {
      Initial { head_id, seed, parties, .. } ->
        (head_id, seed, list.length(parties))
      _ -> fail error_expected_head_datum_type
    }

  // 5. Verify datum matches policy
  expect head_id == policy_id && seed == seed_input

  // 6. Count minted tokens
  let minted_tokens = tokens(tx.mint, policy_id)
  let minted_tokens_list = dict.to_pairs(minted_tokens)
  let minted_count =
    list.foldr(
      minted_tokens_list,
      0,
      fn(token, acc) {
        let Pair(_name, qty) = token
        acc + qty
      },
    )
  expect minted_count == n_parties + 1

  // 7. Check ST is paid to head
  let st_in_head = quantity_of(head_output.value, policy_id, "ST") == 1
  expect st_in_head

  // 8. Find initial outputs
  let initial_address =
    Address { payment_credential: Script(initial_validator), stake_credential: None }
  let initial_outputs =
    list.filter(
      tx.outputs,
      fn(output) { output.address == initial_address },
    )

  // 9. Check number of initial outputs
  expect list.length(initial_outputs) == n_parties

  // 10. Check each initial output has exactly one PT
  let all_have_pt =
    list.all(
      initial_outputs,
      fn(output) {
        let tokens_list = flatten(output.value)
        let pt_tokens =
          list.filter(
            tokens_list,
            fn(token) {
              let (pol, _name, qty) = token
              pol == policy_id && qty == 1
            },
          )
        list.length(pt_tokens) == 1
      },
    )
  expect all_have_pt

  // 11. Check each initial output has correct datum (head_id)
  let all_have_correct_datum =
    list.all(
      initial_outputs,
      fn(output) {
        when output.datum is {
          InlineDatum(datum_data) -> {
            expect datum_head_id: ByteArray = datum_data
            datum_head_id == policy_id
          }
          _ -> False
        }
      },
    )
  expect all_have_correct_datum

  True
}

// Validate token burning
fn validate_tokens_burning(policy_id: PolicyId, tx: Transaction) -> Bool {
  // All tokens of this policy must have negative quantity (burning)
  let minted_tokens = tokens(tx.mint, policy_id)
  let minted_tokens_list = dict.to_pairs(minted_tokens)
  let all_negative =
    list.all(
      minted_tokens_list,
      fn(token) {
        let Pair(_name, qty) = token
        qty < 0
      },
    )
  all_negative
}
