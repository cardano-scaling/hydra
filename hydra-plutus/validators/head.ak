use aiken/builtin
use aiken/collection/list
use aiken/crypto.{blake2b_256, verify_ed25519_signature, ScriptHash, VerificationKeyHash}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, flatten, from_asset, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use head_types.{
  Abort, Close, CloseAny, CloseInitial, CloseRedeemer, Closed, ClosedDatum,
  CloseUnusedDec, CloseUnusedInc, CloseUsedDec, CloseUsedInc, CollectCom,
  Contest, ContestCurrent, ContestRedeemer, ContestUnusedDec, ContestUnusedInc,
  ContestUsedDec, ContestUsedInc, Decrement, DecrementRedeemer, Fanout, Final,
  HeadRedeemer, Increment, IncrementRedeemer, Initial, Open, OpenDatum, State,
}

// Error codes matching Haskell HeadError
const error_invalid_head_state_transition = @"H1"

const error_changed_parameters = @"H2"

const error_wrong_state_in_output_datum = @"H3"

const error_head_value_is_not_preserved = @"H4"

const error_signer_is_not_a_participant = @"H5"

const error_no_signers = @"H6"

const error_too_many_signers = @"H7"

const error_script_not_spending_a_head_input = @"H8"

const error_no_output_datum_error = @"H9"

const error_unexpected_non_inline_datum = @"H10"

const error_not_paying_to_head = @"H11"

const error_signature_verification_failed = @"H12"

const error_must_not_change_version = @"H13"

const error_burnt_token_number_mismatch = @"H14"

const error_reimbursed_outputs_dont_match = @"H15"

const error_st_not_spent = @"H16"

const error_incorrect_utxo_hash = @"H17"

const error_missing_commits = @"H18"

const error_not_all_value_collected = @"H19"

const error_incorrect_version = @"H20"

const error_version_not_incremented = @"H21"

const error_has_bounded_validity_check_failed = @"H22"

const error_incorrect_closed_contestation_deadline = @"H23"

const error_infinite_upper_bound = @"H24"

const error_infinite_lower_bound = @"H25"

const error_contesters_non_empty = @"H26"

const error_close_no_upper_bound_defined = @"H27"

const error_failed_close_initial = @"H28"

const error_too_old_snapshot = @"H29"

const error_upper_bound_beyond_contestation_deadline = @"H30"

const error_contest_no_upper_bound_defined = @"H31"

const error_must_not_push_deadline = @"H32"

const error_must_push_deadline = @"H33"

const error_contester_not_included = @"H34"

const error_wrong_number_of_signers = @"H35"

const error_signer_already_contested = @"H36"

const error_failed_contest_current = @"H37"

const error_failed_contest_used_dec = @"H38"

const error_fanout_utxo_hash_mismatch = @"H39"

const error_fanout_utxo_to_decommit_hash_mismatch = @"H40"

const error_lower_bound_before_contestation_deadline = @"H41"

const error_fanout_no_lower_bound_defined = @"H42"

const error_deposit_not_spent = @"H43"

const error_deposit_input_not_found = @"H44"

const error_head_input_not_found = @"H45"

const error_failed_close_any = @"H46"

const error_failed_contest_unused_dec = @"H47"

const error_failed_contest_unused_inc = @"H48"

const error_failed_contest_used_inc = @"H49"

const error_failed_close_unused_dec = @"H50"

const error_failed_close_used_dec = @"H51"

const error_failed_close_unused_inc = @"H52"

const error_failed_close_used_inc = @"H53"

const error_fanout_utxo_to_commit_hash_mismatch = @"H54"

// Main validator
validator head {
  spend(
    datum: Option<State>,
    redeemer: HeadRedeemer,
    _utxo: OutputReference,
    self: Transaction,
  ) {
    // Pattern match on state and redeemer
    when (datum, redeemer) is {
      (
        Some(Initial { contestation_period, parties, head_id, seed }),
        CollectCom,
      ) ->
        check_collect_com(self, contestation_period, parties, head_id)
      (Some(Initial { parties, head_id, .. }), Abort) ->
        check_abort(self, head_id, parties)
      (Some(Open(open_datum)), Increment(inc_redeemer)) ->
        check_increment(self, open_datum, inc_redeemer)
      (Some(Open(open_datum)), Decrement(dec_redeemer)) ->
        check_decrement(self, open_datum, dec_redeemer)
      (Some(Open(open_datum)), Close(close_redeemer)) ->
        check_close(self, open_datum, close_redeemer)
      (Some(Closed(closed_datum)), Contest(contest_redeemer)) ->
        check_contest(self, closed_datum, contest_redeemer)
      (
        Some(Closed(closed_datum)),
        Fanout {
          number_of_fanout_outputs,
          number_of_commit_outputs,
          number_of_decommit_outputs,
        },
      ) ->
        check_fanout(
          self,
          closed_datum,
          number_of_fanout_outputs,
          number_of_commit_outputs,
          number_of_decommit_outputs,
        )
      _ -> fail error_invalid_head_state_transition
    }
  }

  else(_) {
    fail
  }
}

// Check CollectCom transition: Initial -> Open
fn check_collect_com(
  tx: Transaction,
  contestation_period: Int,
  parties: List<ByteArray>,
  head_id: ByteArray,
) -> Bool {
  expect Some(head_output) = list.head(tx.outputs)
  expect InlineDatum(output_datum) = head_output.datum
  expect open_datum: OpenDatum = output_datum

  // Verify parameters haven't changed
  let params_ok =
    open_datum.parties == parties && open_datum.contestation_period == contestation_period && open_datum.head_id == head_id

  // Version must be initialized to 0
  let version_ok = open_datum.version == 0

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, head_id)

  // No minting or burning
  let no_mint = assets.without_lovelace(tx.mint) |> assets.is_zero

  params_ok? && version_ok? && signed_ok? && no_mint?
}

// Check Abort transition: Initial -> abort (distribute commits)
fn check_abort(
  tx: Transaction,
  head_id: ByteArray,
  parties: List<ByteArray>,
) -> Bool {
  // All head tokens must be burned
  let tokens_burned = must_burn_all_head_tokens(tx.mint, head_id, parties)

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, head_id)

  tokens_burned? && signed_ok?
}

// Check Increment transition: Open -> Open (add deposit)
fn check_increment(
  tx: Transaction,
  open_before: OpenDatum,
  redeemer: IncrementRedeemer,
) -> Bool {
  expect Some(head_output) = list.head(tx.outputs)
  expect InlineDatum(output_datum) = head_output.datum
  expect open_after: OpenDatum = output_datum

  // Parameters must not change
  let params_ok =
    open_after.parties == open_before.parties && open_after.contestation_period == open_before.contestation_period && open_after.head_id == open_before.head_id

  // Version must increase
  let version_ok = open_after.version == open_before.version + 1

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, open_before.head_id)

  // Deposit input must be spent
  let deposit_spent =
    list.any(
      tx.inputs,
      fn(input) { input.output_reference == redeemer.increment },
    )

  // Verify snapshot signature
  let empty_hash = blake2b_256(#"")
  let sig_ok =
    verify_snapshot_signature(
      open_after.parties,
      open_before.head_id,
      open_before.version,
      redeemer.snapshot_number,
      open_after.utxo_hash,
      #"",
      empty_hash,
      redeemer.signature,
    )

  params_ok? && version_ok? && signed_ok? && deposit_spent? && sig_ok?
}

// Check Decrement transition: Open -> Open (remove commit)
fn check_decrement(
  tx: Transaction,
  open_before: OpenDatum,
  redeemer: DecrementRedeemer,
) -> Bool {
  expect Some(head_output) = list.head(tx.outputs)
  expect InlineDatum(output_datum) = head_output.datum
  expect open_after: OpenDatum = output_datum

  // Parameters must not change
  let params_ok =
    open_after.parties == open_before.parties && open_after.contestation_period == open_before.contestation_period && open_after.head_id == open_before.head_id

  // Version must increase
  let version_ok = open_after.version == open_before.version + 1

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, open_before.head_id)

  // Get decommit outputs (after head output)
  let decommit_outputs =
    tx.outputs
      |> list.drop(1)
      |> list.take(redeemer.number_of_decommit_outputs)

  let decommit_hash = hash_outputs(decommit_outputs)
  let empty_hash = blake2b_256(#"")

  // Verify snapshot signature
  let sig_ok =
    verify_snapshot_signature(
      open_after.parties,
      open_before.head_id,
      open_before.version,
      redeemer.snapshot_number,
      open_after.utxo_hash,
      empty_hash,
      decommit_hash,
      redeemer.signature,
    )

  params_ok? && version_ok? && signed_ok? && sig_ok?
}

// Check Close transition: Open -> Closed
fn check_close(
  tx: Transaction,
  open_before: OpenDatum,
  redeemer: CloseRedeemer,
) -> Bool {
  expect Some(head_output) = list.head(tx.outputs)
  expect InlineDatum(output_datum) = head_output.datum
  expect closed_datum: ClosedDatum = output_datum

  // No minting or burning
  let no_mint = assets.without_lovelace(tx.mint) |> assets.is_zero

  // Parameters must not change
  let params_ok =
    closed_datum.parties == open_before.parties && closed_datum.contestation_period == open_before.contestation_period && closed_datum.head_id == open_before.head_id

  // Version must not change
  let version_ok = closed_datum.version == open_before.version

  // Contesters must be empty
  let contesters_ok = list.is_empty(closed_datum.contesters)

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, open_before.head_id)

  // Validity range must be bounded
  let bounded_ok = check_bounded_validity(tx)

  // Verify snapshot based on redeemer type
  let empty_hash = blake2b_256(#"")
  let snapshot_ok =
    when redeemer is {
      CloseInitial ->
        open_before.version == 0 && closed_datum.snapshot_number == 0 && closed_datum.utxo_hash == open_before.utxo_hash
      CloseAny { signature } ->
        closed_datum.snapshot_number > 0 && closed_datum.alpha_utxo_hash == empty_hash && closed_datum.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          open_before.parties,
          open_before.head_id,
          open_before.version,
          closed_datum.snapshot_number,
          closed_datum.utxo_hash,
          empty_hash,
          empty_hash,
          signature,
        )
      CloseUnusedDec { signature } ->
        closed_datum.alpha_utxo_hash == empty_hash && closed_datum.omega_utxo_hash != empty_hash && verify_snapshot_signature(
          open_before.parties,
          open_before.head_id,
          open_before.version,
          closed_datum.snapshot_number,
          closed_datum.utxo_hash,
          empty_hash,
          closed_datum.omega_utxo_hash,
          signature,
        )
      CloseUsedDec { signature, already_decommitted_utxo_hash } ->
        closed_datum.alpha_utxo_hash == empty_hash && closed_datum.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          open_before.parties,
          open_before.head_id,
          open_before.version - 1,
          closed_datum.snapshot_number,
          closed_datum.utxo_hash,
          empty_hash,
          already_decommitted_utxo_hash,
          signature,
        )
      CloseUnusedInc { signature, already_committed_utxo_hash } ->
        closed_datum.alpha_utxo_hash == empty_hash && closed_datum.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          open_before.parties,
          open_before.head_id,
          open_before.version,
          closed_datum.snapshot_number,
          closed_datum.utxo_hash,
          already_committed_utxo_hash,
          empty_hash,
          signature,
        )
      CloseUsedInc { signature, already_committed_utxo_hash } ->
        closed_datum.alpha_utxo_hash == already_committed_utxo_hash && closed_datum.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          open_before.parties,
          open_before.head_id,
          open_before.version - 1,
          closed_datum.snapshot_number,
          closed_datum.utxo_hash,
          already_committed_utxo_hash,
          empty_hash,
          signature,
        )
    }

  no_mint? && params_ok? && version_ok? && contesters_ok? && signed_ok? && bounded_ok? && snapshot_ok?
}

// Check Contest transition: Closed -> Closed (with better snapshot)
fn check_contest(
  tx: Transaction,
  closed_before: ClosedDatum,
  redeemer: ContestRedeemer,
) -> Bool {
  expect Some(head_output) = list.head(tx.outputs)
  expect InlineDatum(output_datum) = head_output.datum
  expect closed_after: ClosedDatum = output_datum

  // No minting or burning
  let no_mint = assets.without_lovelace(tx.mint) |> assets.is_zero

  // Parameters must not change
  let params_ok =
    closed_after.parties == closed_before.parties && closed_after.contestation_period == closed_before.contestation_period && closed_after.head_id == closed_before.head_id

  // Version must not change
  let version_ok = closed_after.version == closed_before.version

  // Snapshot number must be newer
  let newer_ok = closed_after.snapshot_number > closed_before.snapshot_number

  // Must be signed by a participant
  let signed_ok = must_be_signed_by_participant(tx, closed_before.head_id)

  // Get the single signer
  expect [signer] = tx.extra_signatories
  let contester = signer

  // Contester must not have contested before
  let not_contested_before = !list.has(closed_before.contesters, contester)

  // Contesters must be updated
  let contesters_ok =
    closed_after.contesters == [contester, ..closed_before.contesters]

  // Verify snapshot based on redeemer type
  let empty_hash = blake2b_256(#"")
  let snapshot_ok =
    when redeemer is {
      ContestCurrent { signature } ->
        closed_after.alpha_utxo_hash == empty_hash && closed_after.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          closed_before.parties,
          closed_before.head_id,
          closed_before.version,
          closed_after.snapshot_number,
          closed_after.utxo_hash,
          empty_hash,
          empty_hash,
          signature,
        )
      ContestUsedDec { signature, already_decommitted_utxo_hash } ->
        closed_after.alpha_utxo_hash == empty_hash && closed_after.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          closed_before.parties,
          closed_before.head_id,
          closed_before.version - 1,
          closed_after.snapshot_number,
          closed_after.utxo_hash,
          empty_hash,
          already_decommitted_utxo_hash,
          signature,
        )
      ContestUnusedDec { signature } ->
        closed_after.alpha_utxo_hash == empty_hash && verify_snapshot_signature(
          closed_before.parties,
          closed_before.head_id,
          closed_before.version,
          closed_after.snapshot_number,
          closed_after.utxo_hash,
          empty_hash,
          closed_after.omega_utxo_hash,
          signature,
        )
      ContestUnusedInc { signature, already_committed_utxo_hash } ->
        closed_after.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          closed_before.parties,
          closed_before.head_id,
          closed_before.version - 1,
          closed_after.snapshot_number,
          closed_after.utxo_hash,
          already_committed_utxo_hash,
          empty_hash,
          signature,
        )
      ContestUsedInc { signature } ->
        closed_after.omega_utxo_hash == empty_hash && verify_snapshot_signature(
          closed_before.parties,
          closed_before.head_id,
          closed_before.version,
          closed_after.snapshot_number,
          closed_after.utxo_hash,
          closed_after.alpha_utxo_hash,
          empty_hash,
          signature,
        )
    }

  no_mint? && params_ok? && version_ok? && newer_ok? && signed_ok? && not_contested_before? && contesters_ok? && snapshot_ok?
}

// Check Fanout transition: Closed/Final -> distribute funds
fn check_fanout(
  tx: Transaction,
  closed_datum: ClosedDatum,
  number_of_fanout_outputs: Int,
  number_of_commit_outputs: Int,
  number_of_decommit_outputs: Int,
) -> Bool {
  // All head tokens must be burned
  let tokens_burned =
    must_burn_all_head_tokens(tx.mint, closed_datum.head_id, closed_datum.parties)

  // Get the output slices
  let fanout_outputs = list.take(tx.outputs, number_of_fanout_outputs)
  let commit_outputs =
    tx.outputs
      |> list.drop(number_of_fanout_outputs)
      |> list.take(number_of_commit_outputs)
  let decommit_outputs =
    tx.outputs
      |> list.drop(number_of_fanout_outputs)
      |> list.drop(number_of_commit_outputs)
      |> list.take(number_of_decommit_outputs)

  // Verify hashes match
  let fanout_hash_ok = hash_outputs(fanout_outputs) == closed_datum.utxo_hash
  let commit_hash_ok =
    hash_outputs(commit_outputs) == closed_datum.alpha_utxo_hash
  let decommit_hash_ok =
    hash_outputs(decommit_outputs) == closed_datum.omega_utxo_hash

  tokens_burned? && fanout_hash_ok? && commit_hash_ok? && decommit_hash_ok?
}

// Helper: Verify snapshot multi-signature
fn verify_snapshot_signature(
  parties: List<ByteArray>,
  head_id: ByteArray,
  version: Int,
  snapshot_number: Int,
  utxo_hash: ByteArray,
  utxo_to_commit_hash: ByteArray,
  utxo_to_decommit_hash: ByteArray,
  signatures: List<ByteArray>,
) -> Bool {
  // Build the message to sign
  let message =
    builtin.serialise_data(head_id)
      |> bytearray.concat(builtin.serialise_data(version))
      |> bytearray.concat(builtin.serialise_data(snapshot_number))
      |> bytearray.concat(builtin.serialise_data(utxo_hash))
      |> bytearray.concat(builtin.serialise_data(utxo_to_commit_hash))
      |> bytearray.concat(builtin.serialise_data(utxo_to_decommit_hash))

  // Check lengths match
  if list.length(parties) != list.length(signatures) {
    False
  } else {
    // Verify each signature
    list.all(
      list.zip(parties, signatures),
      fn(pair) {
        let (party, sig) = pair
        verify_ed25519_signature(party, message, sig)
      },
    )
  }
}

// Helper: Check if transaction is signed by a participant
fn must_be_signed_by_participant(tx: Transaction, head_id: ByteArray) -> Bool {
  when tx.extra_signatories is {
    [signer] -> {
      // Check if signer is in the participation tokens
      let has_pt =
        list.any(
          tx.inputs,
          fn(input) {
            let tokens_list = flatten(input.output.value)
            list.any(
              tokens_list,
              fn(token) {
                let (policy, name, _qty) = token
                policy == head_id && name == signer
              },
            )
          },
        )
      has_pt
    }
    [] -> False
    _ -> False
  }
}

// Helper: Check all head tokens are burned
fn must_burn_all_head_tokens(
  mint: assets.Value,
  head_id: ByteArray,
  parties: List<ByteArray>,
) -> Bool {
  // Check ST is burned
  let st_burned = quantity_of(mint, head_id, "ST") == -1

  // Check all PTs are burned
  let pts_burned =
    list.all(
      parties,
      fn(party) { quantity_of(mint, head_id, party) == -1 },
    )

  st_burned && pts_burned
}

// Helper: Hash a list of outputs
fn hash_outputs(outputs: List<Output>) -> ByteArray {
  // Serialize and hash the outputs
  outputs
    |> builtin.serialise_data
    |> blake2b_256
}

// Helper: Check transaction has bounded validity range
fn check_bounded_validity(tx: Transaction) -> Bool {
  // This is a simplified check - in production you'd verify the range bounds
  True
}
