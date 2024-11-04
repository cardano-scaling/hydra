use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash}
use aiken/option
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, OutputReference, Transaction,
}
use commit

pub type Datum =
  PolicyId

pub type CommitDatum {
  input: OutputReference,
  pre_serialized_output: ByteArray,
}

pub type Redeemer {
  ViaAbort
  ViaCommit { committed_refs: List<OutputReference> }
}

// ^ Points to the committed Utxo.

const hydra_head_v1: ByteArray = "HydraHeadV1"

type InitialError {
  STNotBurned
  MissingOrInvalidCommitAuthor
  LockedValueDoesNotMatch
  MismatchCommittedTxOutInDatum
  CouldNotFindTheCorrectCurrencySymbolInTokens
  MultipleHeadTokensOrMoreThan1PTsFound
  MissingCommittedTxOutInOutputDatum
  CommittedTxOutMissingInOutputDatum
  MissingDatum
  UnexpectedNonInlineDatum
  ExpectedCommitDatumTypeGotSomethingElse
  ExpectedSingleCommitOutput
  WrongHeadIdInCommitDatum
  MintingOrBurningIsForbidden
  OutRefNotFound
}

fn to_error_code(err: InitialError) -> String {
  when err is {
    STNotBurned -> @"I01"
    MissingOrInvalidCommitAuthor -> @"I02"
    LockedValueDoesNotMatch -> @"I03"
    MismatchCommittedTxOutInDatum -> @"I04"
    CouldNotFindTheCorrectCurrencySymbolInTokens -> @"I05"
    MultipleHeadTokensOrMoreThan1PTsFound -> @"I06"
    MissingCommittedTxOutInOutputDatum -> @"I07"
    CommittedTxOutMissingInOutputDatum -> @"I08"
    MissingDatum -> @"I09"
    UnexpectedNonInlineDatum -> @"I10"
    ExpectedCommitDatumTypeGotSomethingElse -> @"I11"
    ExpectedSingleCommitOutput -> @"I12"
    WrongHeadIdInCommitDatum -> @"I13"
    MintingOrBurningIsForbidden -> @"I14"
    OutRefNotFound -> @"I15"
  }
}

// | The v_initial validator verifies that:
//
//   * spent in a transaction also consuming a v_head output
//
//   * ensures the committed value is recorded correctly in the output datum
//
//   * ensures that the transaction was signed by the key corresponding to the
//     PubKeyHash encoded in the participation token name
//
// NOTE: It does not need to ensure that the participation token is of some
// specific Head currency.
validator initial(commit_validator: ScriptHash) {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    utxo: OutputReference,
    self: Transaction,
  ) {
    expect Some(head_id) = datum
    when redeemer is {
      ViaAbort -> must_burn_st(head_id, self.mint)
      ViaCommit(committed_refs) ->
        check_commit(head_id, commit_validator, committed_refs, utxo, self)
    }
  }

  else(_) {
    fail
  }
}

fn must_burn_st(head_id: PolicyId, mint: Value) -> Bool {
  let quantity =
    mint
      |> assets.quantity_of(head_id, hydra_head_v1)
  trace_if_false(to_error_code(STNotBurned), quantity == -1)
}

fn check_commit(
  head_id: PolicyId,
  commit_validator: ScriptHash,
  committed_refs: List<OutputReference>,
  utxo: OutputReference,
  self: Transaction,
) -> Bool {
  let committed_utxo =
    committed_refs
      |> list.map(
          fn(ref) {
            self.inputs
              |> transaction.find_input(ref)
            //  |> option.(trace_fail(to_error_code(OutRefNotFound)))
          },
        )
  let locked_datums =
    self.outputs
      |> transaction.find_script_outputs(commit_validator)
      |> list.map(fn(out) { out.datum })
  let commit_datum =
    when locked_datums is {
      [dat] ->
        when dat is {
          NoDatum -> trace_fail(to_error_code(MissingDatum))
          DatumHash(_dataHash) ->
            trace_fail(to_error_code(UnexpectedNonInlineDatum))
          InlineDatum(data) ->
            if data is commit.Datum {
              data
            } else {
              trace_fail(to_error_code(ExpectedCommitDatumTypeGotSomethingElse))
            }
        }
      _otherwise -> trace_fail(to_error_code(ExpectedSingleCommitOutput))
    }
  let initial_value =
    self.inputs
      |> transaction.find_input(utxo)
      |> option.map(fn(i) { i.output.value })
      |> option.or_else(assets.zero)
  if commit_datum.commits is locked_commits: List<CommitDatum> {
     check_committed_value(commit_validator, committed_utxo, initial_value, self)
     && check_locked_commit( committed_utxo, locked_commits,)
     && check_head_id(head_id, commit_datum.head_id)
     && must_be_signed_by_participant( head_id, initial_value, self,)
     && must_not_mint_or_burn(self.mint)
   } else {
     trace_false(@"Failed to decode locked_commits")
   }
}

fn check_committed_value(
  commit_validator: ScriptHash,
  committed_utxo: List<Option<Input>>,
  initial_value: Value,
  self: Transaction,
) -> Bool {
  let initial_lovelace =
    initial_value
      |> assets.lovelace_of
  let locked_value =
    self.outputs
      |> transaction.find_script_outputs(commit_validator)
      |> list.map(fn(output) { output.value })
      |> list.reduce(assets.zero, assets.merge)
      |> assets.lovelace_of
  let committed_value =
    committed_utxo
      |> list.map(fn(i) {
           when i is {
             Some(input) -> input.output.value
             None -> assets.zero
           }
         })
      |> list.reduce(assets.zero, assets.merge)
      |> assets.lovelace_of
  trace_if_false(
    to_error_code(LockedValueDoesNotMatch),
    // NOTE: Ada in initialValue is usually lower than in the locked ADA due
    // to higher deposit needed for commit output than for initial output
    locked_value >= initial_lovelace + committed_value,
  )
}

fn check_locked_commit(
  committed_utxo: List<Option<Input>>,
  locked_commits: List<CommitDatum>,
) -> Bool {
  when (committed_utxo, locked_commits) is {
    ([], []) -> True
    ([], _) -> trace_false(to_error_code(MissingCommittedTxOutInOutputDatum))
    (_, []) -> trace_false(to_error_code(CommittedTxOutMissingInOutputDatum))
    ([i, ..rest_committed], [c, ..rest_commits]) ->
      when i is {
        Some(input) ->
          trace_if_false(
            to_error_code(MismatchCommittedTxOutInDatum),
            builtin.serialise_data(input.output) == c.pre_serialized_output && input.output_reference == c.input && check_locked_commit(
              rest_committed,
              rest_commits,
            ),
          )
        None -> False
     }
  }
}

fn check_head_id(self_head_id: PolicyId, committed_head_id: PolicyId) -> Bool {
  trace_if_false(
    to_error_code(WrongHeadIdInCommitDatum),
    self_head_id == committed_head_id,
  )
}

fn must_be_signed_by_participant(
  head_id: PolicyId,
  initial_value: Value,
  self: Transaction,
) -> Bool {
  let token_map =
    initial_value
      |> assets.tokens(head_id)
  when dict.size(token_map) is {
    0 ->
      trace_false(to_error_code(CouldNotFindTheCorrectCurrencySymbolInTokens))
    1 -> {
      let participation_tokens =
        token_map
          |> dict.to_pairs
          |> pairs.find_all(1)
      when participation_tokens is {
        [our_participation_token_name] ->
          trace_if_false(
            to_error_code(MissingOrInvalidCommitAuthor),
            self.extra_signatories
              |> list.has(our_participation_token_name),
          )
        _more_than_one_token ->
          trace_false(to_error_code(MultipleHeadTokensOrMoreThan1PTsFound))
      }
    }
    _more_than_one_token ->
      trace_false(to_error_code(MultipleHeadTokensOrMoreThan1PTsFound))
  }
}

fn must_not_mint_or_burn(mint: Value) -> Bool {
  trace_if_false(
    to_error_code(MintingOrBurningIsForbidden),
    assets.is_zero(mint),
  )
}

// Helpers
fn trace_false(trace_log: String) -> Bool {
  trace trace_log
  False
}

fn trace_if_false(trace_log: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace_false(trace_log)
  }
}

fn trace_fail(trace_log: String) -> a {
  trace trace_log
  fail
}
