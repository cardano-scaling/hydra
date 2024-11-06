use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken/option
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  DatumHash, InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use commit
use util.{must_burn_st, trace_fail, trace_if_false}

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

// Mapping for the error types we also have in Haskell land
//  STNotBurned -> @"I01"
//  MissingOrInvalidCommitAuthor -> @"I02"
//  LockedValueDoesNotMatch -> @"I03"
//  MismatchCommittedTxOutInDatum -> @"I04"
//  CouldNotFindTheCorrectCurrencySymbolInTokens -> @"I05"
//  MultipleHeadTokensOrMoreThan1PTsFound -> @"I06"
//  MissingCommittedTxOutInOutputDatum -> @"I07"
//  CommittedTxOutMissingInOutputDatum -> @"I08"
//  MissingDatum -> @"I09"
//  UnexpectedNonInlineDatum -> @"I10"
//  ExpectedCommitDatumTypeGotSomethingElse -> @"I11"
//  ExpectedSingleCommitOutput -> @"I12"
//  WrongHeadIdInCommitDatum -> @"I13"
//  MintingOrBurningIsForbidden -> @"I14"
//  OutRefNotFound -> @"I15"

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
      ViaAbort -> trace_if_false(@"I01", must_burn_st(head_id, self.mint)?)
      ViaCommit(committed_refs) ->
        check_commit(head_id, commit_validator, committed_refs, utxo, self)
    }
  }

  else(_) {
    fail
  }
}

fn check_commit(
  head_id: PolicyId,
  commit_validator: ScriptHash,
  committed_refs: List<OutputReference>,
  utxo: OutputReference,
  self: Transaction,
) -> Bool {
  let tx_outputs = get_script_outputs(commit_validator, self)
  let locked_datums = tx_outputs |> list.map(fn(out) { out.datum })
  let commit_datum =
    when locked_datums is {
      [dat] ->
        when dat is {
          NoDatum -> trace_fail(@"I09")
          DatumHash(_dataHash) -> trace_fail(@"I10")
          InlineDatum(data) ->
            if data is commit.Datum {
              data
            } else {
              trace_fail(@"I11")
            }
        }
      _otherwise -> trace_fail(@"I12")
    }
  if commit_datum.commits is locked_commits: List<CommitDatum> {
    let committed_utxo =
      committed_refs
        |> list.map(
            fn(ref) {
              when
                self.inputs
                  |> transaction.find_input(ref)
              is {
                Some(i) -> Some(i)
                None -> trace_fail(@"I15")
              }
            },
          )
    let initial_value =
      self.inputs
        |> transaction.find_input(utxo)
        |> option.map(fn(i) { i.output.value })
        |> option.or_else(assets.zero)
    check_head_id(head_id, commit_datum.head_id) && must_be_signed_by_participant(
      head_id,
      initial_value,
      self.extra_signatories,
    ) && must_not_mint_or_burn(self.mint) && check_committed_value(
      committed_utxo,
      initial_value,
      tx_outputs,
    ) && check_locked_commit(committed_utxo, locked_commits)
  } else {
    trace_fail(@"Failed to decode locked_commits")
  }
}

fn get_script_outputs(commit_validator: ScriptHash, self: Transaction) {
  self.outputs
    |> transaction.find_script_outputs(commit_validator)
}

fn check_committed_value(
  committed_utxo: List<Option<Input>>,
  initial_value: Value,
  commit_outputs: List<Output>,
) -> Bool {
  let initial_lovelace =
    initial_value
      |> assets.lovelace_of
  let locked_value =
    commit_outputs
      |> list.map(fn(output) { output.value })
      |> list.reduce(assets.zero, assets.merge)
      |> assets.lovelace_of
  let committed_value =
    committed_utxo
      |> list.map(
          fn(i) {
            when i is {
              Some(input) -> input.output.value
              None -> assets.zero
            }
          },
        )
      |> list.reduce(assets.zero, assets.merge)
      |> assets.lovelace_of
  trace_if_false(
    @"I03",
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
    ([], _) -> trace_fail(@"I07")
    (_, []) -> trace_fail(@"I08")
    ([i, ..rest_committed], [c, ..rest_commits]) ->
      when i is {
        Some(input) ->
          trace_if_false(
            @"I04",
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
  trace_if_false(@"I13", self_head_id == committed_head_id)
}

fn must_be_signed_by_participant(
  head_id: PolicyId,
  initial_value: Value,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  let token_map =
    initial_value
      |> assets.tokens(head_id)
  when dict.size(token_map) is {
    0 -> trace_fail(@"I05")
    1 -> {
      let participation_tokens =
        token_map
          |> dict.to_pairs
          |> pairs.find_all(1)
      when participation_tokens is {
        [our_participation_token_name] ->
          trace_if_false(
            @"I02",
            signatories
              |> list.has(our_participation_token_name),
          )
        _more_than_one_token -> trace_fail(@"I06")
      }
    }
    _more_than_one_token -> trace_fail(@"I06")
  }
}

fn must_not_mint_or_burn(mint: Value) -> Bool {
  trace_if_false(@"I14", assets.is_zero(mint))
}
