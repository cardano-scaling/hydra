# Head logic programs

The open-head `ReqTx`, `ReqSn`, and `AckSn` handlers express their decisions as
typed programs. Their production wrappers interpret those programs into the
existing `Outcome`, `StateChanged`, and `Effect` values. State aggregation,
network messages, and persistence use their existing formats.

## Implementation

- `Hydra.HeadLogic` contains the rules, including collision checks, snapshot
  selection, transaction pruning, and incremental-action approval.
- `Hydra.HeadLogic.Program` defines instructions for ledger application,
  signing, verification, state changes, effects, named decisions, and termination.
- `Hydra.HeadLogic.Interpreter` supplies the primitives and evaluates the program.
- `Hydra.HeadLogic.Trace` describes the instructions observed during execution.
- `Hydra.HeadLogic.Retry` defines retry eligibility for the node queue.

`Program tx ledger signing a` records ledger and signing capabilities in its
type. Transaction programs can apply transactions; snapshot programs can also
sign; acknowledgement programs need neither capability. Verification is
available to all three. The rule environment contains party identity and the
deposit period. Signing keys remain in interpreter configuration.

The Church encoding composes instructions without building an intermediate tree.
`foldProgram` supports other interpretations, including controlled primitive
responses in tests. Production execution and tracing share the same evaluator.

## Completion, waiting, and rejection

State changes and effects are buffered during an attempt:

| Terminal result | Retained state changes | Released effects |
| --- | --- | --- |
| Completion | All, in order | All, in order |
| Wait | The staged prefix, in order | None |
| Rejection | None | None |

A received transaction remains registered while waiting for a prerequisite.
An invalid final multisignature discards the tentative signature change from
that attempt. A transaction whose retry budget expires records invalidation and
completes normally.

`RecordAppliedTransaction` supplies validated before/after UTxOs. The production
interpreter emits the existing applied-transaction event and leaves those
endpoints unevaluated. Aggregation reconstructs the UTxO through `applyTxTo`.

## Behavior and regression coverage

A `ReqTx` whose ID is already in both `localTxs` and `allTxs` completes without
changes, effects, or ledger application. This protects the validated registry
payload from a same-ID transaction with invalid witnesses and prevents duplicate
timeout from deleting a locally pending transaction.

The snapshot migration preserves guard ordering and existing event ordering,
including two signature events for a partial acknowledgement. Invalid early
signatures still prevent later replacement by the same party. Finite retry
expiry still requires client re-submission when a dependency arrives too late.

`ProgramSpec` checks terminal semantics, primitive arguments, tracing, alternate
interpretations, and comparison with the previous transaction handler.
`SnapshotProgramSpec` compares snapshot outcomes and resulting states with the
previous handlers across waits, deposits, decommits, pruning, and signatures.
`HeadLogicSpec` retains the real Cardano witness-stripped duplicate regression.
The previous handlers are test fixtures; shared helpers limit their independence.

This branch contains the HeadLogic implementation and focused Haskell tests.
The development lab, benchmark artifacts, experimental recovery format, and
Agda/specification extensions are outside this iteration.
