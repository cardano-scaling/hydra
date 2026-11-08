# Head logic programs

All input handlers express their decisions as typed programs. `updateProgram`
validates and dispatches an input; `update` is the single production interpretation
boundary, returning the existing `Outcome`, `StateChanged`, and `Effect` values.
State aggregation, network messages, and persistence use their existing formats.

## Implementation

- `Hydra.HeadLogic` contains the rules, including collision checks, snapshot
  selection, transaction pruning, and incremental-action approval.
- `Hydra.HeadLogic.Program` defines instructions for ledger application,
  signing, verification, state changes, effects, named decisions, and termination.
- `Hydra.HeadLogic.Interpreter` supplies the primitives and evaluates the program.
- `Hydra.HeadLogic.Version` holds opaque, version-indexed validation evidence.
- `Hydra.HeadLogic.Trace` describes the instructions observed during execution.
- `Hydra.HeadLogic.Retry` defines retry eligibility for the node queue.

`Program tx ledger signing a` records ledger and signing capabilities in its
type. Transaction programs can apply transactions; snapshot programs can also
sign; acknowledgement programs need neither capability. Verification is
available to all three. Complete handlers return `HeadProgram tx ()`, which allows
both capabilities. `promoteProgram` embeds the narrower transaction and
acknowledgement rules without changing their instructions or relaxing the types
inside those rules. Ledger implementations and signing operations are supplied
only at the `update` boundary.

Only the acknowledgement rule establishes a `handler` scope, because it uses
`finish` for successful early completion. The scope lives inside the narrow rule,
so both direct and promoted calls resume their caller. Other handlers complete
normally and need no scope.
Startup fanout recovery shares the pure `fanoutStepTx` constructor with the input
handlers; it produces a transaction without interpreting a second program or
applying state changes outside the input loop.

The Church encoding composes instructions without building an intermediate tree.
`foldProgram` supports other interpretations, including controlled primitive
responses in tests. Production execution and tracing share the same evaluator.

Snapshot requests and acknowledgements use named stages in ordinary `do`
notation. Each stage returns the data needed by the next stage, such as resolved
transactions or a signed snapshot. These stages compose existing instructions;
they need neither additional stage instructions nor a `Cont` wrapper. Signing
and ledger application still go through the interpreter. Request processing
stages the acknowledgement before pruning local transactions, preserving the
instruction order.

Shared predicates keep repeated protocol conditions consistent:
`hasPendingSnapshotAction` checks whether a commit or decommit is still pending
at the snapshot's version; `Version.matchSettlement` matches a settlement's
snapshot number and successor version; `canRequestSnapshot` checks idle leader
eligibility; and `depositsWithStatus` selects deposits by lifecycle status.
Version-bump requests retain their separate eligibility rule because they can
replace a pending request but cannot interrupt signature collection.

## Version evidence

Snapshot numbers and versions remain distinct. A successful header check returns
an existential `RequestVersion v` after the existing checks have run in their
original order. Funding, resolved transactions, applied transactions, and the
signed snapshot retain that same version index. Snapshot construction assigns
the validated version, and the signing helper keeps the snapshot paired with the
signature produced for it. Evidence constructors are private and version roles
are nominal, preventing callers from changing the index with `coerce`.

`SettledSnapshot` packages a snapshot at version `v` with its successor version
`v + 1`, after checking the observed snapshot number and version. Increment and
decrement adoption consume this matched pair; deposit identity and signature
verification remain separate checks in the rule.

These are internal validation types, not new wire or persistence formats. The
program still has its ordinary `Monad` instance. A version mismatch still waits;
re-observation and forward-version aggregation keep their existing comparisons.
The head can advance while an older in-flight snapshot finishes, and fanout still
uses its existing equality-based selection. No new rejection is introduced for
those cases. Version evidence does not by itself prove head or action identity.

## Completion, waiting, and rejection

State changes and effects are buffered during an attempt:

| Terminal result | Retained state changes | Released effects |
| --- | --- | --- |
| Completion | All, in order | All, in order |
| Wait | The staged prefix, in order | None |
| Rejection | None | None |

`finish` skips the remainder of the nearest `handler` and resumes its caller.
Nested handlers delimit their own successful completion. Without a handler scope,
`finish` ends the whole program. It does not commit buffers immediately: a later
`suspend` or `reject` still governs the entire update using the retention rules
above. Acknowledgement processing uses `finish` to ignore already confirmed
snapshots and to stop after recording a partial signature collection. Waiting
and rejection cross handler boundaries and terminate the entire update.

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

`ProgramSpec` checks terminal semantics, nested handler completion, capability
promotion, primitive arguments, tracing, alternate interpretations, and comparison
with the previous transaction handler.
`SnapshotProgramSpec` compares snapshot outcomes and resulting states with the
previous handlers across waits, deposits, decommits, pruning, and signatures.
Version-evidence tests combine deterministic numeric boundaries with shrinking
QuickCheck properties over values below and above the 64-bit boundary. Related
request and settlement versions are derived from each generated base version;
coverage checks require both numeric ranges to be exercised.
`HeadLogicSpec` retains the real Cardano witness-stripped duplicate regression.
The previous handlers are test fixtures; shared helpers limit their independence.

This branch contains the HeadLogic implementation and focused Haskell tests.
The development lab, benchmark artifacts, experimental recovery format, and
Agda/specification extensions are outside this iteration.
