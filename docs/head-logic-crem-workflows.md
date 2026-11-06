# Programs inside crem

Every input goes through `HeadLogic.updateProgram`. It starts a fresh crem
workflow, whose actions expose Program instructions to the interpreter. Waiting
ends that attempt; a retry starts again against the current state. No crem
continuation is persisted or suspended.

## Execution

```text
HeadLogic.update
  Interpreter.runProgramWith (snapshotConfig environment ledger)
    updateProgram publicEnvironment now state input
      Workflow.run (Rules.Input.machine ...)
        input admission → phase-specific dispatch → rule actions
  aggregateState
    Machine.apply: lifecycle → incremental action → snapshot coordination → metadata
```

[`Rules/Input.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Input.hs) checks accumulator
bounds before any path can echo a client input. It then enforces synchronisation:
chain inputs are always admitted, clients are rejected while catching up, and
network inputs wait. Its phase constructors carry the corresponding Idle, Open,
Closed or FanoutProgress payload.

[`Rules/Client.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Client.hs),
[`Rules/Network.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Network.hs), and
[`Rules/Chain.hs`](../hydra-node/src/Hydra/HeadLogic/Rules/Chain.hs) route admitted
inputs. The node and bounded explorer use this same entry point. The older named
handler entry points in `HeadLogic.hs` remain thin wrappers for focused tests and
startup reposting; they do not duplicate the rules.

## Rules and their boundaries

| Module | Work it owns |
| --- | --- |
| [ReqTx](../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs) | Duplicate and collision checks, ledger application, retry/invalidation, next snapshot selection. |
| [ReqSn](../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs) | Request guards, deposit/decommit resolution, transactions, snapshot construction, signing, acknowledgement, pruning. |
| [AckSn](../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs) | Signature collection, verification, confirmation, settlement approval, next request. |
| [Decommit](../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs) / [ReqDec](../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) | Client admission / network reservation and snapshot request. |
| [Settlement](../hydra-node/src/Hydra/HeadLogic/Rules/Settlement.hs) | Record increment/decrement, verify a matching seen snapshot, adopt it, request work at the new version. |
| [Deposits](../hydra-node/src/Hydra/HeadLogic/Rules/Deposits.hs) / [Synchronisation](../hydra-node/src/Hydra/HeadLogic/Rules/Synchronisation.hs) | Common tick prefix, deposit activation/expiry, head-specific work, sync transitions. |
| [Recover](../hydra-node/src/Hydra/HeadLogic/Rules/Recover.hs) / [Repost](../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs) | Deposit recovery, rollback bookkeeping, and settlement reposting. |
| [Initialise](../hydra-node/src/Hydra/HeadLogic/Rules/Initialise.hs) / [Close](../hydra-node/src/Hydra/HeadLogic/Rules/Close.hs) | Init configuration, closing, signed snapshot adoption, counter-contesting. |
| [SideLoad](../hydra-node/src/Hydra/HeadLogic/Rules/SideLoad.hs) | Snapshot and L1 field checks, signature verification, local-state replacement. |
| [Fanout](../hydra-node/src/Hydra/HeadLogic/Rules/Fanout.hs) / [Selection](../hydra-node/src/Hydra/HeadLogic/Rules/Fanout/Selection.hs) | Full/selective fanout, observations, continuation and reposting / pure choice of the next transaction. |
| [SnapshotRequest](../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs) / [Transactions](../hydra-node/src/Hydra/HeadLogic/Rules/Transactions.hs) | Shared selection, output checks, transaction application and pruning. |

Multi-step rules have typed crem phases. Short actions compose directly as
programs inside the input workflow. There is no extra machine around a single
broadcast or state change. `SideLoad` uses `ExceptT` for sequential requirements;
a failure becomes the existing client effect, not a protocol `Reject`.

Crem's `ActionResult` selects its destination type outside the action. A primitive
reply therefore enters a result phase before the rule branches on it.
`ReqTx.CheckLedgerResult` is one example. `AckSn.ConfirmAndContinue` carries a
private `VerifiedRound`, built only after successful verification.

## Program and interpretation

[`Program.hs`](../hydra-node/src/Hydra/HeadLogic/Program.hs) provides typed ledger
and signature queries, named decisions, changes, effects, waits and rejection.
Its ledger/signing indices prevent a rule from using a primitive absent from its
configuration. `RuleEnvironment` contains public configuration only.

The Church encoding avoids constructing a conventional instruction tree on the
production path. `foldProgram` exposes instructions to tracing and replay.
Keeping Program inside crem lets these tools substitute primitive replies without
putting protocol decisions in an interpreter.

[`Interpreter.evaluateChanges`](../hydra-node/src/Hydra/HeadLogic/Interpreter.hs)
defines the terminal behavior once:

| Result | Retained events | Released effects |
| --- | --- | --- |
| Completion | All staged events, in order | All buffered effects, in order |
| Wait | Staged prefix, in order | None |
| Rejection | None | None |

The conversion preserves observable ordering. Partial acknowledgements record a
signature twice. A rejected final acknowledgement discards its staged signature.
ReqDec can emit ReqSn without recording `SnapshotRequestDecided`. Settlement
records finalization even when snapshot adoption fails. These are characterized
in tests; a refactor does not silently change them.

## Testing and inspection

[All diagrams](head-logic-diagrams.md) renders the machine graphs and concrete
executions. Graphs constrain phase changes; traces show the decisions and
instructions taken by one input. Neither is a proof of guard correctness.

```sh
cabal run hydra-node:head-logic-lab -- trace-inputs docs/docs/dev/architecture/head-logic-experiments/inputs +RTS -N1
cabal run hydra-node:head-logic-lab -- compare-crem docs/docs/dev/architecture/head-logic-experiments/workflows +RTS -N1
python3 scripts/head-logic-diagrams.py
```

[`InputComparison`](../hydra-node/testlib/Test/Hydra/HeadLogic/InputComparison.hs)
compares full inputs against frozen handlers and the frozen event reducer. It
checks ordered outcomes, complete state, CBOR event/checkpoint round trips, and
recorded primitive replay. The suite adds bounded arbitrary inputs and scheduled
prefixes; settlement/rollback sequences also use this comparison at every step.

The earlier [free-program comparisons](../hydra-node/testlib/Test/Hydra/HeadLogic/Crem/Comparison.hs)
check instruction order, query arguments and replies, staged payloads, and
terminal results. Pure selection helpers, primitives and codecs remain shared,
so these comparisons alone cannot establish correctness.

[`InputWorkflowSpec`](../hydra-node/test/Hydra/HeadLogic/InputWorkflowSpec.hs)
compares admission priority, client decommit failures, settlement adoption,
rollback boundaries and fanout continuation with extracted Agda functions.
Existing Agda comparisons cover the bounded snapshot round and retained phases.
They exclude ledger/crypto implementation, full payloads, fanout multiset
arithmetic, Haskell strictness, chain validity and distributed progress.

## Maintaining a rule

Change decisions and instruction order in the relevant rule module. Change its
phase type only when the transition structure or carried data changes. Update the
specification, bounded reference where applicable, examples, and generated
traces. An interpreter changes only when a new primitive is needed.

The [maintenance exercise](docs/dev/architecture/head-logic-experiments/lab.md#round-benchmarks-and-maintenance-exercise)
shows a guard change reaching execution, tests and diagrams without an
interpreter-specific rule edit. The [bug ledger](docs/dev/architecture/head-logic-experiments/bugs.md)
and [measurements](docs/dev/architecture/head-logic-experiments/measurements.md)
keep defects and costs separate from the design description.

## Shared state transitions

ReqTx, ReqDec, ReqSn, AckSn, settlement and side-loading stage their core events through the
[snapshot and transaction owners](head-logic-coordinated.md). AckSn carries the
tentative continuation from signature collection to confirmation. Program still
provides primitive queries and terminal semantics; the owner transitions produce
the events and next state together. Other input rules retain their existing
event instructions.

The later decommit fix changes shared request selection and transaction-owner
rebase, without changing the Program interpreter. Exact legacy comparisons exclude
those intentional differences; the [owner tests](head-logic-coordinated.md) and
historical/corrected sequences check the new behavior independently. Finite retry
expiry and invalid acknowledgement recovery are unchanged.
