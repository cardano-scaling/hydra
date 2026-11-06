# Adversarial review of Crem/Program

Reviewed on 2026-11-06 using crem 0.1.1.0, including the rebase onto master
`0aa8987dc` (PR #2927). Production Haskell and the formal model match the existing
capacity port before the rebase. The merged regression tests are retained and
pass through Crem/Program; both review witnesses still reproduce. This review
adds examples and documentation without changing production behavior.

The strongest parts are the executable Program rules, shared terminal semantics,
and shared live/replay state transitions. Crem makes ownership clearer, but the
current owner topologies exclude fewer mistakes than their diagrams suggest.
The work has found and fixed real bugs. **It has not fixed all of them:** this
review reproduces another decommit defect with the normal interpreter.

## Findings in priority order

| Priority | Finding | Consequence |
| --- | --- | --- |
| P1 | A confirmed conflicting transaction leaves an orphaned decommit reservation. | One party refuses subsequent valid decommits until local state is cleared. |
| P2 | Confirmation evidence is not bound to the Head's signing parties. | A mistaken caller can create evidence accepted by the owner without the required signatures. Current AckSn does not make this mistake. |
| P2 | Live commands and trusted replay commands share a port; refusal is often an empty event list. | The API permits bypassing admission and makes rejected transitions easy to overlook. |
| P2 | The owner topologies allow every phase-to-phase transition. | Their edge constraints do not establish the protocol properties readers may infer. |
| P2 | More phase scaffolding and a measured throughput cost accompany the sharing. | Human and runtime costs need further reduction before this is an obvious improvement. |

P1 is a reproduced protocol defect. The P2 API findings are demonstrated misuse
or design limitations, not newly demonstrated network exploits.

## Correctness: a remaining decommit bug

Start three parties with output 1. An ordinary transaction spends `1 → 2`; a
decommit spends `1 → 10`. Both requests are broadcast. Deliver them in different
orders:

| Stage | Alice | Bob | Carol |
| --- | --- | --- | --- |
| First requests | Applies the ordinary transaction and requests snapshot 1. | Accepts the decommit and reserves input 1. | Applies the ordinary transaction. |
| Other request arrives | Decommit waits. | Ordinary transaction waits. | Decommit waits. |
| Snapshot 1 arrives | Validates the ordinary transaction against the confirmed base. | Also validates it; rebase leaves the decommit recorded. | Also validates it. |
| All deliveries and retries finish | Confirms UTxO `{2}`. | Confirms UTxO `{2}`, still retaining the decommit of input 1. | Confirms UTxO `{2}`. |

The sequence takes 24 deliveries, uses production retry eligibility, drops no
messages, and uses real signature verification. Bob then rejects a valid decommit
of output 2 with `DecommitAlreadyInFlight 10`. Side-loading the existing confirmed
snapshot clears the orphan; the example checks this recovery too. Ordinary
transactions can still progress. This is not a demonstration of lost funds or
an unrecoverable Head.

The defect crosses three modules:

- [`Transactions.Rebase`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs#L74)
  retains the reservation unconditionally. Removing input 1 from the new UTxO
  has no effect because the confirmed transaction already consumed it.
- [`decommitStatus`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs#L136)
  calls every unavailable-input case `AwaitingInputs`. It cannot distinguish a
  dependency still to be confirmed from an input already spent by confirmed work.
- [`ReqDec.ExistingCheck`](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs#L119)
  consequently keeps refusing a replacement.

All 24 prefixes pass the compatibility harness's outcome, state, serialized
replay, and primitive-reply comparisons. The witness also checks the original
frozen reducer: this is an inherited defect. Equality with the old
implementation cannot establish correctness.

The existing reservation invariant misses it: absence of reserved inputs from
`localUTxO` is also true when a competing transaction has consumed them. The
general explorer excludes incremental actions; the decommit driver covers an
honest dependency sequence, not competing spends. The bounded Agda reservation
model does not establish eventual disposition of conflicting reservations.

The repair should distinguish a still-possible dependency from a conflict with
confirmed work, then release or invalidate the reservation consistently. A
missing input alone is not enough to decide this: legitimate decommits can
depend on unconfirmed transactions. Add this schedule and its permutations to
the incremental tests, including serialized recovery at every prefix. Check
that the next valid decommit succeeds and that the existing dependency-first
case still works. Update the spec and Agda with the conflict case alongside that
behavior change.

### Read and reproduce the trace

The [diagram](../head-logic-diagrams.md#conflicting-transaction-leaves-a-decommit-reservation)
contains the actual Program instructions, grouped by input and component.
Start with **ReqDec 10 to party 1**: it records the reservation. Then find
**ReqSn 0/1 txs [1] from party 0 to 1**: the ordinary transaction passes ledger validation and snapshot
rebase retains that reservation. The later confirmations succeed; no instruction
invalidates the conflicting decommit. The final assertions in
[`review-examples.hs`](review-examples.hs) check the resulting state and refusal,
which are not visible from instruction labels alone.

From the repository root:

```sh
cabal repl hydra-node:test:tests
```

In that REPL, whose working directory is `hydra-node`:

```text
:load ../docs/head-logic/review-examples.hs
runExamples
writeConflictTrace "../docs/head-logic/traces/conflicting-decommit.mmd"
:quit
```

Then regenerate the central page with `python3 scripts/head-logic-diagrams.py`.
This is a focused SimpleTx witness, not an exhaustive search, a minimized
counterexample, or a real Cardano ledger reproduction. Its assertions describe
the current defect; they must change when the defect is fixed.

### What the work has discovered and fixed

| Finding | Status and attribution |
| --- | --- |
| A witness-stripped duplicate replaces a validated registry payload. | Fixed by ignoring locally applied IDs before receipt. The spike has a real Cardano regression. |
| An expired duplicate deletes a transaction that remains locally applied. | Fixed by the same guard; tests cover every production TTL. |
| Rebase restores a pending decommit's reserved inputs. | Fixed in the transaction owner; the new conflict case shows why reservation preservation alone is insufficient. |
| A decommit is requested before its ordinary-transaction dependencies confirm. | Fixed by shared readiness and request selection; SimpleTx and Cardano regressions cover it. |
| An oversized snapshot proposal strands the request marker. | Fixed with bounded selection, pruning, and checked proposals. This was reported upstream in [PR #2927](https://github.com/cardano-scaling/hydra/pull/2927), not independently discovered by Crem. |
| Strict fields force UTxOs discarded by compatibility interpretation. | Spike-introduced performance defect, fixed with explicit laziness and a regression test. |
| Retry expiry strands dependent work; invalid partial acknowledgements block replacement. | Still present, deliberately deferred by the user. Re-submission and closing respectively provide exits. |
| Partial acknowledgements record a signature twice. | Still present; idempotent in the map reducer but redundant persisted work. |
| A confirmed conflict leaves a decommit reservation behind. | Newly reproduced by this review; still present. |

The [current checks](checks.md) and their linked tests describe these guarantees
and limits. The discoveries came from executable schedules, independent payload
invariants, and comparing ledger contexts. None was a compiler rejection of an
illegal crem edge.

## Ease of maintenance

The separation is useful. [`Program`](../../hydra-node/src/Hydra/HeadLogic/Program.hs)
contains instructions, [`Interpreter`](../../hydra-node/src/Hydra/HeadLogic/Interpreter.hs)
owns primitive execution and terminal semantics, and
[`Mermaid`](../../hydra-node/testlib/Test/Hydra/HeadLogic/Mermaid.hs) owns rendering.
Rules use ordinary `do` notation. There is no second handwritten set of rules
for traces, and production has no debugger or fault-injection switch.

The phase machinery is less economical. In
[`ReqTx`](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs), seven phases require
a phase enumeration, topology, singleton declarations, indexed state constructors,
and transition clauses. `CheckLedgerResult` exists to branch after an effect.
This is understandable once learned, but more work than reading the corresponding
Program. Several other rules repeat the structure. In
[`Input`](../../hydra-node/src/Hydra/HeadLogic/Rules/Input.hs), unpacking HeadState
into indexed states and reconstructing it for dispatch also buys limited static
protection.

Keep phases that establish useful data or ownership boundaries. Do not require
a crem phase for every guard just to enlarge the diagram. Prefer one shared
declaration helper or an upstream API improvement over another local rule DSL.
Give each substantial rule one documented path through its owner calls; small
modules help only if their names and connections explain the split.

There is also a strictness trap. [`Coordinated.Runtime`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs#L49)
must keep its state projection lazy so staging does not replay the ledger work
that event application will perform. A seemingly harmless strict field or
diagnostic inspection can change that cost. Keep the existing non-evaluation
tests and allocation benchmarks whenever this boundary changes.

## Duplication

There is less duplicated policy, but more supporting code. The important sharing
is request selection/capacity, collision checks, Program completion/wait/rejection,
and owner transitions used by live staging and retained-event application.

| Remaining repetition | Assessment |
| --- | --- |
| Client [`Decommit`](../../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs) and network [`ReqDec`](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) admission | Share the common ledger/output/capacity result, while leaving retry policy, deposit priority, and effect order explicit at the callers. |
| Leader/phase/useful-work checks around shared selection in ReqTx, AckSn, ReqDec, deposits and settlement | Selection is centralized; its triggers are not. A small eligibility helper could help, but their differences matter: ReqDec omits a request marker, while settlement can replace an older-version request. |
| Pure and Program transaction loops in [`Transactions`](../../hydra-node/src/Hydra/HeadLogic/Rules/Transactions.hs) | Two applications of the collision/ledger rule remain. The pure path supports reference comparisons; do not make the oracle call the implementation merely to reduce lines. |
| Frozen handlers, free programs, selectors and reducers in the test library | Deliberate migration duplication, several thousand lines. Set a removal milestone after review; preserve compact historical bug witnesses and independent properties. Some low-level helpers and codecs are already shared, limiting independence. |
| Ledger application followed by `applyTxTo` during persisted replay | Deliberate reconstruction under the unchanged event format, not repeated validation. Resolved replay remains an experiment. |
| Phase names, topology edges and state constructors | Parallel descriptions maintained by hand. Diagrams are generated, so they add no further handwritten rule copy. |

Admission rules and retained-event reducers have different jobs. Sharing the
state transition does not justify running admission again during recovery, or
removing independent checks in the ledger and on-chain validators.

## Type safety

### Verification evidence has an incomplete context

[`VerifiedSnapshot`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotVerification.hs#L16)
retains the snapshot, signatures and signed bytes, but not the keys against which
verification succeeded. `verifyCandidate` accepts caller-supplied keys.
`Coordinated.confirmSnapshot` passes bytes and signatures to the owner, which
only checks that the bytes match its current candidate.

The second example in `review-examples.hs` calls `verifyCandidate [] ... mempty`
using the real verifier, then stages confirmation in a three-party Head. It emits
`SnapshotConfirmed` with an empty multisignature. Independent verification with
the actual Head's keys returns `KeyNumberMismatch`. An empty set verifying
against an empty set is not itself a cryptographic bug; the missing context
binding is the problem.

Current [`AckSn`](../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs#L167), settlement
and side-loading supply the actual Head's keys. I found no network path that
substitutes the empty list. Nevertheless, the owner does not enforce the
stronger guarantee suggested by the opaque evidence type. Bind evidence to an
owner-issued candidate/context including the Head and ordered signing parties,
and check that context at consumption. Preserve cached signing bytes rather than
rehashing large UTxOs at every acknowledgement.

The ledger/signing indices on `Program` prevent using unavailable capabilities.
They do not prove that a transition has been validated. For example,
`Coordinated.applyTransaction` accepts arbitrary before/after UTxOs. The current
ReqTx caller obtains them from the ledger correctly; a narrower admitted-result
type would protect future callers. Public record construction of cached snapshot
fields is another in-process trust boundary. Serialized decoding rebuilds those
caches, so this is not evidence of a wire-format injection vulnerability.

### Phase safety and replay safety are different

[`Snapshots.snapshotTopology`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs#L30)
contains all six non-identity edges among three phases.
[`Transactions.transactionTopology`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs#L27)
contains both directions between its two phases. Crem always permits identity
edges. Consequently these two topologies exclude **no phase-to-phase
transition**. Their indexed payloads and ownership ports still help; input
workflow graphs are also more selective. [Crem topology definition](https://github.com/marcosh/crem/blob/main/src/Crem/Topology.hs).

Ordinary typed reducers could enforce the same separation of owner fields.
Crem's additional cost is justified where its topology excludes an illegal
sequence or its composition removes wiring that would otherwise be duplicated.
That case is stronger for the selective workflows than for these complete owner
graphs. Program and the independent checks account for most of the demonstrated
bug-finding value; those benefits do not depend on crem.

Replay and adoption explain some broad transitions. Separate the live command
interface from the trusted event adapter while keeping one underlying state
transition. Today [`fromEvent`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs#L139)
produces commands accepted by the same `stage` function, and `Program.change`
can emit a raw confirmation event. Internal constructor modules are hidden by
Cabal, but these exported paths still permit bypassing live evidence checks.

Refusal also needs a result type. Wrong-phase or mismatched owner commands often
produce `[]`, and `stage` returns a runtime without an acceptance indication.
[`SendSnapshot`](../../hydra-node/src/Hydra/HeadLogic/Rules/SendSnapshot.hs) explicitly
checks event presence; other callers do not. Return an `Accepted`/`Refused`
result and make dependent effects consume acceptance.

Do not narrow graphs by removing legitimate observations. In particular,
`Closed` and `ReadyToFanout` must reach `Idle` when a passive node observes another
party completing fanout. Client admission and chain observation are different
inputs, even when they affect the same lifecycle.

### Changes worth proposing to crem

1. **Allow an effect to select the destination phase.** `ActionResult` currently
   chooses its existential destination outside `m`. An effectful ledger reply
   therefore needs a separate result phase before branching. An alternative
   interface could return an existential transition inside the effect, retaining
   its allowed-edge proof. [BaseMachine API](https://github.com/marcosh/crem/blob/main/src/Crem/BaseMachine.hs).

   ```haskell
   data Next topology state from output where
     Next :: AllowedTransition topology from to
          => output -> state to -> Next topology state from output

   -- Schematic action signature:
   action :: state from -> input -> m (Next topology state from output)
   ```

2. **Separate execution from rendering constraints.** `StateMachine.Basic`
   requires singleton and rendering dictionaries even for execution. An optional
   renderable wrapper, or a declaration helper, could reduce each rule's
   singleton/import boilerplate. [StateMachine API](https://github.com/marcosh/crem/blob/main/src/Crem/StateMachine.hs).

3. **Make state inspection guarantees explicit.** Its `NoThunks` instance accepts
   `Basic _` without inspecting the base machine. That cannot certify a retained
   continuation's state. An explicit inspection/projection hook would help
   profiling and any future attempt to retain runtime machines between events.
   Hydra should still own checkpoint formats and protocol evidence.
   [StateMachine instance](https://github.com/marcosh/crem/blob/main/src/Crem/StateMachine.hs).

These are library proposals, not prerequisites for fixing the decommit bug.
The confirmation context and live/replay distinction belong in Hydra regardless.

## Security and operational cost

The important existing checks remain: authenticated network membership, ReqSn
ledger validation, and aggregate signature verification. Receipt in the registry
does not certify a transaction. Signing keys remain in interpreter configuration,
and fault injection is confined to the development library. Rejection after a
staged final signature discards the entire tentative prefix; waiting retains
changes but releases no buffered effects. These terminal semantics are shared.

The improvements are easier auditing, reproducible schedules and stronger
ownership boundaries. They are not stronger cryptography or a protocol proof.
The invalid-partial-acknowledgement case requires an authenticated party, which
can already withhold its signature in an n-of-n Head. The new decommit witness
needs no invalid signature. Both concern availability; neither demonstrates
an invalid production confirmation or theft.

Retained events and checkpoints remain trusted. The new types do not sanitize
old state. Replay reports can contain transaction and state data even though
they contain no signing keys; fixture traces are not evidence that arbitrary
production traces contain no sensitive application data.

There is a real operational tradeoff: recorded small-Head cluster runs lose
**3.54–7.82% TPS** and allocate **12.25–14.58% more per confirmed transaction**.
Larger Heads show a smaller relative cost; burst results are noisier and mostly
worse. The [measurements](measurements.md) do not support a general sub-5% claim.
Profile runtime reconstruction and staging allocations before retaining this
cost in production. These measurements do not establish an exploitable DoS.

## Review verification and next steps

Both reproduction examples run in the test REPL after rebasing. The 12 focused
upstream capacity checks also pass, including the three-party progress scenario
and the extracted Agda comparisons. See [capacity](capacity.md#merged-fix-and-regression-tests)
for the mapping from the merged fix to the current rules.

Before the rebase, the HeadLogic selection passed 452 of 453 tests in the REPL;
the fully-evaluated snapshot UTxO test reported a `TxIn` thunk in interpreted
mode. That same test passed in the compiled `-O1` binary. The mode discrepancy
remains to be explained; it is not evidence of a new production regression.
After the rebase, `just lint` and `just check` pass, including all 1,448 compiled
node tests. Unchanged package checks are reused. [Validation](validation.md)
records the checks separately.

Before treating the conversion as ready to merge: fix the conflicting reservation
with an independent regression and model update; bind confirmation evidence to
its committee; make owner refusal explicit and isolate replay authority; then
reduce phase boilerplate and profile the measured overhead. Keep the deferred
retry and acknowledgement policies documented until separate decisions change
them. None of those requires replacing Program or changing persistence formats.
