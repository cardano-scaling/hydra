# Snapshot programs, bounded exploration and resolved replay

Use `docs/head-logic-review.md` in the checkout for the implementation approach
and links to specific functions. This page is the runnable companion: commands,
expected results, recorded artifacts and validation.

The input workflows and retry policy run in `hydra-node`. The explorer, debugger
and resolved-replay experiment live in `testlib` and the `head-logic-lab` executable.
Network messages and persisted `StateChanged` encodings are unchanged.

## Complete input workflow

All inputs run through `HeadLogic.updateProgram`, including admission checks,
client commands, network messages and chain observations. Generate concrete
comparisons and phase graphs with:

```sh
cabal run hydra-node:head-logic-lab -- trace-inputs docs/docs/dev/architecture/head-logic-experiments/inputs +RTS -N1
cabal run hydra-node:head-logic-lab -- compare-crem docs/docs/dev/architecture/head-logic-experiments/workflows +RTS -N1
python3 scripts/head-logic-diagrams.py
```

The first command compares 31 examples with frozen handlers and event replay,
including event/checkpoint serialization and recorded primitive replies. The
second compares five individual workflows with their earlier implementations.
[All diagrams](../../../../head-logic-diagrams.md) renders the resulting `.mmd`
files with explanations; `python3 scripts/head-logic-diagrams.py --check` verifies
that the page covers every source and is up to date.

## Other demonstrations

For the peer demonstration, see `docs/docs/dev/architecture/head-logic-experiments/demonstration.md`. The additional
commands reproduce the real Cardano registry failure, discover the retry-expiry
failure in a focused search, and check 24 three-party settlement/rollback schedules:

```sh
cabal run hydra-node:head-logic-lab -- registry-demo /tmp/registry-demo.json
cabal run hydra-node:head-logic-lab -- explore /tmp/expiry.json --scenario expiry --search retry-windows
cabal run hydra-node:head-logic-lab -- replay /tmp/expiry.json --normal
cabal run hydra-node:head-logic-lab -- settlement /tmp/settlement.json
```

Registry and settlement exports contain complete outcomes and states. The expiry
export uses the existing scenario format and validates recorded primitive queries
on replay. Settlement uses trusted chain observations and the Simple ledger. It
does not extend the general explorer to chain events or model chain validity and
complete rollback histories in Agda.

## Decommit dependency sequence

```sh
cabal run hydra-node:head-logic-lab -- decommit-dependency --historical /tmp/decommit-old.json
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit-fixed.json
```

The frozen run stops at snapshot 1 after 24 deliveries. The corrected run confirms
snapshot 3 after 45 deliveries. Each writes its complete input/outcome/state report
and an adjacent Mermaid trace. These reports use the decommit demo format, not
the generic explorer replay format. See the [bug explanation](bugs.md#decommit-dependencies-and-reserved-inputs)
and [component diagrams](../../../../head-logic-diagrams.md#decommit-dependency-correction).

## Snapshot round

`onOpenNetworkReqSnProgram` and `onOpenNetworkAckSnProgram` expose the production
ReqSn and AckSn workflows. Their handlers choose primitive configuration and run the shared
interpreter. Ledger application, signing and byte-level multisignature checking
are typed instructions. The interpreter holds the signing key; instructions and traces contain only
public arguments and signatures.
The rules receive only public `RuleEnvironment` fields. Ledger/signing capability
indices make unsupported primitive configurations a type error. Collision
checks, incremental-action guards, pruning in sequence order, accumulator bounds,
snapshot construction, and increment/decrement/next-request selection remain in
the rule.

Every interpretation uses `evaluateChanges`: completion commits changes and
releases effects, waiting commits its prefix and discards effects, and rejection
discards both. The compatibility mapping turns the validated transaction change
into the existing event. No reducer is run twice inside a production handler.

`LegacySnapshot` freezes the previous handlers, sharing existing pure helpers.
Tests compare events, effects, errors and resulting node states in order.
It includes missing ids, version waits, deposit statuses, decommits, partial,
early and duplicate acknowledgements, invalid signatures, confirmation and the
next request. Existing HeadLogic and simulation tests remain applicable.

Two observable acknowledgement details are preserved: partial collection emits
`PartySignedSnapshot` twice, and the number guard precedes the redundant-ack
shortcut. The repeated event is a possible cleanup, not fixed here. Failed final
verification discards the signature staged before verification.

Agda remains independent. Its reference functions compare outcomes and ordered events/effects
for a subset of the protocol without incremental actions, output collisions,
accumulator overflow, pruning payloads or scheduler behavior. Existing independent
incremental-action guard references still apply. An explorer counterexample describes one
schedule; a passing search covers only the reported bounds.

## Reproduce a trace and counterexample

```sh
nix run .#head-logic-lab -- trace /tmp/snapshot.json
nix run .#head-logic-lab -- explore /tmp/invalid-confirmation.json --fault
nix run .#head-logic-lab -- replay /tmp/invalid-confirmation.json
nix run .#head-logic-lab -- replay /tmp/invalid-confirmation.json --break 'TEST ONLY: bypass multisignature verification'
nix run .#head-logic-lab -- replay /tmp/invalid-confirmation.json --step
nix run .#head-logic-lab -- replay /tmp/invalid-confirmation.json --normal
nix run .#head-logic-lab -- explore /tmp/corrected.json
```

Trace and counterexample commands write a version 2 JSON scenario and adjacent
Mermaid file. `explore` always prints its report and writes the requested scenario
file only when it finds a failure. Version 2 includes the entire starting world,
execution mode and disturbance policy. The v1 adapter uses the frozen v1 seed;
changed rule behavior can still invalidate historical tapes or retry schedules. The normal truncated run therefore need not
create `/tmp/corrected.json`. The initial scenario fixes three parties and two
dependent Simple-ledger transactions (`1 → 2 → 3`). The scheduler chooses delivery
order, an optional duplicate, an optional delay/release, and explicit retries.
Retries use the same `retryDecision` as the production queue. Delaying other
pending messages needs no clock advancement: the scenario has a fixed chain slot
and contains no timed settlement actions.

The generated sequence diagrams have lanes for head logic, ledger replies,
cryptography, staged changes and buffered effects. Unused lanes are omitted.
Read downward for execution order; each shaded section names the delivery,
retry or scheduler action. These are responsibilities within the recipient node,
not separate services. Changes and effects remain tentative until the terminal
note says what is committed or discarded. The settlement diagram instead uses
one lane per party, because it records whole updates rather than instructions.
Rendering lives in `Test.Hydra.HeadLogic.Mermaid`; the debugger supplies semantic
step categories, so changing a label cannot move an instruction to another lane.

Defaults are 24 deliveries per path and 100,000 visited states. `--deliveries N`
and `--states N` override them. Reports include both limits, visited counts,
depth cutoffs and truncation. A truncated or failure-stopped search is not
exhaustive. Deduplication uses serialized complete node states, queues, disturbance
budgets, confirmation history and remaining delivery depth; diagnostic traces are
excluded. Exploration order is deterministic. `--scenario dependent|duplicate|expiry`
selects the input family; `--search deliveries-first|retries-first|breadth-first`
selects DFS action priority or BFS. `--invalid-ack` permits bad input with normal
verification; `--fault` additionally selects the test-only bypass interpreter.
Malformed options and nonpositive limits fail explicitly.

The independent invariants verify confirmed signatures with the real verifier,
compare complete confirmed snapshots at equal version/number across history,
check input availability in confirmed transaction order against the previous
snapshot, require exact registry payload equality, and reconcile local UTxO
against the snapshot plus pending sequence using an independent ledger fold.
A scenario-specific progress check detects unconfirmed work after all queues
drain, provided no acknowledgement was corrupted. The fault demonstration corrupts one acknowledgement and transforms
only the development program to bypass verification. The invariant still calls
the real verifier. There is no production fault flag or alternative verifier.

Failures are minimized to a fixed point by deleting individual scheduled actions,
including disturbances, while requiring every remaining action to be enabled and
the same invariant failure to recur under the original interpreter. This is
minimized under that procedure, not globally minimal.

Replay checks each primitive's identity and complete public arguments before
decoding and consuming its reply. Missing, extra, wrongly typed or mismatched
replies fail. Editing a reply in the scenario provides controlled primitive
responses; subsequent query arguments must still match. Instruction views retain
staged state-change payloads even if a later rejection discards them. Recording
and replay walk the same typed `Cursor` exposed for live stepping in a REPL.
CLI `--step` steps a prepared trace, after all replies have been consumed; `--break` stops at a named rule
decision. `replay --normal` instead executes the schedule with real primitives
and verification, without consuming recorded primitive replies.

## Resolved transaction experiment

`ReqTx` supplies the transaction and its validated before/after UTxOs in
`RecordAppliedTransaction`. The compatibility interpreter emits the unchanged
`TransactionAppliedToLocalUTxO`. The development interpreter constructs removed
and added UTxO sets mechanically from the endpoints. Its log begins with
`hydra-reqtx-resolved-v2`; it binds the log to its head and retains invalidation
diagnostics. Translation rejects unsupported changes and wrong head IDs. Recovery
rejects non-open/wrong-head states and inconsistent deltas. No production
persistence format is modified. `compatibilityEvents` restores the original ReqTx
events without ledger work.

Recovery checks structural consistency and installs the explicit removals/additions and registry/sequence/request
updates. It does not perform ledger validation, call `applyTxTo`, sign, or select protocol actions.
Production replay already avoids validation; eliminating `applyTxTo`
reconstruction is the experiment's additional saving. Tests compare complete
states after every attempt, including serialized round trips, waits, timeout
invalidation, collisions and dependent transaction sequences.

```sh
nix run .#head-logic-lab -- resolved --json /tmp/resolved.json --regress allocated:iters +RTS -T -N1
```

The command prints encoded sizes for both formats and measures delta
construction, recovery and decoding plus recovery, with 100, 10,000 and 30,000
background outputs and a one-input/one-output transaction. Whole-state difference
cost is intentional. The current measurements show that this construction is expensive; the
experiment has not changed production persistence.

## Round benchmarks and maintenance exercise

The existing `head-logic-program` benchmark now includes `round/` groups for the
Simple and Cardano ledgers. Each runs ReqTx, ReqSn, all acknowledgements and state
aggregation, forcing confirmation and the local state projection. Equivalence
checks are outside timing and tracing is disabled. Set
`HEADLOGIC_BENCH_ORDER=program-first` to reverse each round pair. Run both orders
and repeat noisy cases; 5% is an investigation threshold, not a demonstrated
upper bound when intervals overlap it.

The [measurement report](measurements.md) records the separate round
and resolved-change workloads alongside the earlier ReqTx results.

```sh
HEADLOGIC_BENCH_ORDER=legacy-first nix run .#head-logic-program -- -m prefix round/ --json /tmp/round-a.json --regress allocated:iters +RTS -T -N1
HEADLOGIC_BENCH_ORDER=program-first nix run .#head-logic-program -- -m prefix round/ --json /tmp/round-b.json --regress allocated:iters +RTS -T -N1
```

For a small maintenance exercise, temporarily change the named ReqSn number
predicate in `Rules.ReqSn.machine` from `sn == seenSn + 1` to
`sn == seenSn + 2`. Run `SnapshotProgram` tests: the migration and independent
Agda comparisons reject this rule change. Generate the snapshot trace again: the
first ordinary snapshot request now records a failed named decision and rejection.
Neither interpreter nor diagram renderer needs a rule edit. Revert the predicate;
tests and the ordinary successful trace return. An intentional protocol change
would additionally update the independent specification/reference and acceptance
expectations instead of updating the frozen migration baseline to hide the change.

```sh
just test hydra-node SnapshotProgram
just test hydra-node HeadLogic
just test hydra-node OffChain
just test hydra-node CBOR
just test hydra-agda
nix develop .#spec --command hydra-agda/regenerate.sh
just lint
just check
```

When using the Nix test shell directly, run the binary from its package directory
so that the existing golden files are found:

```sh
(cd hydra-node && nix develop ..#hydra-node-tests --command tests -p '/HeadLogic/||/OffChain/||/CBOR/||/Behavior/||/Model/||/Node/' --hide-successes +RTS -N2 -RTS)
```

## Recorded demonstration

The current [snapshot round](traces/snapshot-round-v2.json.mmd)
and [counterexample](traces/counterexample-v2.json.mmd) have adjacent
self-contained v2 JSON tapes. The fault search visits 18 states; deletion reduces
17 actions to 10 (nine deliveries and one corruption). Normal execution rejects
`InvalidMultisignature`. The v1 snapshot/counterexample artifacts remain historical
fixtures for the reader; new exports use v2.

The normal retry-first expiry search visits 100,000 states and records 15,906 depth
cutoffs without finding a failure. It is truncated and misses the focused progress
witness. The corrected registry scenario and retained expiry limitation are
explained in `docs/docs/dev/architecture/head-logic-experiments/bugs.md`:

```sh
nix run .#head-logic-lab -- explore /tmp/expiry-search.json --scenario expiry --search retries-first
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-experiments/traces/registry-regression-v2.json --normal
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-experiments/traces/expired-dependency-v2.json --normal
```

The isolated maintenance mutation was executed during the initial spike: changing
only the ReqSn number predicate failed independent Agda comparison and produced
the [rejection trace](traces/maintenance-rejected.mmd). No interpreter
or renderer-specific rule edit was needed. The predicate was restored. Named Agda
outcomes now make such failures readable without consulting numeric tag mappings.

## Decommit dependency witness

The decommit demonstration runs both the frozen failure and the corrected rules:

```sh
cabal run hydra-node:head-logic-lab -- decommit-dependency --historical /tmp/decommit-old.json +RTS -N1
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit-fixed.json +RTS -N1
```

The historical schedule stops after 24 deliveries. The corrected schedule drains
45 deliveries, preserves the reservation, and confirms the decommit in snapshot
3. Both check serialized state at every prefix. They are focused schedules;
the generic explorer excludes incremental actions. See the
[bug record](bugs.md#decommit-dependencies-and-reserved-inputs).

The [capacity demonstration](capacity.md) shows another cross-rule failure:
selection creates a request that the follower must reject. `capacity --historical`
reproduces it; `capacity` checks the corrected split/prune/merge sequence.
`trace FILE --scenario capacity` writes a versioned replay scenario. The compact
capacity reports have their own schema and are not generic replay tapes.

## Validation record

### Composed owners and capacity (2026-11-06)

| Check | Result |
| --- | --- |
| Full `just check` | Passed: 1,437 node, 278 transaction, 116 Agda and 18 chain-observer tests; unchanged Plutus checks reused |
| Specification and extraction | Agda typechecking, PDF, trust-ledger checks and extraction drift passed |
| Focused capacity/workflow tests | 123 passed in a fresh test REPL; four compatibility properties also passed 1,000 cases each |
| Capacity demonstration | Frozen rules stall after nine deliveries; current rules confirm snapshot 5 after 30; independent audits and serialized replay hold at each prefix |
| Bounded capacity search | 100 states, limit 40 deliveries, no failure found; truncated at the state limit |
| Saved replay tapes | All five v2 tapes validate; their current instruction diagrams are regenerated |
| TUI | All 24 tests pass against the current Nix-built node with `TERM=xterm-256color TASTY_NUM_THREADS=1` |
| Complete-design measurements | Six local-round grids, four retained-event grids, and 16 three-node runs; all 60,000 cluster transactions confirm with no invalid/refused submissions and complete fanout |
| Diagram rendering | All 89 diagrams parse and render; the central page includes every `.mmd` |
| Imports | All 93 changed handwritten Haskell files use explicit or qualified imports, except the permitted Preludes |

The first TUI attempt failed because the shell lacked the `xterm-ghostty`
terminfo entry. The rerun selects an available terminal and the current node
binary explicitly; no test assertion or timeout changed.

The arbitrary-state comparisons now align the open Head's ID with its confirmed
snapshot. The checked request boundary deliberately rejects mismatched contexts;
separate regressions cover those rejections. Compatibility comparisons continue
to exclude the corrected decommit paths, and capacity decisions have independent
regressions. Frozen implementations remain unchanged.

The [performance report](measurements.md) records the regressions as well as the
passing runs. The small-Head and burst results do not meet a general 5% claim.
The local frozen-input baseline already uses free Programs; the whole-node
comparison against PR #2927 measures the complete design.

### Complete input conversion (2026-11-05)

| Check | Result |
| --- | --- |
| Compiled HeadLogic suite | 434 passed, including full-input comparisons and bounded Agda checks |
| Full node suite | 1,418 passed |
| Transaction, Agda and chain-observer suites | 277, 116 and 18 tests passed respectively |
| Specification | Typechecking and PDF build passed |
| `just lint`, formatting and unused-code checks | Passed; unused handler wrappers were removed |
| Settlement sequences | All 24 schedules pass frozen-handler comparison and serialized event/checkpoint replay at every step |
| Saved v2 scenarios | All five replay through the shared input entry point; normal execution rejects the injected invalid acknowledgement |
| Live three-node test | Init, Cardano transaction confirmation, close and fanout passed |
| TUI suite | All 24 tests passed |
| Generated Agda | Regeneration passed; the new bounded input decisions are exercised by node tests |
| Imports | All 73 Haskell files changed on this branch use explicit or qualified imports, except `Prelude` and `Hydra.Prelude` |
| Diagrams and links | All 83 `.mmd` files render in the central page; relative documentation links resolve |
| Complete-input benchmark | Four balanced grid runs and two longer large-UTxO repeats; see [measurements](measurements.md#complete-input-workflow) |

The generic explorer still covers only bounded snapshot-message schedules.
Broader chain, settlement and rollback coverage comes from focused comparisons;
the conversion does not turn those examples into an exhaustive search.

### Earlier program spike (2026-11-04)

| Check | Result |
| --- | --- |
| Focused program, snapshot and Cardano regression tests | 54 passed in the test REPL |
| Full `hydra-node` suite | 1,323 passed |
| `hydra-tx` suite | 277 passed |
| `hydra-agda` suite | 114 passed |
| `hydra-chain-observer` suite | 18 passed |
| Specification and generated Agda | Typechecking, PDF build and extraction freshness passed |
| `just lint` | Passed, including the build with `-Werror` |
| `just check` | Passed, including formatting and unused-code checks |
| Replay artifacts | Existing v2 scenarios replay; the registry demo, searched expiry scenario and settlement diagram reproduce byte for byte |
| Settlement sequences | All 24 schedules pass event and checkpoint replay |
| End-to-end comparison | 12 valid runs, 60,000 confirmed transactions and complete fanout; two balanced pairs in each of three cases |
| Local cluster funding | Seven Faucet tests pass; the large-output benchmark also checks complete fanout |

The focused tests cover the transaction map and local UTxO properties, confirmation
under fair delivery, old/new handler comparison, scenario versions, primitive
reply checks and resolved replay. Normal execution rejects the injected invalid
acknowledgement. The v1 snapshot and counterexample fixtures also replay through
the compatibility reader. Local documentation links were checked.

Golden files and production event encodings are unchanged. Nix test binaries must
run from the package directory, as above, to find golden files. The benchmarks
retain historical evidence separately; current measurements do not establish a
strict 5% performance bound.
