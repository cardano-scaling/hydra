# Tracing and exploration

`head-logic-lab` executes the same input Programs and state aggregation as the
node. Tracing, recorded primitive replies, fault injection, and exploration live
in the development library. Production uses the normal interpreter with tracing
disabled.

## Read a snapshot round

Start with [Complete snapshot round](../head-logic-diagrams.md#complete-snapshot-round)
or generate one:

```sh
cabal run hydra-node:head-logic-lab -- trace /tmp/snapshot.json +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/snapshot.json +RTS -N1
```

The commands write a versioned scenario and adjacent Mermaid files. The default
scenario has three parties and two dependent transactions. `trace` follows FIFO
delivery and uses a 40-delivery limit unless overridden; it reports pending work
when it stops at that limit.

The [ReqTx examples](traces/README.md) cover individual successes, waits and
invalidations.

Read each sequence diagram downward. A shaded section is one input or scheduler
action. Lanes separate head logic, ledger replies, cryptography, staged changes,
and buffered effects within a node. Changes and effects are tentative until the
terminal note: completion retains both, waiting retains only changes, and
rejection discards both. The settlement diagram uses party lanes for whole inputs.
These terminal notes describe the logical outcome, not a durable disk flush.

## Reproduce a bounded failure

```sh
cabal run hydra-node:head-logic-lab -- explore /tmp/expiry.json --scenario expiry --search retry-windows +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/expiry.json --normal +RTS -N1
```

This schedule family varies an initial delivery prefix and each party's retry
window, then drains traffic. It finds the [retry-expiry limit](checks.md#recovery-limits)
within the default budget. This run visits 234 states and reports three depth
cutoffs and truncation. The [saved scenario](traces/expiry-search-v2.json) retains
the 22-delivery schedule. This is a bounded counterexample, not an exhaustive
search or a probability estimate.

The general explorer defaults to **24 deliveries per path and 100,000 states**.
It explores delivery order, optional duplication, delayed delivery/release, and
explicit retries using production `retryDecision`. State deduplication includes
protocol state, queues, disturbance budgets, confirmation history, and remaining
delivery depth; it excludes diagnostic traces. Exploration order is deterministic.

Options select the scenario (`dependent`, `duplicate`, `expiry`, or `capacity`),
search order, and delivery/state limits. Every report states its counts, depth
cutoffs, and truncation. `explore` writes a scenario file only when it finds a
failure. Incremental actions and chain events are outside its state space;
focused drivers cover those sequences.

Minimization repeatedly deletes individual scheduled actions while requiring
every remaining action to be enabled and the same failure to recur. The result
is minimized under this procedure, not globally minimal.

## Check the verifier boundary

```sh
cabal run hydra-node:head-logic-lab -- explore /tmp/invalid-confirmation.json --fault +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/invalid-confirmation.json +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/invalid-confirmation.json --normal +RTS -N1
```

The development-only transformation bypasses multisignature verification after
corrupting an acknowledgement. The independent invariant still uses the real
verifier and detects an invalid confirmation. Normal execution rejects the same
input sequence with `InvalidMultisignature`. There is no production fault flag.

## Inspect primitive replies and staged changes

```sh
cabal run hydra-node:head-logic-lab -- replay /tmp/snapshot.json --step +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/snapshot.json --break 'all parties have signed' +RTS -N1
```

Replay checks each primitive's identity and complete public arguments before
decoding its recorded reply. Missing, extra, mistyped, or mismatched replies fail.
Editing a reply permits a controlled experiment; later query arguments must still
match. Signing keys are absent from the scenario and traces.

`--step` inspects a prepared trace after reply validation; it does not suspend
node execution. `--break` selects a named decision. Step views include complete
staged payloads, even when a later rejection discards them. `--normal` executes
the saved schedule with real primitives instead of consuming the reply tape.

## Capacity, decommit, and settlement

```sh
cabal run hydra-node:head-logic-lab -- capacity /tmp/capacity.json +RTS -N1
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit.json +RTS -N1
cabal run hydra-node:head-logic-lab -- settlement /tmp/settlement.json +RTS -N1
```

Capacity confirms the independent merge in snapshot 5 after 30 deliveries.
Decommit confirms its dependency before starting the decommit round, reaching
snapshot 3 after 45 deliveries. Settlement checks 24 schedules varying confirmation,
rollback position, and party re-observation order. These drivers check event and
checkpoint serialization at every prefix. They use trusted chain observations
and SimpleTx, not a model of Cardano settlement validity or arbitrary chain forks.

These reports have their own schemas and are not generic replay tapes. For a
replayable capacity scenario, use `trace FILE --scenario capacity`.

## Generate current diagrams

```sh
cabal run hydra-node:head-logic-lab -- machines docs/head-logic-machines
cabal run hydra-node:head-logic-lab -- trace-inputs docs/head-logic/inputs +RTS -N1
cabal bench hydra-node:head-logic-program --benchmark-options='--trace docs/head-logic/traces'
python3 scripts/head-logic-diagrams.py
python3 scripts/head-logic-diagrams.py --check
```

For individual protocol topology graphs, `compare-crem /tmp/head-workflows`
generates `*-topology.mmd` alongside compatibility comparisons. Copy the topology
files to `docs/head-logic/workflows` when the phases change. The central index
requires an explanation for every `.mmd` and keeps archived diagrams separate.

Read [Program](../../hydra-node/src/Hydra/HeadLogic/Program.hs) and
[Interpreter](../../hydra-node/src/Hydra/HeadLogic/Interpreter.hs) for execution,
[Replay](../../hydra-node/testlib/Test/Hydra/HeadLogic/Replay.hs) and
[Debugger](../../hydra-node/testlib/Test/Hydra/HeadLogic/Debugger.hs) for inspection,
and [Mermaid](../../hydra-node/testlib/Test/Hydra/HeadLogic/Mermaid.hs) for rendering.

## Competing decommit and ordinary transaction

```sh
cabal run hydra-node:head-logic-lab -- decommit-conflict /tmp/decommit-conflict.mmd
```

This focused schedule uses real signature verification and production retries.
It checks event and checkpoint replay at every delivery. In the final AckSn to
party 1, confirmation emits `DecommitInvalid` for the competing reservation.
[The state-transition analysis](state-transitions.md#the-conflict-repair) explains
why release belongs at confirmation; the general explorer still excludes ReqDec.
