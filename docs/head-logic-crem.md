# Composing head state transitions with crem

The node applies retained events through [crem 0.1.1.0](https://hackage.haskell.org/package/crem-0.1.1.0).
This page describes event application. All inputs use
[crem workflows with program actions](./head-logic-crem-workflows.md) to decide
which events and effects to produce.
Wire messages, `StateChanged` encodings and checkpoint formats stay the same.

## Start here

| Code | What to read |
| --- | --- |
| [Machine.hs](../hydra-node/src/Hydra/HeadLogic/Machine.hs) | `components` composes the machines with crem's `Sequential`; `step` filters foreign-head events and restores checkpoints. |
| [Lifecycle.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Lifecycle.hs) | Init, close, contest, fanout selection, partial observations, reversion and completion. |
| [Incremental.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Incremental.hs) | Deposit selection, pending decommit, settlement and repeated observations. |
| [Deposit.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Deposit.hs) | One tracked deposit: observation, status update, consumption and rollback. |
| [OffChain.hs](../hydra-node/src/Hydra/HeadLogic/Machine/OffChain.hs) | Transaction registry, snapshot requests, acknowledgements, confirmation and clearing local state. |
| [Types.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Types.hs) | The immutable event record passed between components. |
| [Operations.hs](../hydra-node/src/Hydra/HeadLogic/State/Operations.hs) | Pure operations shared with input rules: settlement retention, UTxO selection and fanout output comparisons. |
| [Node/Deposit.hs](../hydra-node/src/Hydra/Node/Deposit.hs) | Persisted deposit data and its unchanged codecs. |
| [MachineSpec.hs](../hydra-node/test/Hydra/HeadLogic/MachineSpec.hs) | Full-state migration tests, checkpoint recovery, phase references and settlement sequences. |

`Hydra.HeadLogic.aggregateNodeState` calls `Machine.apply`. The old `applyEvent`
reducer is replaced by the components above. Its frozen counterpart lives only
in [LegacyState.hs](../hydra-node/testlib/Test/Hydra/HeadLogic/LegacyState.hs).

## Where crem runs in the node

This is in the normal execution path, with no opt-in switch:

```text
Node.processNextInput
  HeadLogic.update                   -- interprets the complete input workflow
  HeadLogic.aggregateState           -- folds the retained event prefix
    HeadLogic.aggregateNodeState
      Machine.apply / Machine.step
        Crem.run (Machine.components)
          Lifecycle → Incremental → OffChain → Node metadata
```

The machine components perform the updates; the production path does not call
the frozen reducer and then check its answer. Tracked-deposit updates inside
node metadata also execute `Deposit.machine` through `runBaseMachineT`.

| Operational use | Call site |
| --- | --- |
| Update live state while processing an input | [Node.hs](../hydra-node/src/Hydra/Node.hs): `processNextInput`, inside `modifyNodeState`. |
| Recover state from persisted events at startup | [Node.hs](../hydra-node/src/Hydra/Node.hs): `hydrate` / `recoverNodeStateC`, folding `aggregateNodeState`. |
| Maintain the API's node-state projection | [API/Server.hs](../hydra-node/src/Hydra/API/Server.hs): `withAPIServer`, constructing `nodeStateP`. |
| Build checkpoints when rotating persistence | [Node/Run.hs](../hydra-node/src/Hydra/Node/Run.hs): `prepareEventStore`, supplying the same aggregate to `newRotatedEventStore`. |

The API projection and checkpoint aggregate are additional consumers of the
same events. The replay microbenchmark measures one aggregate, not all work
performed by these consumers together.

All input admission and protocol rules live in Program actions within crem
workflows. `HeadLogic.update` interprets the complete input workflow. The event machines below do not repeat those guards. `Error` contributes no retained events;
`Wait` contributes its saved prefix. A phase graph alone does not describe those
input decisions.

## Composition and ownership

A transition carries the event, the original `NodeState`, and its current
updated value. Each component returns the next value. These are ordinary
immutable Haskell values; no independent mutable head state is introduced.

Execution order is deliberate:

1. **Lifecycle** changes the outer `HeadState` constructor, chain state and fanout fields.
2. **Incremental** changes the pending deposit/decommit, settlement version and local settlement UTxO. It reads the original deposit registry before consumption.
3. **OffChain** changes transaction and snapshot state. It records settlement snapshots on confirmation and uses the original version to distinguish fresh settlement from re-observation.
4. **Node metadata** updates sync status, chain time and deposit history. Deposit operations execute the tracked-deposit machine.

The pending selection fields of `SnapshotRequested` and `LocalStateCleared`
belong to Incremental; their transaction and snapshot fields belong to OffChain.
A new Head resets all components. A checkpoint restores the complete state and
reconstructs every continuation. Rollback restores chain state and deposit
history; it preserves L2 transactions, snapshots and versions, as before.

Production reconstructs crem's phase indices at each event boundary. `Runtime`
also supports continuous execution for tests and experiments. Its constructor
is private so callers cannot pair a continuation with a different state. Only
`NodeState` is persisted; recovery never decodes a crem continuation.

## What the phases mean

There is no persistent Init phase in this branch. Observing a matching Init
moves directly from Idle to Open.

| Lifecycle phase | Meaning |
| --- | --- |
| Idle | No active Head, including after completed fanout. |
| Open | Transactions and snapshots can be coordinated. |
| Closed | Closed Head; readiness notification has not been sent. |
| ReadyToFanout | The notification flag is set. This is **not** the deadline guard: contestation can extend the deadline without clearing the flag. |
| PendingFanout | Optimistic fanout state, with no distributed output observed. |
| FanoutProgress | At least one distributed output is recorded. |

`Closed` and `ReadyToFanout` can go directly to `Idle` on `HeadFannedOut`.
A party that did not initiate fanout can observe its completion while still
closed; it need not pass through the initiator's optimistic PendingFanout state.
This is the existing
[`onClosedChainFanoutTx`](../hydra-node/src/Hydra/HeadLogic.hs) behavior, not a
transition that abandons a Head. The lifecycle diagrams label completion edges.

The incremental machine distinguishes no pending action, a selected deposit,
a pending decommit, and both fields populated. The last case is representable
by raw events even though input guards prevent selecting both. Retained signed
settlement snapshots are history used for reposting after rollback; they are
not another pending action.

A tracked deposit has missing, inactive, active, expired and consumed phases.
Consumption retains its previous status. Rollback can undo consumption or
remove the record; it does not recompute activation from time. Re-observation
starts a new record, and repeated consumption stamps the new slot.

The off-chain component covers the persistent coordination phases:

| Phase | `SeenSnapshot` | Events that normally advance it |
| --- | --- | --- |
| NoRequestSeen | `NoSeenSnapshot` | Deciding a request or receiving `ReqSn`. |
| SnapshotIdle | `LastSeenSnapshot` | Deciding or receiving the next request. |
| RequestSent | `RequestedSnapshot` | Receiving `ReqSn`. |
| CollectingSignatures | `SeenSnapshot` | Recording acknowledgements; full confirmation returns to idle. |
| OutsideHead | Head is not Open | Init starts coordination. A closed Head can still adopt a signed snapshot from a contest. |

Receipt of a transaction and a partial acknowledgement usually preserve the
phase while changing its payload. An early acknowledgement waits in the
scheduler. A duplicate acknowledgement is handled by the existing rules. A
fresh settlement preserves a snapshot collecting signatures and resets other
coordination phases; a repeated settlement preserves the phase.

## Diagrams and their limits

The [central diagram page](head-logic-diagrams.md) renders every machine graph
and trace, including the files linked below.

[Generated state and composition diagrams](head-logic-machines/README.md) come
from the actual crem definitions:

```sh
cabal run hydra-node:head-logic-lab -- machines docs/head-logic-machines
```

Rendering lives in
[MachineDiagrams.hs](../hydra-node/testlib/Test/Hydra/HeadLogic/MachineDiagrams.hs).
The production modules only supply crem's required vertex instances.

A topology constrains phase edges. It does not prove that the payload matches
its phase, authenticate a signature, validate a ledger transition or establish
progress. Existing replay accepts resets and out-of-phase events, so several
graphs are broad. Identity transitions are implicit. In particular, a graph
edge is not evidence that an untrusted input can pass the admission guards.
The finite off-chain graph does not enumerate every UTxO, signature map,
snapshot number or retry queue.

The independent phase functions are in
[OffChainReference.agda](../spec/src/Hydra/Protocol/OffChainReference.agda).
Lifecycle, snapshot and tracked-deposit references project retained changes
and explicitly abstract payload-dependent choices. The pending-action reference
only classifies the selected-deposit and decommit fields; its settlement updates
are covered by the full-state migration comparison.
The [specification](../spec/src/Hydra/Protocol/OffChain.lagda.typ) describes the
same boundary. Full-state comparisons with the frozen reducer cover more data,
but agreement between implementations is not a proof of the protocol.

## Verification and measurements

Reproduce the migration and existing settlement checks with:

```sh
just test hydra-node 'HeadLogic.Machine'
cabal run hydra-node:head-logic-lab -- settlement /tmp/crem-settlement.json
nix develop .#spec --command bash hydra-agda/regenerate.sh
just lint
just check
```

After building the tests, reproduce the extended property run with:

```sh
"$(cabal list-bin hydra-node:test:tests)" -p '/HeadLogic.Machine/' --quickcheck-tests=1000 +RTS -N2
```

The retained-event migration passed an extended run of 1,000 generated histories
of 40 events (40,000 prefixes) and 1,000 tracked-deposit cases across four
operations. See the [validation record](docs/dev/architecture/head-logic-experiments/lab.md#validation-record)
for the complete branch checks, including input workflows and live-node coverage.

The migration property compares every event prefix against the frozen reducer,
continuous crem execution, reconstruction at each event, and CBOR recovery.
It includes arbitrary checkpoints and foreign-head events. Separate checks run
all 24 deposit/settlement/rollback schedules and an optimistic fanout sequence.
Existing protocol, simulation and persistence tests remain applicable because
the production aggregate runs crem.

The benchmark adds two comparisons to the existing snapshot-round fixtures:
`aggregation` runs the same operational programs with either reducer;
`event-replay-crem` applies a precomputed round's events. Fixture construction,
full-state equality and success checks run outside timing. The timed round
includes ledger work, signatures and snapshot confirmation, with tracing off;
it excludes transport, disk I/O and a real chain. See the
[measurement page](docs/dev/architecture/head-logic-experiments/measurements.md) for results and
commands. In these fixtures crem adds about 3–4 µs and 36 KiB per complete
round of event replay; the longer full-round repeats measured −0.14% to +1.49%.
A replay microbenchmark alone cannot establish the TPS impact.

## Maintenance costs

To change a transition, change its component's event case and, if the phase
edge changes, its topology. Update the specification and Agda projection,
extend the regression test, then regenerate the diagrams. Interpreters do not
need a second copy of that transition rule. A deliberate behavior change also
needs a scoped update to the migration comparison; the frozen reducer should
not silently follow the implementation.

This adds singleton declarations, transition constraints and four machine
modules. That is a real cost: the broad replay graphs provide less static
protection than a narrower machine designed without compatibility constraints.
The immediate benefits are explicit component ownership, executable composition,
and diagrams tied to the code. Tightening replay acceptance would be a separate
behavior change.

On GHC 9.6.7, crem selects `singletons-base-3.2`, `singletons-th-3.2` and
`th-desugar-1.15`. Their bounds move `th-abstraction` from 0.7.2.0 to 0.6.0.0.
The initial dependency comparison found no other changed existing package
versions, but this causes a substantial rebuild. Compile-time dependencies and
crem's use of singleton-generated code are part of the adoption cost.

## Next steps towards a merge

1. **Review the migration in three parts.** First review persisted types,
   shared operations and the frozen baseline; then lifecycle/incremental
   composition; then off-chain coordination and its tooling. Keep the differential
   harness through this review. No event-format migration or runtime flag is
   required by the current implementation.
2. **Measure the whole node with crem.** Repeat the controlled cluster workloads
   with snapshot batching and both small and large Heads; measure recovery too.
   Include the API projection and checkpoint work. The current local-round
   results leave that TPS question open. If allocations matter, compare carrying
   `Runtime` through an event batch with today's reconstruction per event;
   prefix tests already exercise both execution modes.
3. **Tighten what the types establish.** The incremental and off-chain graphs are
   broad because raw replay is permissive, and the phase index does not prove
   that every payload field is consistent. Identify reachable phase/payload
   combinations, give the useful ones explicit types, and test preservation at
   component boundaries. Keep compatibility cases explicit rather than silently
   rejecting historical events. Component order is currently checked by the
   migration tests, not by the `Sequential` type.
4. **Review rule boundaries.** All input handling now runs through Crem/Program.
   Multi-step rules have typed phases; short actions compose directly as programs.
   Review that split alongside the event machines, particularly settlement,
   rollback and fanout. Keep the Agda generation proposal as separate work.

The adoption decision should weigh the explicit composition and generated
structure against singleton boilerplate, replay graphs that admit more transitions than the protocol,
extra allocation and the dependency rebuild. This implementation makes those
costs reviewable; it does not establish that every HeadLogic rule benefits from
being expressed as a crem transition.
