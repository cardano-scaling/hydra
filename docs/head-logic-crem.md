# Applying retained events with crem

The input workflows choose events and effects. The event machines apply retained
`StateChanged` events for both live aggregation and recovery. Replay does not run
admission rules, validate transactions, sign or verify snapshots. Existing event
and checkpoint encodings are unchanged.

## Follow the connections

Start at [Machine.components](../hydra-node/src/Hydra/HeadLogic/Machine.hs):

```haskell
Basic (DepositRegistry.machine node.deposits)
  `Sequential` Basic (Lifecycle.machine node.headState)
  `Sequential` Basic (Clock.machine node)
```

The first port is `DepositOutput`: the updated registry, the event, and any deposit
value read **before** consumption. Lifecycle returns `HeadOutput`: its head state,
the registry projection and the event. Clock assembles these with its clock and
synchronisation fields into `NodeState`. The port definitions are in
[Machine/Types.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Types.hs).
No component receives a shared `before/current NodeState` to modify.

The `OpenHead` constructor in
[Lifecycle.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Lifecycle.hs) holds an
[Open.OpenHead](../hydra-node/src/Hydra/HeadLogic/Machine/Open.hs). That value owns
head parameters, chain context and the continuation of:

```haskell
Basic (Snapshots.machine ...)
  `Sequential` Basic (Transactions.machine ...)
```

[Coordinated.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) defines
this inner composition. `Open.step` decodes retained events into its commands.
Closing reads its projection into `ClosedState`; the closed phase then owns that
snapshot. Checkpoints reconstruct every owner together. Foreign-head filtering
happens before the composition; recovery of a deposit from a previous head is
intentionally exempt.

## Ownership

| Component | State it owns |
| --- | --- |
| [DepositRegistry](../hydra-node/src/Hydra/HeadLogic/Machine/DepositRegistry.hs) | Tracked deposits, including consumption slots and pruning. Each entry follows [Deposit](../hydra-node/src/Hydra/HeadLogic/Machine/Deposit.hs). |
| [Lifecycle](../hydra-node/src/Hydra/HeadLogic/Machine/Lifecycle.hs) | Idle, open context, closed and partial fanout payloads. The constructors carry their phase's data. |
| [Snapshots](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs) | Version, candidate, signatures, confirmation and retained settlement snapshots. |
| [Transactions](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs) | Local UTxO, transaction order and registry, selected deposit and pending decommit. |
| [Clock](../hydra-node/src/Hydra/HeadLogic/Machine/Clock.hs) | Observed chain time/slot and synchronisation status. |

`NodeState` is the persisted projection, not a second independently writable
state. The private runtime pairs that projection with its continuation. Node
aggregation currently rehydrates it per event; tests also keep the continuation
across complete event sequences. This distinction matters when measuring cost.

## Settlement and rollback

The registry passes a consumed deposit's UTxO to lifecycle. In the open head,
the snapshot owner updates the version and retained settlement evidence, then
passes `CommitSettled` or `DecommitSettled` to the transaction owner. Only a fresh
version adds a deposit or releases a decommit reservation. A repeated observation
refreshes the retained observation slot and preserves a newer pending decommit.

A fresh version preserves a candidate already collecting signatures, but clears
an outstanding request against the old version. Explicit adoption and local
clearing also go through the owners. Clearing retains a decommit only when the
confirmed snapshot carries it at the current version.

Rollback changes chain context and deposit history. It does not roll back L2
transactions or the snapshot version. The separate [Repost rule](../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs)
uses retained settlement evidence to repost an increment/decrement when rollback
crosses its observation slot. Re-observation refreshes that slot.

## What the phase types mean

Lifecycle distinguishes Idle, Open, Closed, ReadyToFanout, PendingFanout and
FanoutProgress. Init opens the head directly on this branch. ReadyToFanout records
a sent notification; a contest can extend the deadline without resetting it.
PendingFanout records an optimistic selection with no observed distributed output.

A Closed or ReadyToFanout node **can receive an observed `HeadFannedOut`** and enter
Idle: a follower need not have initiated fanout locally. The edge does not permit
client-triggered fanout before its time guards. Raw replay also permits `HeadOpened`
to replace an existing phase; the input rules impose the admission conditions.

Snapshot phases distinguish awaiting a request, request sent and collecting
signatures. The awaiting payload distinguishes no seen snapshot from a recorded
number. Reservation phases distinguish no pending decommit from one recorded.
Neither phase index proves ledger validity, absence of conflicts or progress.
Those properties need rules and independent checks.

## Tests and diagrams

[MachineSpec](../hydra-node/test/Hydra/HeadLogic/MachineSpec.hs) compares arbitrary
event prefixes with the frozen reducer, uninterrupted execution, rehydration and
CBOR recovery. It also checks settlement/rollback schedules and independent Agda
phase projections. [CoordinatedSpec](../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs)
checks live staging, evidence binding, wait/reject boundaries, lazy replay and
settlement ownership.

```sh
just test hydra-node 'crem event application'
just test hydra-node 'coordinated state owners'
cabal run hydra-node:head-logic-lab -- machines docs/head-logic-machines
python3 scripts/head-logic-diagrams.py
```

The [diagram index](head-logic-diagrams.md) renders the actual compositions and
phase graphs. The inner open-head composition is a separate graph because its
continuation lives in lifecycle's state. Generated graphs describe allowed replay
edges; they do not claim every edge is reachable from a valid network input.

See [Programs inside crem](head-logic-crem-workflows.md) for execution and
[the owner guide](head-logic-coordinated.md) for live staging. Independent settlement
backends and generated production Haskell remain outside this spike.
