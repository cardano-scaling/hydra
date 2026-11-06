# Snapshot and transaction owners

This extends the Crem/Program design into state application. ReqTx, ReqDec, ReqSn,
AckSn, settlement and side-loading produce their core `StateChanged` events by running the same transitions
that replay uses. The wire and persistence formats stay unchanged. The known
decommit bugs remain reproducible, pending the separate behavior change.

## Read these files

| File | Responsibility |
| --- | --- |
| [Coordinated.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) | The composition, opaque continuation, live staging and legacy-event adapter. |
| [Snapshots.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs) | Own the candidate, signatures, confirmed snapshot and settlement retention, version changes and explicit adoption. |
| [Transactions.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs) | Own local UTxO, transaction order, registry, deposit selection and decommit reservation. |
| [Types.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Types.hs) | Internal command and response types; no `NodeState` passes between owners. |
| [SnapshotVerification.hs](../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotVerification.hs) | Produce opaque confirmation evidence from the verification primitive. |
| [CoordinatedSpec.hs](../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs) | Staging/replay/recovery agreement, terminal behavior, evidence binding and bounded invariant checks. |

The executable composition is simply:

```haskell
Basic (Snapshots.machine ...)
  `Sequential` Basic (Transactions.machine ...)
```

The snapshot owner emits a `Rebase snapshot retainedTransactions deposit` message.
It has no local UTxO field to update. The transaction owner decides how to rebuild
that UTxO and returns its own state. Both next states and persistence events come
from these transitions. Deposit settlement passes a typed ledger command too.
The outer composition also uses owned ports; no shared `NodeState` passes between
its components.

`Sequential` expresses the dependency here. There is no internal feedback loop
to close: network acknowledgements and scheduler retries remain later inputs.

## Where it runs

In [ReqTx](../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs), receipt, application
and invalidation share the owner continuation. A successful ledger reply supplies
the validated UTxO; the compatibility event keeps its existing encoding.
In [ReqDec](../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs), admission and ledger
checks precede `stage ... (reserveDecommit tx)`. In
[ReqSn](../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs), validation, signing and
pruning precede `stage ... (acceptSnapshot ...)`. In
[AckSn](../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs), the tentative continuation
carries signature collection through verification, confirmation and selection of
the next snapshot. The partial-ack path still emits its signature event twice.

`stage` lowers accepted events to ordinary Program instructions. Completion
retains events and effects; waiting retains only the event prefix; rejection
discards both. A rejected continuation never replaces the node state. Signing
keys remain in the interpreter. Existing instruction traces and recorded-query
replay continue to work.

[Machine.step](../hydra-node/src/Hydra/HeadLogic/Machine.hs) applies every retained
event through DepositRegistry → Lifecycle → Clock. While open, lifecycle routes
its commands through these owners, including settlement, rollback context,
deposit selection/recovery, side-loading and clearing. Closed-state adoption
belongs to lifecycle. [The event guide](head-logic-crem.md) follows the connections.
Foreign-head filtering and checkpoint restoration happen at the outer boundary.

Only `NodeState` is persisted. The node's aggregate currently reconstructs a
runtime for each event; tests also run a continuation across event sequences.
Live staging can discard the resulting state, so the transaction projection is
lazy: staging a snapshot does not also replay all its local transactions.

## What this protects

The snapshot owner cannot assign transaction fields through its port. A live
confirmation command requires an opaque verification result, and its signed
bytes must match the current candidate. A candidate cannot be confirmed before
it exists. These are narrower, reviewable boundaries than unrestricted event
construction in each rule.

They are not a proof of the protocol. Verification still trusts the interpreter,
the signable-byte cache and the rule's party-key selection. Replay deliberately
trusts retained events; its adapter can reconstruct a confirmation without
cryptography or demanding cached signing bytes. Other rules still have access to `Program.change`. The reserved
phase says a decommit is recorded, not that every UTxO update respects it.

That last distinction matters. The
[decommit demo](docs/dev/architecture/head-logic-experiments/bugs.md) still catches:

1. Rebase restores output 3 while retaining the decommit that reserves it. The
   fix belongs in the transaction owner's rebuild operation.
2. The next leader requests tx2 and a decommit spending tx2's output together.
   ReqSn applies the decommit first. Snapshot work selection needs to distinguish
   a recorded decommit from one eligible against the confirmed base.

The demo's JSON now includes `reservationHeld` and `decommitRequestsReady` checks.
[CoordinatedAudit.hs](../hydra-node/testlib/Test/Hydra/HeadLogic/CoordinatedAudit.hs)
states them independently for SimpleTx; the Agda reference agrees over all
subsets of three output identities. These are development checks, not new
production guards. Cardano eligibility also involves reference inputs,
collateral and time validity; a check based only on consumed outputs is not a
complete replacement for ledger applicability.

## Reproduce and extend

```sh
just test hydra-node 'coordinated state owners'
cabal run hydra-node:head-logic-lab -- machines /tmp/head-machines
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit.json
```

The [diagram index](head-logic-diagrams.md) renders both the generated composition
and a component-by-component explanation of the decommit failure.

For the separate bug fix, first change rebase in `Transactions.hs` and turn the
reservation counterexample into a passing invariant. Then define eligible
decommit work and use it at every leader request site, including the wake-up
after dependencies confirm. Update the spec, Agda predicate tests and trace;
the Program interpreter needs no rule edit. Keep the frozen failure as historical
evidence and make intentional migration differences explicit.

Settlement now uses these commands, including adoption after an on-chain
observation overtakes the final acknowledgement. Tests compare repeated
observations, reservation release and rollback at every prefix. The independent
Agda model additionally checks version and reservation changes over bounded
versions; it does not model settlement payload identity.

Measure complete rounds and whole-node workloads before drawing a TPS conclusion;
earlier measurements predate these owners.
