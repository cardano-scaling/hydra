# Snapshot and transaction owners

ReqTx, ReqDec, ReqSn, AckSn, settlement, and side-loading produce their core
`StateChanged` events by running the same transitions that replay uses. The wire
and persistence formats stay unchanged. The transaction owner preserves decommit
reservations, and shared selection waits for confirmed dependencies. Confirmation releases
conflicting reservations; current-version certificates block replacement.
[Choosing the state transitions](head-logic/state-transitions.md) explains why
local intent and settlement authority are distinct phases.

## Read these files

| File | Responsibility |
| --- | --- |
| [Coordinated.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) | The composition, opaque continuation, live staging and legacy-event adapter. |
| [Snapshots.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs) | Own the candidate, signatures, confirmed snapshot and settlement retention, version changes and explicit adoption. |
| [Transactions.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs) | Own local UTxO, transaction order, registry, deposit selection and decommit reservation. |
| [Types.hs](../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Types.hs) | Internal command and response types; no `NodeState` passes between owners. |
| [SnapshotRequest.hs](../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs) and [SendSnapshot.hs](../hydra-node/src/Hydra/HeadLogic/Rules/SendSnapshot.hs) | Construct a capacity-checked proposal, consume owner acceptance, and derive its retained marker and message together. |
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
keys remain in the interpreter. Instruction traces and primitive-query replay
use the same Program instructions.

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
bytes must match the current candidate. Confirmation and adoption also require
the Head id and ordered committee used by verification. Refusal rejects staging,
so dependent effects cannot continue. A candidate cannot be confirmed before
it exists. A live request takes an opaque selected proposal, and the owner
accepts it only while idle and at the matching head/version/number. Trusted
replay retains its separate number-only adapter for existing persisted events.

They are not a proof of the protocol. Verification still trusts the interpreter,
the signable-byte cache. Replay deliberately
trusts retained events; its adapter can reconstruct a confirmation without
cryptography or demanding cached signing bytes. `replayEvent` imports facts directly; it no longer exports unverified commands
for live staging. Other rules still have access to `Program.change`.
The transaction owner has `NoDecommit`, `DecommitPending` and
`AwaitingSettlement` phases. Its topology forbids direct replacement of an
authorised decrement by a local reservation; payload invariants still need checks.

Payload invariants supplement the phase types:

- `Transactions.Rebase` rebuilds the local UTxO, then removes pending decommit
  spending inputs. Recording a reservation and preserving it are now both the
  transaction owner's responsibility.
- `ConfirmTransactions` checks a reservation against the confirmed snapshot and
  retained ordinary transactions before removing its spending inputs. A conflict
  clears it and restores still-unspent inputs.
- [SnapshotRequest.decommitStatus](../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs)
  distinguishes missing, waiting, ready and carried decommits. Every leader uses
  it. A newly ready decommit can start a round by itself; a carried one cannot.

[CoordinatedAudit.hs](../hydra-node/testlib/Test/Hydra/HeadLogic/CoordinatedAudit.hs)
states the reservation and readiness requirements independently for SimpleTx.
The Agda reference checks them, the rebase operation and the wake-up condition
over all subsets of three output identities. Cardano availability includes
spending, reference and collateral inputs. It does not replace ReqSn's ledger
validation of scripts, witnesses and time.

## Reproduce and extend

```sh
just test hydra-node 'coordinated state owners'
cabal run hydra-node:head-logic-lab -- machines /tmp/head-machines
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit.json
```

The [diagram index](head-logic-diagrams.md) renders the composition and the
decommit sequence. The example confirms the dependency in snapshot 2 and the
decommit in snapshot 3 after 45 deliveries. It checks serialized events and
checkpoints after every delivery.

Change rebase or request selection in their owning modules, update the independent
spec/Agda checks, and regenerate the affected traces. Neither the Program
interpreter nor the Mermaid renderer contains a copy of these rules.

[Recovery limits](head-logic/checks.md#recovery-limits) include retry expiry,
invalid acknowledgement replacement, and checkpoints with inconsistent contents.
The [adversarial review](head-logic/adversarial-review.md) records the repaired
conflict and evidence binding, and the remaining limits of the owner model.

Settlement uses these commands, including adoption after an on-chain
observation overtakes the final acknowledgement. Tests compare repeated
observations, reservation release and rollback at every prefix. The independent
Agda model additionally checks version and reservation changes over bounded
versions; it does not model settlement payload identity.

The [measurement report](head-logic/measurements.md)
covers complete rounds, retained-event application and three-node workloads with
these owners. Event application adds a few microseconds per round; the full
comparison also includes allocation costs and the capacity checks.

The [capacity contract](head-logic/capacity.md) requires a checked proposal at the
live request boundary, before the owner retains a request marker.
