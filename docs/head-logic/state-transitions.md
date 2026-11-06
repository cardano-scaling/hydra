# Which state transitions belong in Crem?

The useful distinction is **what an operation is allowed to do next**, not which
fields happen to be populated. `Unreserved`/`Reserved` was too coarse: it treated
local intent and an authorised decrement as the same state. It also missed nodes
that learn the decrement through a signed snapshot without receiving `ReqDec`.

There are three different kinds of state in this code. They need different
lifetimes and different guarantees.

| Machine | Lifetime | What a transition establishes |
| --- | --- | --- |
| Input workflow | One input, ending in completion, wait or rejection | A later action has the data produced by an earlier check. For example, AckSn reaches confirmation only with verification evidence. |
| Open-head owners | Across inputs, restored from persisted state | Which operation is pending, authorised, or settled; which component may update each field. |
| Retained-event application | One retained fact, possibly during recovery | How to reconstruct state without repeating ledger validation or signature verification. This trusts the event source. |

`Program` remains useful inside the first kind: it makes decisions and primitive
queries inspectable and gives all workflows the same buffering semantics. Crem
supplies the phase transitions around those actions. It does not replace those
instructions or make cryptographic predicates type-level facts.

## Decommit: local intent versus authority

The spec requires a same-version confirmed decommit to be carried into subsequent
snapshots until settlement. Local reservations have a different lifetime: they
may depend on ordinary transactions and may lose a conflict when a different
transaction confirms.

The executable definition is
[`Transactions.hs`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs).
Its phases are:

| Phase | Meaning | Permitted changes of phase |
| --- | --- | --- |
| `NoDecommit` | No local reservation and no current-version decommit certificate | `DecommitPending` after admission; `AwaitingSettlement` after learning a certificate |
| `DecommitPending` | Local transaction accepted and its spending inputs reserved | `AwaitingSettlement` when confirmed; `NoDecommit` on conflict, clearing or settlement |
| `AwaitingSettlement` | The current-version confirmed snapshot authorises a decrement; the local transaction may be absent | `NoDecommit` when that authority is retired |

**`AwaitingSettlement → DecommitPending` is absent.** Recording another local
reservation cannot replace the authorised action. `reserve` has a concrete source
phase, `NoDecommit`; calling it from the settling branch requires a Crem edge that
does not exist. Tests also check the runtime refusal and admission after a fresh
settlement. Self-transitions remain possible in Crem, so guards still protect
operation identity and prevent replacing one pending transaction with another.

`NoDecommit → AwaitingSettlement` must exist. `ReqSn`, settlement adoption and
side-loading can reveal a valid certificate before the original `ReqDec` reaches
this node. Banning that edge would break legitimate delivery orders.

We do not add separate persistent phases for “waiting for dependency”, “ready”,
and “included in a candidate”. The first two depend on the current ledger; an
input can disappear during pruning. Inclusion in an unconfirmed candidate does
not authorise settlement or cancellation. Those facts remain predicates and
candidate data. The durable change of authority occurs at **confirmation**.

### The conflict repair

Consider an ordinary transaction `1 → 2` and a decommit `1 → 10`. A party can
accept the decommit first, then confirm the ordinary transaction proposed by the
leader. Previously it kept an unusable reservation forever, refusing subsequent
decommits until local state was cleared.

`ConfirmTransactions` now reconstructs the ledger from the confirmed snapshot
and retained ordinary transactions **before reserving inputs**. It keeps a local
decommit if the certificate carries it, or if all its required inputs survive
and no different decommit is authorised. Otherwise it clears the reservation,
restores the reconstructed ledger and emits the existing `DecommitInvalid` event.
This also restores any other inputs of a multi-input reservation. Valid retained
dependencies survive. Signing a candidate alone does not release anything.

[`AckSn`](../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs) uses the returned
owner state when selecting the next request. Using its original captured state
would keep the invalid reservation in that decision.

The distinction also fixes admission without a local `ReqDec`: both client and
network rules check the current certificate through
[`settlingDecommit`](../../hydra-node/src/Hydra/HeadLogic/State/Operations.hs).
The owner enforces the same restriction if a future caller misses that guard.
This is an admission/coordination defect; the investigation does not demonstrate
an invalid signed snapshot or lost funds.

## Snapshot round: do not confuse a request marker with a certificate

For one `(HeadId, version, number)`, the meaningful progression is:

1. Select a proposal satisfying capacity and incremental-action constraints.
2. Validate the received proposal and construct the candidate to sign.
3. Collect signatures for that candidate.
4. Verify the complete ordered committee's signatures and confirm the candidate.

The first step is leader-local; followers legitimately begin at step 2. A chain
observation can supply a certificate before step 3 finishes locally. A new round
has a new identity: it is not an earlier stage of the same round.

Those prerequisites live in
[`SnapshotRequest`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs),
[`ReqSn`](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs),
[`AckSn`](../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs) and
[`SnapshotVerification`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotVerification.hs).
Confirmation evidence now retains the ordered verification keys. The owner
checks Head identity, committee and candidate bytes. Adoption checks Head and
committee. Staging a refused confirmation or adoption rejects the Program,
including any earlier staged signature.

The snapshot **storage** graph still allows all three phases to reach each
other. It combines request markers, candidates, explicit adoption, clearing and
trusted historical events. It is not evidence that all those edges are legal
network actions. Removing an edge there solely to obtain a sparser diagram would
misrepresent replay. In particular, arbitrary retained-event tests import facts
that live guards would never produce.

The next useful refinement would be an owner-issued candidate handle tied to a
round, with separate live commands and recovery initialization. It should prevent
replacing a collecting candidate or confirming a different round without copying
the ReqSn rules into another machine. That is not implemented here. Cached signing
bytes and valid recovery data remain assumptions; a new phase name cannot prove
either invariant.

### Fixed: a certificate without its carried transaction

`ReqSn` can arrive before `ReqDec`. Previously Bob could validate Alice's
snapshot-1 request carrying decommit `1 → 10`, confirm it with all three
signatures, then lose the transaction: the snapshot kept outputs `{10}`, but
`decommitTx` stayed empty. As leader for snapshot 2, Bob would broadcast
`ReqSn 0 2 [20] Nothing Nothing` and reject his own request with
`ReqSnDecommitNotSettled`. Settlement could release the stuck request marker;
the example demonstrated a stalled round, not lost funds.

The repair gives the payload an explicit lifetime without changing persistence
formats:

1. [ReqSn](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs) stages
   `retainSnapshotDecommit` after validation and pruning. The transaction owner
   emits an existing `TransactionReceived` event if it needs to retain the payload.
   A different local reservation stays intact.
2. [ReqTx](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs) ignores a duplicate
   of that candidate transaction before receipt, validation or expiry can replace
   or evict it. This includes a witness-stripped duplicate with the same body hash.
3. [ConfirmTransactions](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs)
   promotes the matching payload into `decommitTx` and removes its registry entry.
   A conflicting reservation is released at this point. If settlement already
   advanced the version, the old payload is removed without being revived.
4. [SnapshotRequest](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs)
   requires the matching carried transaction before constructing a proposal.
   The snapshot owner checks this obligation again when consuming the proposal.

Matching uses the non-empty output UTxO, just as ReqSn's carry check does. Cardano
output references include the transaction body hash. Normal confirmation uses
the local reservation; only a missing or different reservation needs a registry
search. An old checkpoint that already lost the payload waits for settlement;
these changes cannot recover transaction bytes it never retained.

[SnapshotDecommitSpec](../../hydra-node/test/Hydra/HeadLogic/SnapshotDecommitSpec.hs)
starts with the failing sequence and checks confirmation of snapshot 2 after the
repair. It also checks competing reservations, rejected signatures, duplicates,
settlement ordering, side-loading, and serialized events and checkpoints at every
retained prefix. A real Cardano duplicate test is in
[HeadLogicSpec](../../hydra-node/test/Hydra/HeadLogicSpec.hs).

The [executable example](missing-decommit-example.hs) runs the corrected sequence
through the normal dispatcher and verifier. In `cabal repl hydra-node:test:tests`:

```text
:load ../docs/head-logic/missing-decommit-example.hs
probe
writeTrace "../docs/head-logic/traces/carried-decommit.mmd"
```

It confirms snapshot 2 with UTxO `{20}` while retaining the unsettled decommit.
The [component trace](traces/carried-decommit.mmd) is also embedded in the
[central diagram page](../head-logic-diagrams.md).

### What this says about Crem

This is evidence for the usefulness of the ownership model, not a demonstration
that Crem is necessary. Separating local intent from settlement authority made
us ask how a party enters `AwaitingSettlement` without first seeing `ReqDec`.
That question exposed the lost payload. The topology alone accepted this sequence;
it could not prove that the payload survived.

Crem gives us checked phase edges, composition and diagrams of the executable
machines. Program keeps their decisions inspectable. Typed reducers could enforce
the same requirements. The case for keeping Crem is the shared structure and the
mistakes it prevents as rules change, weighed against its type-level machinery.
The regression, payload ownership and checked proposal are what fix this bug.

## Settlement and rollback

A decrement advances the version and retires the previous version's authority.
A repeated observation must not release a newer reservation. Rollback changes
chain context and repost eligibility; it does **not** undo the off-chain version
or recreate spent outputs. Retained settlement certificates are therefore a
separate responsibility from the pending-operation phase.

[`Settlement.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Settlement.hs),
[`Snapshots.hs`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs)
and [`Repost.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs) implement
those rules. The existing 24 settlement/rollback schedules remain relevant.
Likewise, `Closed` and `ReadyToFanout` can reach `Idle` when a node observes another
party completing fanout. The state describes local knowledge, not just locally
submitted transactions.

## Deposits: chain presence is not Head accounting

[`Deposit.hs`](../../hydra-node/src/Hydra/HeadLogic/Machine/Deposit.hs) tracks one
deposit as missing, inactive, active, expired or consumed. Its broad graph is
mostly explained by observation and rollback: a consumed deposit can reappear,
and a later observation can replace its recorded slot and status. An expiry flag
alone cannot tell us whether its value is already accounted for inside the Head.

The useful additional distinction is **unaccounted versus accounted by a finalized
increment**, tied to the deposit id and settlement certificate. This is already a
semantic distinction in the code, but it lives across two owners: the deposit
registry restores chain presence, while snapshot state retains the finalized
increment. [`Recover.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Recover.hs)
checks the latter before allowing recovery, and
[`Repost.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Repost.hs) reposts the
increment after rollback.

A future composed deposit model should keep these facts independent:

| Component | States/facts | Reason |
| --- | --- | --- |
| Chain presence | Missing, recorded with slot/status, consumed with slot | Rollback can reverse observation or consumption. |
| Head accounting | Unaccounted, accounted with deposit id and certificate | Rollback does not remove value already added to the off-chain ledger. |

The useful prohibition is **accounted → recoverable outside the Head**, without
undoing the Head's accounting. Banning `ConsumedDeposit → ActiveDeposit` would be
wrong: that edge describes a legitimate rollback. Merely signing a candidate
must not be called “accounted”; the current recovery guard is specifically about
a finalized increment. Deposit expiry and dropping a settling commit also differ
from a decommit, which must remain carried until settlement.

This refinement is analysis, not an additional implementation in this change.
Its acceptance test should start with a finalized increment, spend its value
off-chain, roll the increment back, and attempt both recovery and a second
commit of the same deposit. A typed recovery command would have to obtain its
permission from the accounting owner, rather than rediscovering it from a registry
status. The existing settlement schedules and guards are the behavior to preserve.

## Evidence and limits

[`CoordinatedSpec`](../../hydra-node/test/Hydra/HeadLogic/CoordinatedSpec.hs) checks
24 conflicting delivery orders, multi-input release, retained dependencies,
wrong Head/committee/candidate evidence, admission without `ReqDec`, fresh
settlement, and recovery. The
[independent Agda reference](../../spec/src/Hydra/Protocol/OffChainReference.agda)
contains the decommit transition relation and an impossibility proof for direct
replacement of an authorised decommit. Differential tests cover input subsets of
three output identities and real signatures with bounded committee permutations.
This is not a proof of the Cardano ledger, liveness, or all chain histories.

Live rules use abstract `Command`s. `replayEvent` imports trusted persisted facts;
it does not return an unverified command that a live Program can stage. Existing
event and checkpoint encodings are unchanged. Recovery still trusts its inputs,
and `Program.change` remains an escape hatch for rules outside these owners.

Reproduce the corrected conflict and render the actual machines:

```sh
just test hydra-node 'coordinated state owners'
cabal run hydra-node:head-logic-lab -- decommit-conflict /tmp/decommit-conflict.mmd
cabal run hydra-node:head-logic-lab -- machines /tmp/head-machines
```

The [central diagram page](../head-logic-diagrams.md) includes both.

### Check the forbidden edge directly

In `cabal repl hydra-node:lib:hydra-node`, load the owner's local scope:

```text
:module *Hydra.HeadLogic.Machine.Coordinated.Transactions
:module +Hydra.Ledger.Simple
:set -XDataKinds
let allowed = pureResult () (Pending (error "not evaluated") (error "not evaluated")) :: ActionResult Identity TransactionTopology (State SimpleTx) 'NoDecommit ()
let forbidden = pureResult () (Pending (error "not evaluated") (error "not evaluated")) :: ActionResult Identity TransactionTopology (State SimpleTx) 'AwaitingSettlement ()
```

The first declaration typechecks. The second fails with no instance for
`AllowedTransition ('Topology '[]) AwaitingSettlement DecommitPending`.
The unevaluated payloads keep this check about the edge, not ledger data. Both
commands have been checked against the executable owner definition.
