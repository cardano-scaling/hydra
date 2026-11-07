# Snapshot capacity and request progress

Archived from `a6c4966f8`. This records an earlier investigation; claims and commands belong to the revisions named here. Use the [current Crem/Program guide](../head-logic-review.md) for the implementation.

[PR #2927](https://github.com/cardano-scaling/hydra/pull/2927) exposes a missing
contract: **a leader must check a proposal's capacity before recording a snapshot
request**. Follower rejection protects signing; it cannot undo a request marker
retained by an earlier input.

This branch ports the policy from PR revision
[`f0a169f48`](https://github.com/cardano-scaling/hydra/tree/f0a169f48a9aa09ec189af1f75c34a1a8323e62c)
into the Crem/Program rules. It also makes `SnapshotRequest` opaque and requires
it at the live coordination boundary. No rebase has been performed. Network,
event and checkpoint encodings are unchanged.

## The failure and repair

The reported release allowed 4,095 outputs; the trusted setup on this branch
allows 32,767. At either bound, a split can be valid against the optimistic ledger
but impossible to snapshot. The old leader records `RequestedSnapshot`, every
party rejects the oversized ReqSn, and a later merge cannot trigger another
request. Restart preserves the marker. See
[issue #2924](https://github.com/cardano-scaling/hydra/issues/2924).

The retained [original trace](diagrams.md#oversized-snapshot-request-before-rebase)
comes from production revision `386e95947`: deliver the split, deliver the
rejected request, then deliver the independent merge. Read its three blocks:
`SnapshotRequestDecided`, rejection, then `no snapshot is in flight: no`.
The rejection belongs to a different input from the retained marker.

The current [capacity demonstration](../../hydra-node/testlib/Test/Hydra/HeadLogic/Capacity.hs)
starts all three parties at a signed snapshot 3, version 1, with a full
UTxO. It delivers a split and an independent merge, then drains all generated
messages in FIFO order. Run both implementations:

```sh
cabal run hydra-node:head-logic-lab -- capacity --historical /tmp/capacity-old.json +RTS -N1
cabal run hydra-node:head-logic-lab -- capacity /tmp/capacity-fixed.json +RTS -N1
```

Each command writes a compact JSON report and an instruction trace in `.json.mmd`.
The historical run uses frozen programs and checks agreement with their frozen
reducer. The corrected run uses the node's complete input program. Both check
serialized events and checkpoints at every delivery.

| Execution | Selected snapshot 4 | Result after all messages drain |
| --- | --- | --- |
| Frozen rules | Overflowing split | All parties remain at snapshot 3; leader remains in `RequestedSnapshot 3 4`. |
| Current rules | Empty pruning round | The split is pruned; the independent merge confirms everywhere in snapshot 5. |

The corrected run audits each emitted request using SimpleTx sets, independently
of production selection and ledger application. It also checks real signatures,
agreement and transaction dependencies. This is a fixed schedule, not an
exhaustive exploration or a Cardano-ledger performance test.

## Code to read

| Responsibility | Implementation |
| --- | --- |
| Count both fanout alternatives | [`SnapshotCapacity.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotCapacity.hs) |
| Select a bounded prefix and incremental action; construct the opaque proposal | [`SnapshotRequest.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs) |
| Stage the owner's accepted events and emit the message from that same proposal | [`SendSnapshot.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/SendSnapshot.hs) |
| Permit a live request only from the idle coordination phase with matching context | [`Coordinated/Snapshots.hs`](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs) |
| Prune by ledger applicability, deposit-output collisions and capacity | [`Transactions.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Transactions.hs) |
| Check decommit capacity before recording or broadcasting it | [`Decommit.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs), [`ReqDec.hs`](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) |
| Check the request against the receiving rule and independent references | [`CapacitySpec.hs`](../../hydra-node/test/Hydra/HeadLogic/CapacitySpec.hs) |

The phase edge `AwaitingRequest → RequestSent` remains necessary. Its live
operation now takes a proposal containing the selected transaction payloads,
incremental action, head ID, version and next snapshot number. The constructor
and record fields are private. Ordinary accessors cannot update it. `sendSnapshot`
uses the owner's acceptance directly, avoiding another copy of its guards.

This establishes a module boundary, not full ledger validity. Selection uses
arithmetic `applyTxTo`; followers still check transactions, observations and
signatures. Construction and consumption happen in the same input invocation.
Matching coordinates assume the parties agree on the confirmed snapshot at those
coordinates; the type does not prove that distributed invariant.

ReqDec preserves its historical broadcast-only behavior: it consumes a checked
proposal but does not retain `SnapshotRequestDecided`. Trusted event replay has a
separate `ReplaySnapshotRequest` command. Old events contain only a number and
cannot reconstruct admission evidence. Restoring an old stalled checkpoint does
not repair it; recovery of such a Head remains a separate procedure.

## Selection policy

For active outputs `U`, pending commit outputs `C`, and pending decommit outputs
`D`, both **`U ∪ D` and `U ∪ C` must fit**. These are alternative fanouts. Count
output references after each union, not the sum of all three sizes.

- Ordinary ReqTx admission may exceed capacity locally. Payloads remain available
  when a follower receives competing transactions in a different order, even at
  TTL zero. `TxValid` does not promise snapshot inclusion.
- Selection stops at the first overflowing prefix. A later merge cannot repair
  that prefix. Carried commit/decommit outputs reserve capacity throughout.
- A new deposit is selected after the ordinary prefix and must fit its final
  ledger. It can use space freed by a merge; it does not constrain that prefix's
  intermediate states. A deposit that does not fit waits.
- Pending transactions can justify an empty round. Pruning skips blocked work and
  continues with later independent transactions. A carried incremental action
  alone does not repeatedly request empty rounds.
- Decommits must have outputs and fit against both local and confirmed ledgers,
  before their outputs are removed. Unconfirmed merges cannot supply confirmed
  capacity.

The port preserves the decommit dependency and reservation fixes already on this
branch. `AwaitingInputs`, `ReadyToDecommit` and `CarryDecommit` remain distinct;
a newly ready decommit can start a round by itself. Rebase still reserves its
spending inputs. These are semantic differences to preserve when rebasing #2927.
Retry expiry and invalid-acknowledgement replacement policies remain unchanged.

## What could have caught it earlier

The useful contract compares *different rules*, independently of the old code:

> Given the same settled snapshot, payloads and deposit observations, an honest
> leader's emitted request does not fail the follower's capacity check.

The repaired demonstration delivers the actual emitted messages to the actual
rules. Boundary tests cover carried increments/decrements, deposits after a
merge, stale proposals, different transaction arrival orders, and empty rounds.
The progress assertion is specific: under stable chain state and this finite
honest schedule, draining the queues confirms the independent merge. It neither
requires every locally accepted transaction to confirm nor proves liveness under
retry exhaustion.

The [Agda reference](../../spec/src/Hydra/Protocol/OffChainReference.agda)
checks deployed-limit count predicates and small-set prefix selection/pruning.
The [literate specification](../../spec/src/Hydra/Protocol/OffChain.lagda.typ)
also checks the split/prune/merge cycle at capacity three by reduction. Haskell
compares bounded sequences with those extracted functions after adding untouched
background outputs to reach the real limit. Transactions never name the padding.
The small-set prefix model excludes decommits; direct Haskell tests cover their
reserved capacity. These checks are not a proof of the scheduler, real ledger,
restart process or general distributed progress.

The generic explorer has a `capacity` scenario and independently checks both
fanout sizes for seen and confirmed snapshots. A replayable FIFO trace uses the
existing versioned scenario format and validates primitive query identities and
arguments before consuming recorded replies:

```sh
cabal run hydra-node:head-logic-lab -- trace /tmp/capacity-replay.json --scenario capacity +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/capacity-replay.json +RTS -N1
cabal run hydra-node:head-logic-lab -- explore /tmp/capacity-search.json --scenario capacity --deliveries 40 --states 100 +RTS -N1
```

The last command reports its bounds, visited states and truncation. A truncated
search is not evidence that every schedule succeeds. Incremental actions and
chain events remain outside this explorer; focused tests cover them.

## Why the earlier evidence missed it

Frozen comparisons preserved this bug. Oversized-ReqSn tests established follower
rejection but never asked whether the leader could create that request and stall.
The bounded round model assumed small accumulators, and the explorer's fixtures
never crossed capacity. Crem restricted phase transitions and ownership; it did
not constrain the contents of a proposal on a legal edge.

Program supplies an executable witness and decision trace. Crem supplies the
place to require checked proposal data. The independent capacity/progress
contract supplies the missing test. Neither representation discovers that
contract automatically.
