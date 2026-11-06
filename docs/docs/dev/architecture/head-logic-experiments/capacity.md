# Snapshot capacity and request progress

[PR #2927](https://github.com/cardano-scaling/hydra/pull/2927) exposes a missing
contract between transaction selection and snapshot coordination: **a leader
must check the snapshot it proposes before recording that a round is in flight**.
The follower's capacity check protects signing, but does not make the leader
recover from an impossible request.

This investigation reads PR head
[`f0a169f48`](https://github.com/cardano-scaling/hydra/tree/f0a169f48a9aa09ec189af1f75c34a1a8323e62c)
against spike revision `386e95947`. It does not apply the fix or rebase the spike.

## The failure

The report describes a split transaction taking the Head over capacity. All
parties reject the resulting `ReqSn`. A later merge reduces the optimistic UTxO,
but the leader's retained `RequestedSnapshot` still suppresses new requests.
Restart preserves that marker. The reported release had a 4,095-output limit;
this branch's trusted setup allows 32,767. The same missing check applies at
either boundary. See [issue #2924](https://github.com/cardano-scaling/hydra/issues/2924)
and [`KZGTrustedSetup.maxAccumulatorSize`](../../../../../hydra-plutus/src/Hydra/Contract/KZGTrustedSetup.hs).

The [runnable witness](traces/capacity-before-rebase.ghci) reproduces the failure
on this branch with SimpleTx and the production entry point, interpreter and
aggregation. It starts three parties at the same signed snapshot 3, version 1,
with 32,767 outputs. Nine deliveries—split to everyone, generated ReqSn to
everyone, independent merge to everyone—produce:

| Point | Local outputs at each party | Leader's round | Confirmed snapshot |
| --- | ---: | --- | ---: |
| Start | 32,767 | `LastSeenSnapshot 3` | 3 |
| After split | 32,768 | `RequestedSnapshot 3 4` | 3 |
| After all parties reject ReqSn | 32,768 | `RequestedSnapshot 3 4` | 3 |
| After merge | 32,767 | `RequestedSnapshot 3 4` | 3 |

All three errors are `ReqSnUTxOSetTooLarge`; the merge emits no network message.
The witness verifies the starting signatures, compares traced and untraced
execution, and round-trips all three final states through checkpoint CBOR. This
is a fixed schedule from an established open Head, not a process-restart test,
a Cardano-ledger workload or an exhaustive exploration.

Run from the repository root:

```sh
cabal repl hydra-node:test:tests \
  < docs/docs/dev/architecture/head-logic-experiments/traces/capacity-before-rebase.ghci
# The script writes /tmp/head-logic-capacity.mmd.
```

Read the [rendered trace](../../../../head-logic-diagrams.md#oversized-snapshot-request-before-rebase)
in three blocks. The first commits the request marker; the second rejects that
request; the third reaches `no snapshot is in flight: no`. The second block's
rejection cannot roll back the first block. Its title names the error checked by
the witness; the current instruction trace records rejection without a separate
named capacity decision.

The relevant path in this branch is:

1. [`ReqTx`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs)
   accepts the split against the local ledger. `Select` obtains a request from
   `chainedSnapshotRequest`, stages `requestSnapshot nextSn`, and emits `ReqSn`.
2. [`Snapshots`](../../../../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs)
   turns `RequestSnapshot number` into `RequestedSnapshot lastSeen number` and
   emits the existing `SnapshotRequestDecided` event.
3. [`ReqSn`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs)
   rejects at `BuildSnapshot`, before signing. Program discards this invocation's
   staged changes; it cannot undo the earlier, completed ReqTx invocation.
4. Subsequent ReqTx invocations see `snapshotInFlight == True`. The merge is
   applied locally, but no replacement request is sent.

The bad transition is `AwaitingRequest → RequestSent` **with an oversized
proposal**. That edge itself is necessary. Removing it from the crem topology
would remove normal snapshot requests too.

## What the proposed fix actually does

The PR's latest implementation allows the optimistic local UTxO to exceed the
bound. It checks request construction and pruning, rather than imposing a new
capacity rejection on every ReqTx. This preserves transaction payloads when
followers receive competing transactions in a different order. `TxValid` still
means local ledger acceptance, not a promise of snapshot inclusion. See the
[implementation at the reviewed revision](https://github.com/cardano-scaling/hydra/blob/f0a169f48a9aa09ec189af1f75c34a1a8323e62c/hydra-node/src/Hydra/HeadLogic.hs).

The policy has several connected parts:

- Select a transaction prefix that fits after the requested decommit, reserving
  any commit or decommit outputs. Stop at the first blocked transaction, even
  when a later merge would bring the final size back down.
- Defer a new deposit that does not fit alongside the actual selected request.
  Continue carrying a settling commit.
- Allow an empty request to start a round that prunes blocked pending work.
  Pruning checks capacity and held deposit outputs, as well as ledger validity;
  it can retain a later independent merge.
- Reject an expanding decommit that does not fit against either the optimistic
  or confirmed ledger. A pending merge is not confirmed capacity: followers
  apply the decommit before the request's ordinary transactions.

These checks concern **two alternative fanout sets**. For active outputs `U`,
pending commit outputs `C`, and pending decommit outputs `D`, both `U ∪ D` and
`U ∪ C` must fit. Count references after forming each union; adding the three
cardinalities is not the rule. Pending decommit outputs are no longer spendable,
but remain owed by the Head until settlement.

## How the representation should change

Keep the current phase graph. Strengthen the data and operation at the request
boundary, using the same selection policy at every leader call site.

[`SnapshotRequest.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs)
already shares selection between ReqTx, AckSn, ReqDec, tick and settlement. Put
the capacity policy there, with a small shared capacity module for decommit
admission and pruning. Keep those decisions out of the interpreter and the
retained-event reducer.

The useful next type is an opaque `CapacityCheckedRequest tx`, produced only by
the selector. It contains the selected request and its snapshot context. Its name
must not imply full ledger validation: selection currently uses `applyTxTo`, and
followers still validate transactions, versions, deposits and signatures.

A live request helper should consume that value immediately in the same input
execution and derive **both** the staged request and network message from it.
That closes today's gap between `requestSnapshot :: SnapshotNumber -> Command tx`
and a separately constructed `ReqSn`. Bind the check to the selected transaction
payloads, incremental action and settled base, not just the snapshot number or a
cached `Bool`. Do not add an independently mutable capacity counter to another
owner.

This is a module-boundary guarantee, not a proof that arbitrary Haskell cannot
bypass it. The selector still needs an independent postcondition test. Keeping
construction and consumption within one invocation avoids introducing another
persisted state or asynchronous approval protocol.

The replay adapter remains a distinct trusted path for the existing
`SnapshotRequestDecided number` event. It cannot reconstruct admission evidence
that the old format never recorded. Preserve that encoding and make the live
versus replay distinction explicit, as confirmation already does. Old persisted
stalls also need the upstream recovery procedure; a stronger live constructor
does not repair them on restart.

Do not add an unconditional `RequestSent → AwaitingRequest` timeout as a shortcut.
Another party might already have signed the previous proposal at that number.
Recovery needs its own protocol argument; this fix avoids constructing the
impossible request in the first place.

| Responsibility | Code to change when rebasing |
| --- | --- |
| Select a request and explain exclusions | [`Rules/SnapshotRequest.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs) |
| Share the check before recording or broadcasting a decommit | [`Rules/Decommit.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs), [`Rules/ReqDec.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) |
| Prune against the snapshot's incremental-action context | [`Rules/Transactions.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/Transactions.hs), called by [`ReqSn.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs) |
| Couple live request staging to its checked contents | [`Machine/Coordinated.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated.hs) and the shared rule helper |
| Retain the existing request marker during recovery | [`Coordinated/Snapshots.hs`](../../../../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs), through `fromEvent` |
| Show why selection stops or pruning removes a transaction | Named Program decisions in the rules; rendering stays in [`Mermaid.hs`](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/Mermaid.hs) |

Two merge conflicts need semantic review. Preserve this branch's `AwaitingInputs`
versus `ReadyToDecommit` distinction and the rule that newly ready decommit-only
work starts a round. Replacing that condition with the PR's
`hasPendingTransactions` would lose the dependency fix. Also preserve the
transaction owner's removal of pending decommit inputs after rebase while
adding the new pruning checks. Neither change can be resolved by taking one
whole version of the helper.

## What could have caught this earlier

The most useful property is **agreement between a locally generated request and
the receiving rule**, not equality with the old implementation:

> With the same settled snapshot, required transaction payloads and deposit
> observations, a request emitted by an honest leader does not fail the follower's
> accumulator-capacity check.

State the premises explicitly: this does not promise acceptance across a chain
version change, missing dependencies, expired validity intervals or conflicting
deposit observations. Check the property by delivering the actual emitted ReqSn
to the actual Program, and independently count both resulting fanout sets.
Merely calling the selector's own capacity helper again would miss a shared error.

Then check the sequence: start at capacity, accept a split, deliver the generated
request to everyone, and submit an independent merge. With finite honest input,
stable chain state and all queued deliveries/retries drained, the merge must be
confirmable. An empty round must remove blocked pending work or enable subsequent
work; repeatedly confirming empty snapshots without either change is another
failure. Do not require every `TxValid` transaction to confirm, since pruning is
part of the selected policy.

Use boundary cases at `limit - 1`, `limit`, and `limit + 1`, varying:

- a carried commit, a new deposit and an unsettled decommit;
- a merge before or after an overflowing split, including across the tx-count cap;
- independent merges and merges dependent on a transaction that gets pruned;
- different delivery orders, a delayed ReqSn, and restart after each retained event.

The abstract Agda model can use a capacity of two or three and explicit sets of
output names. Prove that both fanout sets of each selected prefix fit, then check
the selection/pruning cycle with a pending-request marker and a finite delivery
queue. A production comparison must still use the deployed limit. One bridge is
to pad small scenarios with untouched background outputs so the same boundary
is reached without changing production configuration. The padding argument only
applies when transactions cannot consume or collide with those background names.

The PR already adds extracted capacity predicates and Haskell boundary tests;
reuse those on rebase. They check counts and pruning decisions, not the whole
request/progress cycle. Extending the model to that cycle is the additional work
this formalism makes practical.

## Why the current evidence missed it

- Frozen-handler comparisons deliberately preserve old behavior, including this
  defect. They establish migration fidelity, not protocol correctness.
- The existing oversized ReqSn test checks that the follower rejects. It does
  not ask whether the leader can generate that request and become stuck.
- The bounded Agda round model explicitly assumes bounded accumulators and has
  no scheduler or restart semantics. Its assumptions exclude this counterexample.
- The explorer's transaction fixtures stay small. Its quiescence check is useful,
  but no schedule can manufacture a capacity crossing from those transactions.
- Crem currently restricts phase transitions and payload ownership. A legal edge
  with unsuitable proposal contents remains legal.

Program gives us executable rules and a trace of the failing sequence; crem
gives us a precise place to require proposal evidence. Neither discovers the
missing capacity/progress contract by itself. The gain comes from writing that
contract independently and exercising the composition of the rules.
