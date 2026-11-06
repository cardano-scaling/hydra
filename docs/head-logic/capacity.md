# Snapshot capacity

A leader must select a capacity-checked proposal before recording a live snapshot
request. Follower rejection cannot remove a request marker retained by a separate
input. The same capacity policy applies to selection, decommit admission, and
pruning.

## Selection policy

For active outputs `U`, pending commit outputs `C`, and pending decommit outputs
`D`, both **`U ∪ D` and `U ∪ C` must fit**. These are alternative fanouts. Count
distinct output references after each union. The configured accumulator limit is
32,767 outputs.

- ReqTx may exceed capacity in the optimistic local ledger. Its payload remains
  available for snapshot validation, including when competing transactions
  arrive in different orders. `TxValid` does not promise snapshot inclusion.
- Selection stops at the first overflowing transaction prefix. Carried commit
  and decommit outputs reserve capacity throughout that prefix.
- A new deposit is selected after ordinary transactions and must fit the final
  ledger. It can use space freed by a merge. A deposit that does not fit waits.
- Pending transactions can justify an empty round. Pruning skips blocked work
  and continues with later independent transactions.
- A new decommit needs outputs and must fit against both local and confirmed
  ledgers before its outputs are removed. Unconfirmed merges cannot supply
  confirmed capacity.

Decommit readiness is a separate condition. Selection waits until its required
inputs are confirmed. A newly ready decommit can start a round by itself; an
already carried incremental action cannot repeatedly start empty rounds.

## Code to read

| Responsibility | Implementation |
| --- | --- |
| Count both fanout alternatives | [SnapshotCapacity.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotCapacity.hs) |
| Select the bounded prefix and incremental action | [SnapshotRequest.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs) |
| Retain the accepted request and broadcast its message | [SendSnapshot.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/SendSnapshot.hs) |
| Require the idle phase and matching head/version/number | [Coordinated/Snapshots.hs](../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Snapshots.hs) |
| Prune by applicability, deposit collisions, and capacity | [Transactions.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/Transactions.hs) |
| Check decommit capacity | [Decommit.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/Decommit.hs), [ReqDec.hs](../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) |
| Check requests against independent references | [CapacitySpec.hs](../../hydra-node/test/Hydra/HeadLogic/CapacitySpec.hs) |

`SnapshotRequest` has a private constructor and private record fields. Its public
accessors cannot update it. The proposal contains the selected transactions,
incremental action, head ID, version, and next snapshot number. `sendSnapshot`
uses the owner's acceptance to derive both the retained marker and message.

Selection uses arithmetic `applyTxTo`; capacity checking does not establish full
ledger validity. Followers still validate transactions, observations, and
signatures. Construction and consumption happen in the same input invocation.
Matching coordinates assume agreement on the confirmed snapshot at those
coordinates; the type does not prove that distributed invariant.

ReqDec broadcasts a checked proposal without retaining `SnapshotRequestDecided`.
Trusted replay uses a separate number-only command for persisted request events.
It does not reconstruct admission evidence or repair an inconsistent checkpoint.

## Merged fix and regression tests

The branch is rebased onto master `0aa8987dc`, which merges
[PR #2927](https://github.com/cardano-scaling/hydra/pull/2927). Its policy is present
in the Crem/Program rules: prefix capacity and deposit selection in
`SnapshotRequest`, pruning in `Transactions`, and local/confirmed decommit
capacity in `Decommit` and `ReqDec`. `ReqTx` checks leadership and round status
before constructing a proposal. The capacity port's production code and formal
model are unchanged by the rebase.

The upstream tests are retained:

- [HeadLogicSpec](../../hydra-node/test/Hydra/HeadLogicSpec.hs): seven capacity
  cases and the expanded empty/oversized-decommit test. Selection calls now
  supply the snapshot context and decommit status, and assert `Just` proposals
  through public accessors. Their original expected transactions and outcomes
  are preserved.
- [BehaviorSpec](../../hydra-node/test/Hydra/BehaviorSpec.hs): all three parties
  prune an overflowing transaction and confirm a later independent merge.
- [OffChainAgreementSpec](../../hydra-node/test/Hydra/OffChainAgreementSpec.hs):
  three comparisons between extracted capacity guards and actual HeadLogic.
  This file and BehaviorSpec match master without adaptation.

All 12 focused checks pass in a fresh test REPL. They exercise the production
dispatcher, which executes the Crem workflows and Program interpreter. The
upstream competing-transaction test explicitly requires the optimistic local
ledger to exceed capacity while retaining both payloads. Adding a ReqTx capacity
rejection would break that behavior; capacity is enforced when selecting and
pruning snapshots.

```text
:main -p "/accumulator capacity/ || /prunes an overflowing transaction/ || /rejects a decommit tx with no outputs/"
```

## Executable checks

```sh
cabal run hydra-node:head-logic-lab -- capacity /tmp/capacity.json +RTS -N1
cabal run hydra-node:head-logic-lab -- trace /tmp/capacity-replay.json --scenario capacity +RTS -N1
cabal run hydra-node:head-logic-lab -- replay /tmp/capacity-replay.json +RTS -N1
```

The capacity example starts three parties at signed snapshot 3, version 1, with
a full UTxO. A split overflows capacity; an independent merge frees space. The
rules request empty snapshot 4, prune the split, and confirm the merge everywhere
in snapshot 5 after 30 FIFO deliveries. Each prefix checks independent request
audits, signatures, agreement, dependencies, and serialized state recovery.
The [saved report](traces/capacity-fixed-v1.json) has its own compact schema;
the `trace` command produces the replayable scenario format.

The independent contract is: given the same settled snapshot, transaction
payloads, and deposit observations, an honest leader's request passes the
follower's capacity check. The progress assertion covers this finite honest
schedule with stable chain state, not arbitrary scheduling or retry exhaustion.

The [Agda reference](../../spec/src/Hydra/Protocol/OffChainReference.agda) checks
count predicates and small-set prefix selection/pruning. The
[specification](../../spec/src/Hydra/Protocol/OffChain.lagda.typ) checks the
split/prune/merge cycle at capacity three by reduction. Haskell pads those small
sets with untouched outputs to exercise the deployed limit. This small-set model
excludes decommits; direct Haskell tests cover their reserved capacity.
