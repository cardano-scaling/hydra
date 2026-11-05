# Demonstrating the head logic spike

Start with the real registry failure, then show how the same rules support
inspection and bounded exploration. The verification-bypass example is a useful
test of the tooling, but the registry failure needs no injected fault.

## A transaction that breaks the next snapshot

```sh
cabal run hydra-node:head-logic-lab -- registry-demo /tmp/registry-demo.json
```

The command uses a deterministic Cardano payment and the real Cardano ledger:

1. Apply the valid payment and check that a snapshot containing it can be signed.
2. Strip its witnesses. Its ID stays the same, but ledger validation fails even
   against the original, unspent input.
3. Deliver it to the frozen ReqTx handler. The handler records the replacement
   before waiting; aggregation therefore replaces the valid registry payload.
4. Process the same snapshot request. It now fails with `SnapshotDoesNotApply`.
5. Repeat from the accepted state with the current ReqTx program. Its duplicate
   guard emits no changes, preserves the complete state and lets the snapshot
   request succeed.

The command fails if any of these checks fails. `/tmp/registry-demo.json` contains
both transactions, complete outcomes and resulting states. The adjacent `.mmd`
file shows actual instruction observations for the snapshot rejection and the
corrected path; the frozen handler's receipt/wait is labelled as an outcome.
Neither file contains signing keys.

The checked-in [result](traces/registry-demo-v1.json)
and [diagram](traces/registry-demo-v1.json.mmd)
come from this command.

Read the diagram from top to bottom. Each shaded section is one handler call
or a labelled outcome summary. The lanes separate **Head logic**, **Ledger**,
**Cryptography**, **Staged changes** and **Buffered effects** within one node.
Decisions stay on the head logic lane; primitive replies, staged changes and
buffered effects cross to their respective lanes. The final note says what is
committed or discarded. A buffered `AckSn` is released only when the handler
finishes successfully.

Start at **ReqSn after the frozen ReqTx**: the ledger rejects the replaced
payload. Then follow **Restart from the shared starting state**: the fixed
ReqTx recognises the already-applied transaction and leaves the state unchanged.
The last section shows ReqSn validating the preserved payload, signing and
buffering `AckSn`. Its later “outputs are fresh: no” decision is local transaction
pruning, not a second snapshot rejection. The reset compares two runs from the
same accepted state; it does not repair the corrupted state.

Read [RegistryDemo.hs](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/RegistryDemo.hs),
then `onOpenNetworkReqTxProgram` in
[HeadLogic.hs](../../../../../hydra-node/src/Hydra/HeadLogic.hs). The existing
[Cardano regression](../../../../../hydra-node/test/Hydra/HeadLogicSpec.hs) exercises the
production dispatcher too. This is a registry/progress bug; it does not produce
an invalid confirmed snapshot.

An ordinary regression test can catch this bug. The benefit demonstrated here is
that the production rules also explain where the tentative change is staged,
why waiting retains it, and which rule prevents it. There is no separate diagram
implementation of the protocol.

## Find the retry-expiry failure

```sh
cabal run hydra-node:head-logic-lab -- explore /tmp/expiry.json \
  --scenario expiry --search retry-windows
cabal run hydra-node:head-logic-lab -- replay /tmp/expiry.json --normal
cabal run hydra-node:head-logic-lab -- replay /tmp/expiry.json --step
```

The general DFS and retry-first searches miss the known failure at 100,000
states. `retry-windows` explores a smaller, explicit schedule family: receive a
prefix of the initial queue, allow each recipient an independent number of
eligible retries, then drain deliveries before retries. It does not contain the
recorded failing route or inspect the invariant to choose actions.

This search finds the failure after **234 states**, with **22 deliveries** and
three depth cutoffs. The report is truncated, not exhaustive. Minimization and
primitive-reply replay use the existing implementations. Deduplication includes
the remaining scheduler budgets as well as protocol state and delivery depth.
The default limits remain 24 deliveries and 100,000 states.

The failure is an honest Head with empty queues and unconfirmed work at followers:
the next leader expired the child transaction before its parent arrived.
Re-submitting the child wakes that leader. This search establishes neither the
probability of that schedule nor its timing in the production queue.

The discovered [scenario](traces/expiry-search-v2.json)
and [diagram](traces/expiry-search-v2.json.mmd)
are separate from the earlier manually constructed witness.

Read `exploreRetryWindows` in
[Explorer.hs](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/Explorer.hs).
The [tests](../../../../../hydra-node/test/Hydra/HeadLogic/SnapshotProgramSpec.hs) require the
search to find the failure within budget and replay the minimized route. They
also challenge the independent invariants with an omitted receipt, a dishonest
ledger reply and the existing multisignature-verification bypass. These are
specific sensitivity checks, not a complete mutation campaign.

## Settlement, spending and rollback

```sh
cabal run hydra-node:head-logic-lab -- settlement /tmp/settlement.json
```

This runs 24 schedules through the production `update` and aggregation functions:
three parties observe and activate a deposit, confirm its snapshot, observe the
increment, spend the deposited output, roll back and observe the increment again.
The schedules vary whether the spend confirms before rollback, whether rollback
erases or retains the increment, and all six party re-observation orders.
When snapshot messages are pending, one is delivered between successive parties'
re-observations, exercising different local views of the restored deposit.

Each transition checks complete state equality after serialized event replay and
a checkpoint round trip. The sequence checks real confirmed signatures, the
expected UTxO, preservation of pending and signed L2 state, removal of the settled
deposit, retention of the signed increment and refresh of its observation slot.
Rollback reposts the retained increment exactly when its observation was erased.

All 24 schedules pass. No new settlement bug was found. Existing tests already
cover several of these cases individually; this joins them into three-party
sequences with persistence checks. See
[Settlement.hs](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/Settlement.hs) and the
`Rollback after finalized increment or decrement` group in
[HeadLogicSpec.hs](../../../../../hydra-node/test/Hydra/HeadLogicSpec.hs).

This is a separate bounded driver, not an extension of the general explorer's
off-chain state domain. It supplies trusted chain observations and uses the
Simple ledger. It does not test Cardano settlement validation, arbitrary chain
forks, a crash between persistence and broadcast, or Agda rollback semantics.
The exported JSON records inputs, complete outcomes and before/after states;
the adjacent diagram records the sequence of deliveries and chain observations.
The checked-in [settlement diagram](traces/settlement-v1.mmd)
shows the case where snapshot 2 confirms before rollback erases the increment.

## Performance evidence

Use the [measurement page](measurements.md) for the
end-to-end comparison, workload limits and reproduction commands. The comparison
uses separate node binaries and the same cluster harness; there is no production
switch to the frozen handlers or test mutations.
