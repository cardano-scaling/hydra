# Head logic bugs found with the spike

The spike has found transaction-map corruption and several ways a Head can stop
making progress. The two `allTxs` bugs are fixed. The latest conversion exposes a
decommit dependency mismatch and loss of its local input reservation; both are now fixed in a separate behavior change and remain reproducible with
the frozen handlers. These cases use the
normal interpreter. None demonstrates an invalid confirmation or loss of funds.

The performance investigation also found unnecessary UTxO evaluation introduced
by this spike. The compatibility interpreter discarded the resolved-change
endpoints, but `StrictData` forced them first. Both instruction and staged-change
fields are now explicitly lazy; a regression test checks that compatibility
interpretation does not evaluate them. This is an implementation cost, outside
the Agda model, rather than a bug in the previous protocol rules. See the
[measurement report](measurements.md#what-the-investigation-changed).

| Finding | Current result | Evidence |
| --- | --- | --- |
| Invalid witness variant replaces a validated transaction | Fixed: locally applied IDs are ignored before staging receipt | Real Cardano ledger; one-party head; unchanged body/ID with witnesses removed |
| Duplicate timeout deletes a still-pending transaction | Fixed by the same guard; no duplicate retry is scheduled | Enabled duplicate delivery plus direct attempts at every production TTL |
| Expiry strands dependent work after all messages drain | Retained finite-retry policy; re-submission wakes the next leader | Three honest parties, two dependent transactions |
| Early invalid signature blocks a valid replacement | Retained acknowledgement policy; aggregate verification still rejects | Invalid partial signature, final rejection, corrected signature rejected |
| Decommit spends an unconfirmed output that ReqSn cannot resolve | Fixed: confirm dependencies first, then request the decommit | Three honest parties; parent/child/decommit; every emitted message delivered |
| Snapshot pruning restores a pending decommit's reserved input | Fixed: rebase preserves the input reservation | Same sequence, checked immediately after snapshot 1 is requested |
| Oversized proposal strands the request marker (#2924/#2927) | Fixed: bounded selection, capacity-aware pruning, checked live proposals | [Capacity demonstration and tests](capacity.md); upstream-reported defect |

## Protecting locally applied transactions

Read `ReqTx.machine` in
[Rules/ReqTx.hs](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqTx.hs). Before recording receipt or calling the ledger, it checks whether the transaction
ID is already in `localTxs` and `allTxs`. If so, the attempt completes with no events or
effects. Other deferred transactions are still retained: they can be needed for
snapshot validation even when they conflict with the optimistic local ledger.

The [Cardano regression](../../../../../hydra-node/test/Hydra/HeadLogicSpec.hs),
**“ignores a witness-stripped duplicate and preserves the validated registry
entry”**, applies a valid transaction, removes its witnesses, checks that its ID
is unchanged and the ledger rejects the altered payload, then delivers it. The
registry retains the valid transaction and the same snapshot request succeeds
before and after the duplicate. The frozen handler still waits after staging the
invalid replacement, showing how the old rule failed.

Previously, the repeated transaction could also exhaust its retries. Receipt
followed by `TxInvalid` deleted its registry entry while leaving `localTxs` and
`localUTxO` applied. The updated regression checks a duplicate delivery and all
TTL values from zero through `defaultTxTTL`: every attempt is a no-op and consults
no ledger. The resulting snapshot request resolves its transaction.

The [regression scenario](traces/registry-regression-v2.json)
and [trace](traces/registry-regression-v2.json.mmd)
show this three-action sequence. Old failing tapes with scheduled retries are
intentionally no longer executable: a successful no-op does not enable a retry.
The frozen handler preserves the historical failure evidence.

This fix prevents new corruption; it does not repair an already inconsistent
state recovered from old events. It changes the duplicate-ID rule, not the
production persistence schema or aggregation of historical events.

## Finite expiry and progress

The [22-action scenario](traces/expired-dependency-v2.json)
and [trace](traces/expired-dependency-v2.json.mmd)
deliver the child before its parent at all parties. Bob exhausts its child retry
budget before the parent arrives. Alice and Carol retain their retries. All
remaining queues drain and snapshot 1 confirms everywhere, but Bob leads snapshot
2 without its child payload. Alice and Carol have pending child transactions;
neither can request the next snapshot. No queued message remains to wake Bob.

Retries still have a finite budget. After expiry, the client must re-submit the transaction.
The test checks that re-submitting the child to Bob immediately emits the missing
ReqSn. Retaining a payload alone would not guarantee a leader wakeup; dependency
reactivation would require resource and scheduling rules beyond this increment.
This test shows the effect of retry expiry on progress. It does not model the
timing of the production transport, and no Agda liveness theorem covers it.

Random fair-delivery tests confirm the complete dependent chain when pending
messages arrive before retry exhaustion. Arbitrary scheduled-prefix tests check
safety separately. No unbounded chain-progress or Byzantine liveness claim follows.

## Invalid acknowledgement recovery

In [`AckSn.machine`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/AckSn.hs), partial signature collection commits without
individual verification. An invalid Alice signature is retained; after Bob's
valid signature, Carol's valid final signature triggers `InvalidMultisignature`.
Rejection correctly discards Carol's tentative event. Alice's corrected signature
then gets `SnapshotAlreadySigned`, leaving the round unable to confirm through
these acknowledgements.

This requires a bad payload from an authenticated party, which can already block
n-of-n progress by withholding signatures. The handler keeps the existing
event order and aggregate verification; there is no automatic replacement
or recovery guarantee. Closing remains the protocol exit. Individual signature
verification or replacement is a separate protocol change. The duplicate partial
`PartySignedSnapshot` event is also preserved; it is redundant but idempotent in
the current map aggregation.

## Decommit dependencies and reserved inputs

The conversion makes the incompatible ledger contexts explicit:
[`ReqDec.machine`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqDec.hs) validates
against `localUTxO`; [`ReqSn.machine`](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/ReqSn.hs)
resolves the decommit against the confirmed snapshot *before* applying the
requested ordinary transactions. A decommit accepted by the former can therefore
be impossible in the latter's next request.

The [checked demonstration](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/DecommitDependency.hs)
uses three honest parties, no faults and no expired retries. Start with output 1:

| Delivery stage | Result |
| --- | --- |
| All parties receive tx1: `1 → 2` | Alice requests snapshot 1 containing tx1; delay its delivery. |
| All receive tx2: `2 → 3` | tx2 applies locally while snapshot 1 is in flight. |
| All receive decommit tx3: `3 → 4` | The decommit applies locally and reserves input 3. Local UTxO is empty. |
| Deliver ReqSn 1 to everyone | It snapshots tx1 and retains tx2. Rebuilding local UTxO restores output 3 while the pending decommit remains. |
| Deliver all AckSn 1 messages | Snapshot 1 confirms. Bob requests snapshot 2 with tx2 and decommit tx3. |
| Deliver ReqSn 2 to everyone | The confirmed UTxO is `{2}`. Decommit tx3 needs `{3}`, so every party rejects `SnapshotDoesNotApply 2 3`. |

All 24 deliveries complete. No message remains queued; snapshot 1 stays confirmed
and Bob remains in `RequestedSnapshot 1 2`. No AckSn 2 is emitted. The safety
check prevents an invalid snapshot, but this honest sequence stops progressing.
The historical test uses the frozen handlers and reducer. The corrected test
uses production execution and replay, checking independent invariants after
every delivery. The structural refactor passed legacy comparisons before the
behavior fix.

The reservation loss is a separate observable state error in the same sequence.
[`Transactions`](../../../../../hydra-node/src/Hydra/HeadLogic/Machine/Coordinated/Transactions.hs) removes
the decommit's consumed input when applying `DecommitRecorded`.
The old `Rebase` rebuilt `localUTxO` from the snapshot and retained ordinary
transactions without preserving that reservation. It now removes pending spending
inputs after the rebuild.

```sh
cabal run hydra-node:head-logic-lab -- decommit-dependency --historical /tmp/decommit-old.json +RTS -N1
cabal run hydra-node:head-logic-lab -- decommit-dependency /tmp/decommit-fixed.json +RTS -N1
just test hydra-node IncrementalWorkflow
```

Each command writes complete inputs, outcomes, states and a `.mmd` instruction
trace. The [historical JSON](traces/decommit-dependency-historical-v2.json) and
[trace](traces/decommit-dependency-historical-v2.json.mmd) preserve the failure.
The [corrected JSON](traces/decommit-dependency-fixed-v2.json) and
[trace](traces/decommit-dependency-fixed-v2.json.mmd) show the repair. Both appear
in the [central diagram index](../../../../head-logic-diagrams.md).

Start with **Party 0: ReqDec 3**, then **Party 0: ReqSn 1**. In the historical
run, rebase restores output 3; the JSON's `reservationHeld` becomes false.
**Party 1: AckSn 1** then requests tx2 and the decommit together;
`decommitRequestsReady` becomes false. ReqSn 2 rejects before applying tx2.

In the corrected run, rebase keeps output 3 reserved. Snapshot 2 carries only
tx2. After confirmation, the next leader sees the decommit's inputs in the
confirmed base and requests snapshot 3 with no ordinary transactions. All parties
confirm it and the message queue drains. A carried decommit alone does not
trigger endless new rounds.

[SnapshotRequest.decommitStatus](../../../../../hydra-node/src/Hydra/HeadLogic/Rules/SnapshotRequest.hs)
is shared by every leader request site. Cardano availability checks spending,
reference and collateral inputs; ReqSn still performs full ledger validation.
This matches the ledger's [required-input check](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/babbage/impl/src/Cardano/Ledger/Babbage/Rules/Utxo.hs).
A real Cardano payment-chain regression checks dependency-first confirmation;
a separate property checks the three input kinds and reservation arithmetic.

The three-party demo is deterministic, not an exhaustive or minimized explorer
result. The generic explorer still excludes incremental actions. The Agda model
now checks reservation-preserving rebase and new-work selection over bounded
SimpleTx inputs; it does not model scripts, time or general distributed progress.
Existing event/checkpoint encodings are unchanged. Restoring an old checkpoint
alone does not repair its contents; corrected rebase applies when the next
snapshot is recorded.

## Properties and reference tests

Read [SnapshotProgramSpec.hs](../../../../../hydra-node/test/Hydra/HeadLogic/SnapshotProgramSpec.hs)
and [Explorer.hs](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/Explorer.hs):

- Every locally pending transaction must equal its registry payload, not merely
  share an ID. This catches replacement as well as deletion.
- Applying the pending sequence to the current snapshot must reproduce the local
  UTxO. This independent Simple-ledger calculation uses neither `applyTxTo` nor
  production state aggregation.
- Real confirmed signatures, agreement and transaction dependency order hold at
  every checked prefix. Quiescent unconfirmed work is a separate progress failure.
- Fair delivery before retry exhaustion confirms both dependent transactions.

The Agda reference adds `reqTxKnownOutcomeRef`; the literate spec
proves the already-applied branch has no changes/effects regardless of primitive
answers. The bounded snapshot model now has named phases and outcomes. These
models do not prove registry lifetime or scheduler progress in the whole node.

Agreement with the old handler or with event replay can preserve the same bug
in both implementations. Fresh-ID migration comparisons remain;
regressions check the new duplicate behavior against the unchanged old handler.

The witnesses arose from instruction inspection and focused schedules. Default
DFS did not automatically discover the registry witness. The revised expiry
family with retry-first search also reached 100,000 states and 15,906 depth
cutoffs without finding the known progress failure. It was truncated. Additional
search strategies improve control of exploration, not its completeness claim.

The subsequent `retry-windows` search finds the expiry failure after 234 states
and 22 deliveries. It varies an initial delivery prefix and independent retry
windows at the three parties, then drains traffic. This restricted schedule
family avoids spending the budget on unrelated permutations. It uses production
retry eligibility and the existing invariant, minimizer and replay. Three depth
cutoffs remain; the search is not exhaustive. A regression requires discovery
within the default budget. See the [demonstration](demonstration.md).

The settlement investigation checks 24 three-party deposit/spend/rollback
schedules, including confirmation before or after rollback and both sides of
the increment's observation slot. All pass, including complete state comparisons
after serialized event/checkpoint replay. No new settlement bug was found. The
driver assumes valid chain observations and does not cover crash durability or
Cardano settlement validation.

## Reproduction and validation

```sh
just test hydra-node witness-stripped
just test hydra-node SnapshotProgram
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-experiments/traces/registry-regression-v2.json --normal
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-experiments/traces/expired-dependency-v2.json --normal
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-experiments/traces/expired-dependency-v2.json --step
```

The registry replay reports no invariant failure; expiry replay reports quiescent
unconfirmed transactions. Prepared stepping shows full staged payloads and named
decisions. The [lab validation record](lab.md#validation-record)
contains the final verification results for this revision.

## Complete input conversion

The remaining handlers are frozen in
[LegacyHandlers](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/LegacyHandlers.hs).
Full-input comparisons cover init, synchronization, client decommit, deposit
observation and expiry, settlement, close/contest adoption, side-loading, fanout
and rollback. The structural conversion and decommit fixes are separate commits. Legacy
outcome comparisons now exclude the intentionally changed decommit cases; the
corrected sequences and bounded independent checks cover them.

A suspected previous-head recovery bug was ruled out by a two-head test.
`eventHeadId DepositRecovered` returns `Nothing`, so the aggregate consumes the
registered deposit even when a different head is active. The older test checked
only event emission and used the same head ID; the new test checks the resulting
pending-deposit map with distinct IDs. See `recover-previous-head` in
[InputComparison](../../../../../hydra-node/testlib/Test/Hydra/HeadLogic/InputComparison.hs)
and `RulesSpec`'s recovery assertion. This is regression coverage, not a defect.

## Checks at the component boundary

[Upstream PR #2927](capacity.md) identifies another gap at this boundary: an
honest leader can persist a request that every follower rejects for accumulator
capacity. This branch still has that behavior. The investigation maps the fix to
the shared selector and owners, and proposes independent request/progress checks.
The upstream fix is not applied before the requested rebase.

The [coordinated owners](../../../../head-logic-coordinated.md) expose where each
fix belongs. `reservationHeld` first fails at historical rebase;
`decommitRequestsReady` fails at the next historical proposal. Both hold at every
prefix of the corrected sequence. Crem's Reserved phase alone establishes neither
property: explicit payload invariants and independently stated tests are still
necessary.

## Trace comparison correction

The diagram generator previously reused the production trace for both comparison
panels. This became misleading when capacity added decisions. Each panel now
shows its own execution. Ordered outcomes, primitive queries, staged payloads
and unchanged decisions still compare; the explicitly listed capacity decisions
and moved useful-work decision have separate regressions.
