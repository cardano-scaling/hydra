# Head logic bugs found with the spike

The investigation found two bugs in the transaction map (`allTxs`) and two cases
where a Head stops making progress. The transaction-map bugs are fixed. Retry
expiry and acknowledgement replacement keep their existing behavior, documented
below. These cases use the normal interpreter; they do not bypass verification.
None demonstrates an invalid confirmation or loss of funds.

The performance investigation also found unnecessary UTxO evaluation introduced
by this spike. The compatibility interpreter discarded the resolved-change
endpoints, but `StrictData` forced them first. Both instruction and staged-change
fields are now explicitly lazy; a regression test checks that compatibility
interpretation does not evaluate them. This is an implementation cost, outside
the Agda model, rather than a bug in the previous protocol rules. See the
[measurement report](./docs/dev/architecture/head-logic-benchmark.md#what-the-investigation-changed).

| Finding | Current result | Evidence |
| --- | --- | --- |
| Invalid witness variant replaces a validated transaction | Fixed: locally applied IDs are ignored before staging receipt | Real Cardano ledger; one-party head; unchanged body/ID with witnesses removed |
| Duplicate timeout deletes a still-pending transaction | Fixed by the same guard; no duplicate retry is scheduled | Enabled duplicate delivery plus direct attempts at every production TTL |
| Expiry strands dependent work after all messages drain | Retained finite-retry policy; re-submission wakes the next leader | Three honest parties, two dependent transactions |
| Early invalid signature blocks a valid replacement | Retained acknowledgement policy; aggregate verification still rejects | Invalid partial signature, final rejection, corrected signature rejected |

## Protecting locally applied transactions

Read `onOpenNetworkReqTxProgram` in
[HeadLogic.hs](../hydra-node/src/Hydra/HeadLogic.hs). Before recording receipt or calling the ledger, it checks whether the transaction
ID is already in `localTxs` and `allTxs`. If so, the attempt completes with no events or
effects. Other deferred transactions are still retained: they can be needed for
snapshot validation even when they conflict with the optimistic local ledger.

The [Cardano regression](../hydra-node/test/Hydra/HeadLogicSpec.hs),
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

The [regression scenario](./docs/dev/architecture/head-logic-traces/registry-regression-v2.json)
and [trace](./docs/dev/architecture/head-logic-traces/registry-regression-v2.json.mmd)
show this three-action sequence. Old failing tapes with scheduled retries are
intentionally no longer executable: a successful no-op does not enable a retry.
The frozen handler preserves the historical failure evidence.

This fix prevents new corruption; it does not repair an already inconsistent
state recovered from old events. It changes the duplicate-ID rule, not the
production persistence schema or aggregation of historical events.

## Finite expiry and progress

The [22-action scenario](./docs/dev/architecture/head-logic-traces/expired-dependency-v2.json)
and [trace](./docs/dev/architecture/head-logic-traces/expired-dependency-v2.json.mmd)
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

In `onOpenNetworkAckSnProgram`, partial signature collection commits without
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

## Properties and reference tests

Read [SnapshotProgramSpec.hs](../hydra-node/test/Hydra/HeadLogic/SnapshotProgramSpec.hs)
and [Explorer.hs](../hydra-node/testlib/Test/Hydra/HeadLogic/Explorer.hs):

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
within the default budget. See the [demonstration](./head-logic-demonstration.md).

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
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-traces/registry-regression-v2.json --normal
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-traces/expired-dependency-v2.json --normal
nix run .#head-logic-lab -- replay docs/docs/dev/architecture/head-logic-traces/expired-dependency-v2.json --step
```

The registry replay reports no invariant failure; expiry replay reports quiescent
unconfirmed transactions. Prepared stepping shows full staged payloads and named
decisions. The [lab validation record](./docs/dev/architecture/head-logic-lab.md#validation-record)
contains the final verification results for this revision.
