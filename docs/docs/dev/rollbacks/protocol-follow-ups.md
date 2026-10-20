# Protocol changes deferred from rollback recovery

Rollback recovery retains signed snapshots, records whether their settlements
landed, and submits erased settlements again in version order. It preserves the
existing rules for proposing and signing snapshots and accepting decommit
requests.

The changes below need separate protocol review. They are candidates, not a
claim that the earlier implementations are complete solutions. A Head is
**stuck** here when it cannot confirm another off-chain snapshot without
additional intervention. Closing the Head may still be possible.

Each timeline distinguishes a lasting stall from a temporary wait. A handler
test that shows a refusal is evidence for that decision; it does not by itself
prove that every real execution with that refusal stalls permanently.

## 1. Snapshot requests crossing a settlement

**Candidate:** Accept a request one version behind only when it is based on the
confirmed snapshot and preserves its deposit or decommit. Build and sign the
snapshot using the requested version.

**Reason:** Observing an increment or decrement advances a node's version.
Parties can observe that settlement at different points in the same snapshot
round. The existing version-equality rule can then prevent them from finishing
that round.

**Timeline:**

1. Alice and Bob have confirmed snapshot `S` at version `v`, with a pending
   deposit or decommit.
2. Alice leads the next round and broadcasts `ReqSn(v, S.number + 1)`, carrying
   that same action and some new work.
3. Alice processes the request and starts collecting signatures.
4. Bob observes the settlement first, advancing his local version to `v + 1`.
5. Bob then receives the request at `v`. He waits for version equality instead
   of signing it.
6. Bob's local off-chain version does not decrease, even after a chain rollback.
   Retrying the message cannot restore equality. If the request exhausts its
   retries, it is dropped.
7. Alice needs Bob's signature, and her signature-collection state prevents a
   replacement proposal. Further transactions do not finish this round.

This is a lasting stall under that delivery order; no rollback is required.
The opposite ordering, where the receiver is behind the request, can resolve
when its chain follower catches up.

**Review needed:** Which older versions can safely be signed; exact action
identity; the ledger used to build the signed bytes; and compatibility with
close and contest. A one-version exception does not resolve arbitrary version
differences.

The earlier candidate and handler regressions are in
[cf2ee89a](https://github.com/cardano-scaling/hydra/commit/cf2ee89a03ecf1f0096003063d11ac5b1150339d).

## 2. The same delayed claim after its settlement is rolled back

**Candidate:** If a delayed request is allowed to carry the confirmed snapshot's
deposit again, that decision should remain consistent when another node has
seen the increment land and then be erased.

**Reason:** Retention records what has already been signed. Its landed or
erased status describes one node's chain observations. Mixing those facts into
request eligibility can make parties disagree on the same request.

**Timeline:**

1. Both parties confirm a snapshot claiming deposit `D` at version `v`.
2. Alice broadcasts the next request carrying `D` and starts collecting
   signatures.
3. Before receiving that request, Bob observes the increment and then a
   rollback that erases it. He retains the signed settlement as erased and
   keeps local version `v + 1`.
4. Alice has not observed the increment and still uses version `v`.
5. With the unchanged version-equality rule, Bob waits for the older version,
   as in case 1. If only the landed-only version of candidate 1 is introduced,
   Bob instead refuses the retained deposit.
6. Reposting the increment repairs the chain, but does not supply Bob's
   missing signature or automatically restart Alice's round.

This is a rollback variant of case 1, not a reason to broaden claim acceptance
in the retention work. It must be reviewed together with delayed requests.
A different retained deposit must remain excluded.

The earlier refinement and regression cases are in
[2f3fa653](https://github.com/cardano-scaling/hydra/commit/2f3fa65315a7c12712b0ff0fa59b94398a53e9a9).

## 3. Decommit requests while deposits are pending

**Candidate:** Record a valid decommit request independently of a locally
pending deposit, reserve its inputs, and propose it after the commit settles.

**Reason:** The existing handler waits for a pending commit and eventually
reports `DepositInFlight`. Deposit activation depends on each node's ticks,
so the same broadcast can be recorded by one party and refused by another.

**Timeline:**

1. Alice has activated deposit `D`; Bob has not yet processed that activation.
2. Both receive the same valid decommit request `X`.
3. Alice waits on `D`. Bob records `X`, reserves its inputs and waits for a
   snapshot proposing it.
4. If `D` remains pending until Alice's retries are exhausted, Alice refuses
   `X`. Bob still holds it as his pending decommit.
5. When `D` later settles, Alice may have no recorded `X` to propose. Repeating
   `X` can also meet Bob's existing-decommit guard.

This establishes divergent pending state and a decommit that may fail to
complete. A complete Head stall additionally needs a round or transaction
whose progress depends on resolving that disagreement. A recording node that
later becomes leader may propose `X`, so this timeline alone is not proof of
an inevitable permanent Head stall.

**Review needed:** Distinguish a queued request from an action selected for a
snapshot, preserve input reservations when rebuilding the local ledger, and
define duplicate-request and cancellation behavior. The executable Agda rule
currently requires no pending commit; the proposed queue needs a real model
change, including its invariants and agreement tests.

The earlier candidate is in
[fe6226d8](https://github.com/cardano-scaling/hydra/commit/fe6226d888660db2b6cd8506dd29eed46b480c47).

## 4. Signed claims across deposit re-observation and recovery

**Candidate:** Recognize the confirmed snapshot's claim independently of local
activation, and define explicitly when recovery permits a replacement.

**Reason:** A signed snapshot survives rollback. If the deposit transaction is
also rolled back and then observed again, its local record starts as
`Inactive`, although the confirmed snapshot still claims it. Recovery introduces
a separate question: whether the next snapshot must first drop the old claim
or may replace it immediately.

**Re-observation timeline:**

1. The parties confirm a claim on `D`.
2. Bob rolls back before the deposit transaction and later observes it again.
   His new deposit record is `Inactive`; the transaction's original creation
   time is unchanged.
3. Alice proposes another snapshot carrying the agreed claim.
4. Bob waits for activation. A later tick can resolve this wait.
5. If the request is dropped before activation is processed, Alice can remain
   waiting for Bob's signature.

The status difference itself is not a permanent stall. The dropped-request
step, and the absence of another retry, matter.

**Recovery timeline:**

1. The confirmed snapshot claims `D`, which is then recovered on chain.
2. A fresh deposit `E` is available at the same Head version.
3. A proposal that immediately substitutes `E` meets the existing check that
   the confirmed pending claim must be preserved.
4. If the leader has already entered a round for that proposal, rejection can
   leave the round unfinished.

The existing tick path can instead propose an empty claim first and then claim
`E` in a later snapshot. Immediate replacement therefore needs its own
justification; it is not required merely to retain and replay settlements.
An end-to-end reproduction must establish which proposal path reaches step 3.

The earlier combined candidate is in
[7a43dea6](https://github.com/cardano-scaling/hydra/commit/7a43dea64945f4bdcda2bc2b27153cbd90537586).

## 5. Refusals based on local expiry or recovery timing

**Candidate:** Define which observations justify a permanent refusal and which
require waiting. The earlier implementation accepted a fresh claim despite
local expiry and waited for recovery before refusing a replacement.

**Reason:** An honest leader and a follower can process different ticks or
chain observations before receiving the same request. An immediate error is
not retried by the node.

**Expiry timeline:**

1. Deposit `D` is active for Alice, the leader.
2. Alice selects `D`, processes her proposal and begins collecting signatures.
3. Bob processes the expiry cutoff before receiving the proposal.
4. Bob refuses the fresh claim with `RequestedDepositExpired`.
5. Alice still needs Bob's signature. Bob does not retry the rejected request,
   and Alice does not replace a request while collecting signatures.

This can leave an unfinished round even without a rollback.

**Recovery timeline, if immediate replacement from case 4 is adopted:**

1. Both parties have a confirmed claim on `D`.
2. Alice observes recovery of `D`, then proposes a replacement claim on `E`.
3. Bob receives the proposal before observing recovery.
4. Bob refuses it because he still tracks `D`.
5. Bob later observes recovery, but the rejected request is already gone.
   Alice continues waiting for his signature.

**Review needed:** Signing an expired fresh claim has settlement and
recoverability consequences. Waiting for recovery also needs a retry policy
that does not silently abandon the round. Neither change should be accepted
solely because it makes a model walk pass.

The earlier candidate and state-matrix tests are in
[f804689f](https://github.com/cardano-scaling/hydra/commit/f804689f13230d473e7bdb3c1742c50d9d241ae7).

## Separate hardening: verify acknowledgements before recording them

**Candidate:** Check each acknowledgement's signature against the requested
snapshot before adding it to the collected signatures.

**Timeline:** A party first sends an invalid signature, which occupies its
entry. Its later valid signature is treated as a duplicate. Once all entries
exist, aggregate verification fails, and the round cannot complete with those
stored signatures.

This concerns faulty messages rather than rollback recovery. The earlier fix
and real-signature tests are in
[b9413934](https://github.com/cardano-scaling/hydra/commit/b941393494dfd0250be9562a55eb041b556346c7).

## Testing boundary

Retaining a signed settlement does not guarantee that it can land again. If a
rollback erases an increment and its deposit cannot be spent by a replacement
increment before the validity limit, the off-chain version can remain ahead of
the chain. The existing close path may then also use a version the chain no
longer has. This requires separate recovery or protocol design; retaining more
snapshots and retrying them does not solve it. Rollback of the Head's creation
is another separate boundary.

Rollback tests must continue to cover independent chain delivery, multiple
erased settlements, re-observation, restart, catch-up and partial fanout.
Restricted model walks must state that they observe outstanding settlements
before initiating payments or new decommits. Passing those walks does not
establish liveness for the timelines above.

Future protocol work should first turn each relevant timeline into a
deterministic multi-node regression, then update the specification, executable
Agda reference and implementation together. Agreement should cover waiting and
rejection as well as signing, including pending commits.
