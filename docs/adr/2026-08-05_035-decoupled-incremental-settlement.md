---
slug: 35
title: |
  35. Decoupled incremental settlement: deposits settle first, withdrawals exit through a queue
authors: [v0d1ch]
tags: [Draft]
---

## Status

Draft

## Context

- Today a snapshot is two things at once: an **L2 ledger state** and a **promise about future L1 transactions**. `utxoToCommit` (Uα) says "an increment claiming this deposit will land"; `utxoToDecommit` (Uω) says "a decrement paying these outputs will land". Both reference transactions that have not happened yet and may never happen.

- The rest of the settlement machinery exists to manage those promises:
  - The `version` counter sequences them, and it has two asynchronous writers reasoning about each other: L2 consensus signs "version `v` with a commit pending" while L1 observation bumps the version when the settlement lands.
  - Close needs the `CloseUnused`/`CloseUsed` redeemer variants to answer "did the promised settlement land or not".
  - Off-chain code compares versions in many places to decide whether pending funds are spendable.

- This coupling is a persistent source of defects. All of the following are real, historical bugs or open gaps in this area:

  - snapshots stuck on version races between `ReqSn` and `CommitFinalized`/`DecommitFinalized` observations;
  - a pending deposit re-injected into the spendable L2 UTxO on every snapshot (double-spend);
  - `CommitFinalized` corrupting `localUTxO`;
  - deposit activation racing in-flight snapshots;
  - the commit/decommit mutual exclusion required by the specification but not enforced off-chain;
  - a `ReqSn` omitting the pending deposit silently drops it from the next snapshot with no validation, while the increment can still land;
  - the head **wedging permanently** when a signed snapshot's increment or decrement transaction can never land on L1 (for example when the merged head output would exceed the ledger's `maxValSize`). Nothing can process further deposits or decommits, and once the referenced deposit expires even plain L2-transaction snapshots are blocked.

- Settlements are strictly sequential: at most one pending commit and one pending decommit, and each costs a full snapshot round-trip plus a version bump before the next one can start. L2 transaction throughput is excellent; the settlement pipeline is the bottleneck and the fragile part.

- Guarding individual entry points against individual failure causes does not fix the structural problem: as long as a signed snapshot can reference a settlement that never happens, some failure cause, foreseen or not, can wedge the head.

## Decision

Redesign incremental settlement around one rule:

> **A signed snapshot never references an unsettled L1 event.** A snapshot contains only (a) state derived from L1 transactions the node has already observed and (b) L2 state that only ever grows and never needs undoing. Nothing in a signed snapshot can "fail to happen".

Concretely, the two directions become:

- **Deposits (L1 → L2): settle first, then snapshot.** The increment lands on L1 first; L2 picks the funds up by observing it, the same way it reacts to any other chain event.
- **Withdrawals (L2 → L1): a one-way exit queue.** An exit is an ordinary L2 transaction that moves outputs into an ordered queue inside the snapshot state. Queue entries can be paid out on L1 at any time, by anyone, in order; whatever is unpaid when the head closes is paid by fanout. Nothing ever needs cancelling.

### The change at a glance

```haskell
-- Today
data Snapshot tx = Snapshot
  { headId         :: HeadId
  , version        :: SnapshotVersion     -- sequences settlement promises
  , number         :: SnapshotNumber
  , confirmed      :: [tx]
  , utxo           :: UTxOType tx         -- U : spendable on L2
  , utxoToCommit   :: Maybe (UTxOType tx) -- Uα: increment that SHOULD land
  , utxoToDecommit :: Maybe (UTxOType tx) -- Uω: decrement that SHOULD land
  , accumulator    :: HydraAccumulator
  }

-- This ADR
data Snapshot tx = Snapshot
  { headId      :: HeadId
  , number      :: SnapshotNumber
  , confirmed   :: [tx]
  , utxo        :: UTxOType tx       -- U: spendable on L2; deposits appear here
                                     --    only after their increment is observed
  , exitQueue   :: ExitQueue tx      -- (index, output) entries, ordered; only
                                     --    grows; pruned by observing decrements
  , anchor      :: Hash              -- which increments 'utxo' already absorbed
  , accumulator :: HydraAccumulator  -- A_S: commitment over utxo + exitQueue
  }
```

The multisigned message shrinks accordingly:

```
today    : (headId, version, number, A_S, H(utxoToDecommit), H(utxoToCommit))
this ADR : (headId, number, A_S, anchor)
```

And the open head datum trades the version for settlement bookkeeping with single writers (details below):

```
today    : ..., version
this ADR : ..., pendingIncrements, exitsPaid
```

Supporting principles:

- **Funds appear on L2 only by observation**: after their settlement transaction is seen on-chain plus a maturity buffer (the existing deposit-activation buffer mechanism, reused).
- **L2 → L1 authorizations are per-deposit certificates or queue entries**, never promises baked into a snapshot. Replay protection comes from the UTxO model or from a counter with a single writer, never from cross-layer version comparison.
- **On-chain binding uses hashes** (the accumulator commitment, a hash-chain anchor). The only counters kept are written by exactly one side, and single-writer counters cannot race.
- **The failure of any settlement transaction leaves L2 completely unaffected.** Settlements can be retried forever, batched, or abandoned. Deposits and withdrawals do not exclude each other, and arbitrarily many of each may be in flight.

### Pipeline 1 (deposits): settle, then snapshot

Side by side with today's flow:

Today:

1. The deposit transaction lands; the node records it and waits until it considers the deposit active.
2. The leader references it in a `ReqSn`; parties sign a snapshot carrying `utxoToCommit` at version `v`. This is the promise.
3. The increment is posted with that snapshot's signature; the version bumps on-chain.
4. `CommitFinalized` is observed and the funds become spendable.

If step 3 can never succeed, the promise is already baked into the signed snapshot chain: the head wedges.

This ADR:

1. **The deposit transaction lands: unchanged.** Same validator, same deadline semantics (`Claim` needs the transaction validity upper bound at or before the deadline, `Recover` needs the lower bound after it).
2. **Parties sign a claim certificate.** Each party runs acceptance checks (minimum ADA, a dry-run increment against L1 limits and the *current* head value, token policy limits) and multisigns `(headId, depositTxId, commitsHash)` in a lightweight network round, similar to `AckSn` but bound to no snapshot number and no version. If any party declines, the deposit expires and the depositor recovers; that is the same n-of-n liveness assumption the protocol already has. Honest parties stop signing certificates one deposit period before the deadline (today's `Expired` buffer, reused). They also stop signing new certificates while more than N increments remain unabsorbed by the newest snapshot they have signed: certificates are n-of-n, so any single party enforces this bound alone, and because the chain cannot grow without a party's own certificates, every party unilaterally bounds the close-time replay it may ever face (see close below).
3. **Anyone posts the increment.** It spends the head output and the deposit output, presenting the certificate. The validator checks that the head value grows by exactly the deposited value and appends to the datum's hash chain: `pendingIncrements' = H(pendingIncrements <> commitsHash)`, where `H` is blake2b-256 and `<>` is byte concatenation. No snapshot signature, no version. Increments commute: any number can land in any order, chaining on the head output.
4. **Observation makes it real on L2.** Once the increment is observed and the maturity buffer has passed, the deposited outputs appear in `utxo` of the next snapshot, and that snapshot's `anchor` advances to the chain value it has absorbed.

If the increment never lands (oversized, fees, any unforeseen reason), there is nothing to clean up: L2 never referenced the deposit. It expires and the depositor recovers through the existing API. The node never auto-posts anything; the head does not care.

### Pipeline 2 (withdrawals): the exit queue

Withdrawals cannot be settle-first, because funds leaving the head need L2's authorization. So the authorization is made irrevocable instead: a queue that only grows, where being in the queue already guarantees payout, by decrement or, at the latest, by fanout.

Today:

1. The client POSTs a signed decommit transaction to `/decommit`.
2. The leader puts its outputs into `utxoToDecommit` in the next `ReqSn`; parties sign the promise. At most one decommit in flight, mutually exclusive with a pending commit.
3. The decrement is posted with that snapshot's signature; the version bumps; `DecommitFinalized`.

Cancelling a signed decommit is unsafe (double-spend hazard), and a decrement that can never land wedges the head.

This ADR:

1. **An exit request is an ordinary L2 transaction** that moves the owner's outputs into the exit queue, which is part of the snapshot state. Entries get consecutive indices in snapshot-confirmation order. Full L2 throughput, many exits per snapshot, nothing blocks. **Exits are final**, the same finality a rollup withdrawal has.
2. **The node checks each exit output at entry**: minimum ADA, `maxValSize`, and "a decrement paying just this output fits L1 limits". Because a decrement can pay any batch size down to one, this per-output check is complete: if the output passes, some decrement can always pay it. (Today's `utxoToDecommit` set is validated nowhere.)
3. **Anyone posts a decrement paying the queue in order.** It pays entries `[exitsPaid, exitsPaid + k)`, presents *any* confirmed snapshot's signature plus membership proofs for those entries, and bumps the datum counter `exitsPaid += k`. The validator checks the outputs verbatim against the proven entries and that the head value shrinks by exactly their sum. Decrements need no anchor and no version: an older snapshot can pay older entries, it just cannot prove membership of newer ones.
4. **Observation prunes the queue.** Seeing a decrement on-chain is what removes paid entries from the L2 queue view.

Double-pay is structurally impossible: `exitsPaid` is written only by L1 decrement transactions, so a stale signed decrement re-landing targets already-paid indices and fails the contiguity check. No deadline, no off-chain discipline required. And if no decrement ever lands, entries sit in the queue (unspendable on L2) until fanout distributes them. Funds are always safe; the head is never blocked.

### First-class batching

- **Multi-deposit increments**: certificates are per-deposit and increments commute, so one increment can spend the head output plus N deposit outputs (one certificate each). Batching is an optimization with a guaranteed one-at-a-time fallback.
- **Multi-exit decrements**: the default shape. One decrement settles a contiguous queue prefix, sized by the existing partial-fanout chunking machinery, from the membership-proof batch limit of the published reference string (29 entries today, the same bound a partial-fanout chunk has) down to `k = 1`. Longer prefixes chain across consecutive decrements.
- **Settlement chaining**: settlements chain directly on the head UTxO, so several increments and decrements can land in consecutive blocks, or within one block via transaction chaining. Today each one costs a snapshot round-trip and a version bump, one at a time.

### Certificate traffic gets its own channel

The certificate round and the snapshot stream have opposite delivery requirements. Snapshot messages are ordered and liveness-critical: L2 throughput stalls if they stall. Certificates need no ordering (they are per-deposit and commute), tolerate loss and delay (signing is idempotent, re-broadcast is free, and the worst case is an expired deposit that recovers), and are a few hundred bytes regardless of L2 traffic.

Running certificates on their own logical channel, separate from the ordered snapshot stream, keeps the two from head-of-line blocking each other under load: deposit settlement latency scales with deposit rate rather than with L2 transaction volume, and a burst of deposits cannot delay snapshot signing. Whether that channel shares the existing transport with its own queue or becomes a separate best-effort mechanism is an implementation decision; the protocol only requires eventual delivery before the certificate deadline.

### On-chain data model

The open head datum needs no version and no live accumulator. It carries, besides the immutable parameters:

| Field | Writer | Purpose |
| --- | --- | --- |
| `pendingIncrements : Hash` | increment txs (append) | hash-chain log of claimed deposits' `commitsHash`es |
| `exitsPaid : Int` | decrement txs | count of paid exit-queue entries, only ever grows |

Snapshots multisign `(headId, number, A_S, anchor_S)`: the accumulator over their full state (active UTxO plus exit queue) and the pending-chain value they have absorbed.

Two facts about the existing accumulator machinery matter for the exact commitment layout and should be weighed during specification:

- **Accumulator capacity.** The trusted setup bounds an accumulator's total element count. With a single `A_S` over active outputs and exit-queue entries, every unpaid exit entry consumes capacity otherwise available to the active UTxO set.
- **Membership-proof batch size.** The published reference string caps how many elements a single proof can cover (29 today), bounding `k` for one decrement exactly as it bounds one partial-fanout chunk.

An alternative layout keeps the single signing round but splits the commitment: multisign `(headId, number, A_active, Q_exit, anchor_S)`, where `A_active` is the accumulator over active outputs only and `Q_exit` commits to the ordered queue, for example a running hash chain. Paying a contiguous prefix then verifies by hash replay instead of a pairing check, freeing the queue from both the capacity budget and the batch cap, at the cost of a second commitment structure and proof path on-chain (element domain tags become unnecessary because the role separation is structural). This is a layout choice, not a design change: pipelines, datum fields, and reconciliation are identical under either option.

### Close, contest, and fanout

Today close must pick `CloseUnused` or `CloseUsed` by comparing the snapshot's version against the datum's, to resolve whether a promised settlement landed. Here there are no promises to resolve, only hashes to replay:

- **Close** presents a signed snapshot plus the suffix of `commitsHash`es from the snapshot's `anchor` up to the datum's current `pendingIncrements`; the validator replays the hash chain (one hash per element; the suffix length any party may need to present is bounded by the certificate policy of pipeline 1, not by on-chain state, since L1 cannot observe which increments a snapshot has absorbed). The closed datum records `A_S`, the snapshot number, the unabsorbed increment hashes, and `exitsPaid` at close.
- **Fanout** distributes three things: the active elements of `A_S`; the exit-queue entries with index ≥ `exitsPaid` (the counter overrides the snapshot's possibly stale queue view); and the unabsorbed increments' outputs verbatim (the preimages of the recorded `commitsHash`es are the deposit datums every node observed on-chain).
- **Contest** requires a strictly higher snapshot number, with the same anchor replay against the chain value frozen at close.
- Removed entirely: the `version` field, the `CloseUnused`/`CloseUsed` redeemer variants, `utxoToCommit`/`utxoToDecommit` in snapshots, `IncrementRedeemer.decommitOutputsHash`, `DecrementRedeemer.commitOutputsHash`, and the commit/decommit mutual-exclusion requirement.

### What carries over

- Deposit transaction, validator, deadline windows, and the recover flow/API: unchanged.
- The accumulator machinery becomes the central primitive (exit membership proofs, snapshot state binding) rather than fanout-only.
- Partial-fanout chunking is reused for batched exit payment.
- The deposit lifecycle tracking and activation buffers are repurposed for certificate timing and the increment maturity buffer.
- The existing dry-run increment size checking becomes the certificate acceptance check.
- `HeadLogic` shrinks: `currentDepositTxId`, `decommitTx`, `waitForDeposit`, `ReqSnCommitNotSettled`/`ReqSnDecommitNotSettled`, the version-race special cases and rollback re-posting all disappear.

## Consequences

Positive:

- Every bug class listed in the context dies structurally, not by patch:

  | Bug class | Why it cannot exist |
  | --- | --- |
  | Stuck head on failed increment | snapshots never carry `utxoToCommit`; an unclaimed deposit is invisible to L2 |
  | Stuck head on failed decrement | the exit queue only grows; payment is optional; fanout is the backstop |
  | Version races | there is no version; binding is by hashes, counters have a single writer |
  | Deposit double-spend | deposits enter `utxo` exactly once, on observation, after maturity |
  | Decommit-cancel double-spend | `exitsPaid` makes stale decrements invalid |
  | Mutual-exclusion gap | the pipelines are independent; the requirement itself is gone |
  | Silent deposit drop via `ReqSn` | there is no pending-commit field in snapshots to drop |
  | Sequential settlements | increments commute, exits batch, both unbounded |

- Settlement liveness becomes independent of L2 liveness: even if L2 consensus halts, signed exits remain payable by anyone, unclaimed deposits recover after their deadline, and close plus fanout distributes everything, including the queue and unabsorbed increments.
- Throughput: deposits and withdrawals batch and chain without snapshot round-trips in between.
- The node never auto-posts recovery transactions; recovering an expired deposit remains a depositor action through the existing API.

Negative:

- **Exits are final.** No cancelling an exit request (today's decommit is de-facto uncancellable once signed; this design just says so honestly).
- **Exit payment is FIFO.** An exit waits behind earlier queue entries. Every entry is L1-valid by construction and anyone can batch-pay the whole prefix, so the queue can be delayed but never stalled.
- **This is a protocol version bump**: all head validator branches change, script hashes change, the specification needs a rewrite of the incremental settlement sections, and the changes require security review. This is the cost of replacing the mechanism rather than continuing to patch it.

Neutral:

- Deposit-to-L2 latency is comparable to today (deposit → certificate → increment → observation + maturity → next snapshot), with far less machinery.
- API reshape (sketch, to be detailed at implementation time): `POST /decommit` becomes submitting an L2 exit transaction; deposit lifecycle server outputs change (certificate round instead of commit-approval via snapshot); `Snapshot` drops `version`/`utxoToCommit`/`utxoToDecommit` and gains the anchor and exit-queue view.

## Security considerations

Threat-by-threat results of an adversarial pass over the design. Trust model unchanged: safety against any dishonest minority (any single honest party can contest), liveness requires all n parties. Under a malicious party, every attack below degrades to *delay*, never loss of funds.

1. **Forging or replaying a claim certificate.** Replay is impossible: the increment spends the deposit UTxO, which is one-shot by the UTxO model. Forgery requires breaking the n-of-n multisig. A certificate from another head fails the `headId` binding; swapping the claimed content fails `commitsHash` (the deposit datum is immutable once on-chain). Certificates need no expiry: after the deposit deadline the `Claim` validator branch rejects anyway. Residual: nothing beyond the existing multisig assumptions.

2. **Racing the deadline (increment vs. recover).** A malicious party holding a certificate can post the increment right up to the deadline, ignoring the honest convention of stopping one deposit period earlier, while the depositor posts a recover just after it. Exactly one of them spends the deposit output, and L2 simply follows whichever it observes. Increment wins: funds enter the head, which was the depositor's original intent (they can leave via the queue). Recover wins: no increment can happen thereafter. Today this race is dangerous because L2 has *pre-signed* a snapshot referencing the deposit; here both outcomes are consistent by construction. Residual: a recovery can be front-run into a successful claim, an inconvenience, not a loss.

3. **Forging the anchor suffix at close.** The suffix presented at close must hash-chain from the snapshot's `anchor` to the datum's `pendingIncrements`; omitting, reordering, or substituting elements changes the chain head (collision resistance). A snapshot with a fabricated anchor cannot exist without n-of-n collusion, which is outside the trust model (as it is today; a colluding quorum can already sign arbitrary snapshots).

4. **Attacking `exitsPaid`.** It is a counter with a single writer that only ever grows. A stale decrement targets indices below the counter and fails the contiguity check; skipping ahead fails it too. Redirecting an output fails the verbatim entry check (the accumulator element binds index to output). Indices are assigned deterministically in snapshot order, and an entry keeps its index in every subsequent snapshot until observed as paid, so any recent-enough snapshot can prove any unpaid range. Assigning the same index twice would require a fully colluding quorum.

5. **Griefing the queue.** Flooding it with dust exits delays later withdrawals, but every entry costs the attacker a real min-ADA-bearing output that irrevocably leaves their own L2 balance, and batch decrements clear prefixes at the proof batch limit per transaction, chaining across consecutive transactions. Parties may additionally rate-limit exit transactions per snapshot as policy. A single pathological output that no transaction could ever pay is rejected at entry: the per-output check is complete precisely because payment can always degrade to `k = 1`. (This also guarantees, for exit outputs, that fanout can always distribute them; L2-internal outputs whose size makes them undistributable remain a pre-existing limitation unchanged by this design.)

6. **Value conservation (no orphaned, no invented funds).** Invariant: head value = value(active elements of `A_S`) + value(exit entries with index ≥ `exitsPaid`) + value(unabsorbed increments), for the latest signed snapshot. By induction over the four transitions: increments add exactly the logged value; decrements remove exactly the paid prefix; L2 transactions preserve value within `A_S`, and snapshots absorb exactly the logged increments and exactly the observed-paid prefix; close/fanout distributes exactly the three terms. Decrements after close are impossible (the head output leaves the Open state), so unpaid exits at close are always fanned out.

7. **Rollbacks.** Deposited outputs enter `utxo` only after a maturity buffer sized like today's deposit-activation buffer (hours, versus rollback depths of minutes): the same residual assumption the protocol already makes, applied more explicitly than today's `CommitFinalized` path does. If a rollback nonetheless orphans an absorbed increment, the affected snapshot's anchor no longer replays against the (also rolled-back) datum chain, making that snapshot unusable on-chain; parties re-sign from the last consistent snapshot, and in the worst case close with it: safety holds; recent L2 transactions are lost, bounded by the buffer sizing. Queue pruning is driven by observation and only shrinks state, so its rollback handling is strictly simpler.

8. **Stale contests.** Contest requires a strictly higher snapshot number (as today) and the same anchor replay against the chain value frozen at close; an old snapshot cannot displace a newer one, and a diverged-anchor snapshot fails on-chain validation.

9. **Head-output contention and third parties.** Concurrent settlements contend for the head UTxO exactly like any UTxO chain: losers rebuild on the new output, and every validator branch re-checks against the current datum, so races corrupt nothing. A third party who obtains a signed snapshot can post a *correct* decrement or fanout, and only a correct one, which is harmless (they pay the fee).

10. **`maxValSize` on the merged head value.** An oversized deposit's increment simply never lands: benign by construction (expire, then recover), and the certificate acceptance check avoids even attempting it. Two individually fine concurrent certificates whose combined claims exceed the cap degrade the same way: the second increment cannot land and its deposit recovers. Head value near the cap constrains further deposits until exits shrink it: an operational consideration surfaced at certificate time, not a safety issue.

11. **Domain separation.** Exit entries (`H("exit" <> index <> output)`) and active entries use distinct tags in the accumulator, so a proof for one role cannot be replayed as the other; the index also disambiguates identical outputs queued twice. The two multisigned message types, snapshot tuples `(headId, number, A_S, anchor_S)` and claim certificates `(headId, depositTxId, commitsHash)`, are signed under the same party keys and therefore also carry distinct domain tags, so a signature over one can never verify as the other. Accumulator collision-resistance assumptions are unchanged from the current protocol.

12. **Data availability.** Fanout needs the preimages of unabsorbed `commitsHash`es, which are the deposit datums: on-chain data that every node observed, and any single party suffices to supply it. Same class of assumption as fanout needing the snapshot UTxO today.

Protocol invariants to carry into the specification: (i) snapshots reference only observed L1 state and grow-only L2 state; (ii) `pendingIncrements` is append-only while open and frozen at close; (iii) `exitsPaid` only grows and is written only from L1; (iv) an exit entry, once signed, keeps a stable index in all later snapshots until observed as paid; (v) the value-conservation identity of point 6; (vi) every `ReqSn` pins exactly one settlement-chain position, and all absorption effects (anchor advance, deposited outputs entering `utxo`, pruning of queue entries below the observed `exitsPaid`) are deterministic functions of that position; absorption is prefix-only, so no increment can be skipped, and a party that has not yet observed the chain up to the pinned position waits instead of signing. Observation skew between parties therefore never diverges state: settlement order is fixed by L1 (every settlement spends the head output), and lag only delays a signature.

## Out of scope / next steps

- Formalization in the Hydra specification: state machines for both pipelines, on-chain rules for certificates, the pending chain and the exit queue, and proofs of the invariants above.
- Snapshot commitment layout: a single accumulator over the full state versus split `A_active`/`Q_exit` commitments (see the on-chain data model section), to be decided with the specification by weighing accumulator capacity and proof batch limits against a second on-chain proof path.
- Fee attribution for settlement transactions (making the requesting client fund increments, decrements, and fanout rather than the posting node): deliberately unchanged here, deferred to follow-up work.
- Adversarial review of this document by the team and researchers before any implementation.
- Implementation phasing (single protocol bump, staged): (a) exit queue replacing decommits, (b) increments by observation, (c) removal of the version machinery and `HeadLogic` simplification.
