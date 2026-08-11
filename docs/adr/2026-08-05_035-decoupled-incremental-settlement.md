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
  { headId           :: HeadId
  , number           :: SnapshotNumber
  , confirmed        :: [tx]
  , utxo             :: UTxOType tx       -- U: spendable on L2; deposits appear
                                          --    here only after their increment
                                          --    is observed
  , exitQueue        :: ExitQueue tx      -- (index, output) entries, ordered;
                                          --    only grows; pruned by observing
                                          --    decrements
  , absorbedDeposits :: Hash              -- position in the claimed-deposits
                                          --    chain that 'utxo' has absorbed
  , accumulator      :: HydraAccumulator  -- A_active: commitment over 'utxo'
                                          --    only; the exit queue is committed
                                          --    separately as a hash chain Q_exit
  }
```

The multisigned message shrinks accordingly:

```
today    : (headId, version, number, A_S, H(utxoToDecommit), H(utxoToCommit))
this ADR : (headId, number, A_active, Q_exit, absorbedDeposits)
```

And the open head datum trades the version for settlement bookkeeping with single writers (details below):

```
today    : ..., version
this ADR : ..., claimedDeposits, exitsPaid
```

Supporting principles:

- **Funds appear on L2 only by observation**: after their settlement transaction is seen on-chain plus a maturity buffer (the existing deposit-activation buffer mechanism, reused).
- **L2 → L1 authorizations are per-deposit certificates or queue entries**, never promises baked into a snapshot. Replay protection comes from the UTxO model or from a counter with a single writer, never from cross-layer version comparison.
- **On-chain binding uses hashes** (the accumulator commitment over active outputs, hash chains for the claimed deposits and the exit queue). The only counters kept are written by exactly one side, and single-writer counters cannot race.
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
2. **Parties sign a claim certificate.** Each party runs acceptance checks (minimum ADA, a dry-run increment against L1 limits and the *current* head value, token policy limits) and multisigns `(headId, depositOutRef, commitsHash)` in a lightweight network round, similar to `AckSn` but bound to no snapshot number and no version. `depositOutRef` is the full output reference of the deposit, transaction id *and* index, not merely the transaction id: a certificate authorizes exactly one deposit output, so two outputs of a single transaction that happen to share a `commitsHash` cannot both be claimed with one certificate. Deposits are identified by this output reference everywhere, on-chain and in node state. If any party declines, the deposit expires and the depositor recovers; that is the same n-of-n liveness assumption the protocol already has. Honest parties stop signing certificates one deposit period before the deadline (today's `Expired` buffer, reused). They also stop signing new certificates while more than N of their *outstanding* certificates remain, counting every certificate they have signed that is neither absorbed by the newest snapshot they have signed nor expired. The bound is on certificates issued, not on increments already landed: a party could otherwise sign an unbounded burst while none have landed, then see them all land at once. Because certificates are n-of-n, the chain cannot advance past a position a party has not certified, and because each certificate authorizes exactly one append (step 3), any single party unilaterally caps how far the datum chain can run ahead of its own newest snapshot, and therefore the length of the unabsorbed suffix fanout must later replay (see close below).
3. **Anyone posts the increment.** It spends the head output and one or more deposit outputs, presenting one certificate per deposit output. The validator requires that the deposit-script inputs are *exactly* the certified set (no unnamed deposit input rides along), that the head value grows by exactly the sum of those deposits, and it appends one entry to the datum's hash chain per deposit, in canonical input order: `claimedDeposits' = H("claim" <> claimedDeposits <> commitsHash)` per deposit, where `H` is blake2b-256, `<>` is byte concatenation, and `"claim"` is a domain tag keeping this chain's values disjoint from every other hashed structure in the protocol. The chain is seeded with the head's identity: the open datum starts with `claimedDeposits = H("claim" <> headId)`, so every chain value is head-specific from the first append. No snapshot signature, no version. The exact-input-set rule is what keeps value conservation tight: every lovelace entering the head is recorded by exactly one chain append, so no deposit value can slip in untracked and later become unattributable at fanout, and one certificate can never authorize more than one append. Increments commute: any number can land in any order, chaining on the head output.
4. **Observation makes it real on L2.** Once the increment is observed and the maturity buffer has passed, the deposited outputs appear in `utxo` of the next snapshot, and that snapshot's `absorbedDeposits` advances to the chain value it now covers.

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
3. **Anyone posts a decrement paying the queue in order.** It pays entries `[exitsPaid, exitsPaid + k)`, presents *any* confirmed snapshot's signature plus the entries themselves, verified against that snapshot's queue commitment by hash replay (see the on-chain data model), and bumps the datum counter `exitsPaid += k`. The validator checks the outputs verbatim against the proven entries and that the head value shrinks by exactly their sum. Decrements need neither the deposit chain nor a version: an older snapshot can pay older entries, it just cannot present entries it never committed to.
4. **Observation prunes the queue.** Seeing a decrement on-chain removes paid entries from the local L2 queue view, but the front committed by a *signed* snapshot advances only once the decrement is buffer-deep, the same maturity buffer increments use. Pruning the local view early is fine; committing that advance into a signature is what waits. Advancing the signed front on a not-yet-final decrement would be unsafe: if that decrement rolled back, a snapshot signed at the higher front would commit a `Q_exit` that no longer opens the entries between the rolled-back `exitsPaid` and that front, stranding them (see rollbacks below).

Double-pay is structurally impossible: `exitsPaid` is written only by L1 decrement transactions, so a stale signed decrement re-landing targets already-paid indices and fails the contiguity check. No deadline, no off-chain discipline required. And if no decrement ever lands, entries sit in the queue (unspendable on L2) until fanout distributes them. Funds are always safe; the head is never blocked.

### First-class batching

- **Multi-deposit increments**: certificates are per-deposit and increments commute, so one increment can spend the head output plus N deposit outputs (one certificate each). Batching is an optimization with a guaranteed one-at-a-time fallback.
- **Multi-exit decrements**: the default shape. One decrement settles a contiguous queue prefix, and because prefix payment verifies by hash replay, the batch is bounded by transaction size rather than by any proof batch limit: on the order of a hundred ada-only entries per transaction with today's parameters, down to `k = 1`. Longer prefixes chain across consecutive decrements.
- **Settlement chaining**: settlements chain directly on the head UTxO, so several increments and decrements can land in consecutive blocks, or within one block via transaction chaining. Today each one costs a snapshot round-trip and a version bump, one at a time.

### Certificate traffic gets its own channel

The certificate round and the snapshot stream have opposite delivery requirements. Snapshot messages are ordered and liveness-critical: L2 throughput stalls if they stall. Certificates need no ordering (they are per-deposit and commute), tolerate loss and delay (signing is idempotent, re-broadcast is free, and the worst case is an expired deposit that recovers), and are a few hundred bytes regardless of L2 traffic.

Running certificates on their own logical channel, separate from the ordered snapshot stream, keeps the two from head-of-line blocking each other under load: deposit settlement latency scales with deposit rate rather than with L2 transaction volume, and a burst of deposits cannot delay snapshot signing. Whether that channel shares the existing transport with its own queue or becomes a separate best-effort mechanism is an implementation decision; the protocol only requires eventual delivery before the certificate deadline.

### On-chain data model

The open head datum needs no version and no live accumulator. It carries, besides the immutable parameters:

| Field | Writer | Purpose |
| --- | --- | --- |
| `claimedDeposits : Hash` | increment txs (append) | domain-tagged hash chain over the claimed deposits' `commitsHash`es, seeded with `H("claim" <> headId)` |
| `exitsPaid : Int` | decrement txs | count of paid exit-queue entries, only ever grows |

Both bookkeeping structures are hash chains, so how those work is worth stating once. A hash chain commits to an ordered list through a single fixed-size value: each append folds the new element into the current value, and only the result is stored.

```
c0 = H("claim" <> headId)      -- head opens
c1 = H("claim" <> c0 <> h1)    -- first increment logs deposit hash h1
c2 = H("claim" <> c1 <> h2)    -- second logs h2; the datum stores just c2
```

The value contains nothing and reveals nothing; it verifies. Between two chain positions exactly one list of elements can connect the fold, unless someone breaks blake2b-256 collision resistance: omitting, reordering, substituting, or injecting an element diverges the result permanently. This is what makes the storage constant-size, like a blockchain tip: the datum stays 32 bytes whether the head has claimed one deposit or ten thousand, and the cost surfaces only where a gap must be proven, at one cheap hash per bridged element (fanout bridges from `absorbedDeposits` to `claimedDeposits`, both frozen into the closed datum at close; a decrement opens `Q_exit` down to the entries it pays). Chain values are publicly computable by anyone, and that grants no authority: every chain comparison in this design has both endpoints independently authenticated, one a datum field written only by validated transactions, the other a field inside an n-of-n signed artifact, and the elements accepted between them are constrained by the validator, not by knowledge of hash values. The only assumption is collision resistance of `H`, which the protocol already makes everywhere else.

Snapshots multisign `(headId, number, A_active, Q_exit, absorbedDeposits)`, committing to their full state through two structures with structurally separate roles:

- **`A_active`**: the accumulator over the active UTxO set, used exactly as the accumulator is used today (close, contest, and fanout membership proofs against the published reference string).
- **`Q_exit`**: a hash-chain commitment to the ordered exit queue, linking each entry to the commitment of the rest of the queue after it: `Q = H("queue" <> leaf <> rest)` over entry leaves `leaf = H("exit" <> index <> output)`, with the empty tail seeded as `H("queue" <> headId)`. The signed value thus opens the queue at its unpaid front (recomputed off-chain per snapshot as the queue grows). A decrement paying the prefix `[exitsPaid, exitsPaid + k)` presents those entries plus the chain value of the remainder; the validator recomputes k hashes and compares against the signed `Q_exit`. A stale snapshot additionally replays the already-paid entries between its own front and `exitsPaid`, one hash each, without paying them. No pairing check and no reference string are involved.

A single accumulator `A_S` over active outputs plus domain-tagged queue entries was considered instead: it reuses the existing membership-proof path unchanged and keeps one commitment structure in the specification. Measured on-chain costs rule it out as the default. With today's cost model, the deployed reference string, and mainnet execution prices (five parties, ada-only outputs; measured with the project's transaction-cost benchmark, whose fanout path exercises the same pairing check a single-accumulator decrement would run):

| Per decrement | single accumulator `A_S` | split `A_active` + `Q_exit` |
| --- | --- | --- |
| Fixed execution budget | ~38% of the transaction limit (23% decoding the reference-string datum, 8% the pairing) | none beyond the base transaction |
| Marginal cost per exit paid | ~2.6% of budget, ~0.045 ADA | ~0.06% of budget, ~0.005 ADA |
| Batch cap per transaction | ~19 entries (execution budget; the reference string would allow 29) | transaction size, on the order of 100 entries |
| Queue drain per block (ceiling) | ~40 entries | ~1200 entries |

The gap is not an implementation artifact. Even with optimized reference-string handling, the pairing floor remains (one pairing plus one G2 scalar multiplication per entry) and stays more than an order of magnitude above a hash replay. The same asymmetry applies at fanout, where unpaid queue entries are distributed by hash replay instead of consuming membership-proof chunk capacity. Queue entries also stop consuming trusted-setup capacity, leaving the accumulator's element budget entirely to the active UTxO set.

The split layout is therefore the preferred design; its price is one more 32-byte field in the signed tuple and the closed datum, a small hash-replay branch in the validator, and a second commitment structure to specify and audit. Both layouts share the same single signing round, pipelines, datum fields, and reconciliation, so the single-accumulator variant remains a fallback if specification work surfaces a problem with the second proof path; it would then need domain-tagged elements to keep the exit and active roles apart, and it caps decrement batches at the reference string's membership-proof limit.

### Close, contest, and fanout

Today close must pick `CloseUnused` or `CloseUsed` by comparing the snapshot's version against the datum's, to resolve whether a promised settlement landed. Here there are no promises to resolve, and the reconciliation is deferred to the one settlement-end transaction that is already chunked:

- **Close** presents a signed snapshot and records, into the closed datum, that snapshot's `A_active`, `Q_exit`, `absorbedDeposits`, and number, together with the datum's current `claimedDeposits` and `exitsPaid`. It presents no suffix and replays no chain: close is constant-size no matter how many deposits are unabsorbed. Both endpoints of the eventual replay are thereby frozen and independently authenticated, `absorbedDeposits` inside the n-of-n snapshot and `claimedDeposits` in the datum written only by validated increments; the replay that connects them is postponed to fanout. This is deliberate. A single close transaction that had to fold the whole unabsorbed suffix would be bounded by transaction size, a few hundred hashes; the suffix grows with deposit throughput times the maturity buffer, and older snapshots carry longer suffixes than newer ones, so a busy or deliberately flooded head could reach a suffix that no close transaction could hold, wedging the head permanently. Constant-size close removes that failure mode entirely.
- **Contest** requires a strictly higher snapshot number and swaps in that snapshot's `A_active`, `Q_exit`, `absorbedDeposits`, and number. It is likewise constant-size and replays nothing. The frozen `claimedDeposits` never changes (the chain cannot grow after close, since increments need an Open head output), and a higher-numbered snapshot carries a higher-or-equal `absorbedDeposits` (absorption is prefix-only), so contest can only shorten the unabsorbed suffix, never lengthen it.
- **Fanout** is the only settlement-end transaction whose size scales with state, which is why it is already chunked (partial fanout). It distributes three things: the active elements of `A_active`; the exit-queue entries with index ≥ `exitsPaid`, verified against `Q_exit` by hash replay (the counter overrides the snapshot's possibly stale queue view); and the unabsorbed deposits, obtained by replaying the claimed-deposits chain from the closed datum's `absorbedDeposits` to its `claimedDeposits` (one hash per element) and paying each deposit's committed outputs verbatim (the preimages are the deposit datums every node observed on-chain). All three are chunked by the same partial-fanout mechanism, so an arbitrarily long unabsorbed suffix costs more fanout chunks but can never block close. The certificate policy of pipeline 1 still bounds that suffix in the honest case; with this layout it caps fanout cost and recovery time rather than close feasibility.
- Removed entirely: the `version` field, the `CloseUnused`/`CloseUsed` redeemer variants, `utxoToCommit`/`utxoToDecommit` in snapshots, `IncrementRedeemer.decommitOutputsHash`, `DecrementRedeemer.commitOutputsHash`, and the commit/decommit mutual-exclusion requirement.

### What carries over

- Deposit transaction, validator, deadline windows, and the recover flow/API: unchanged.
- The accumulator machinery and its published reference string carry over unchanged, binding the active UTxO set (close, contest, fanout); the exit queue adds only hash replays on top.
- Partial-fanout chunking is reused for distributing large states at fanout, now including the unabsorbed-deposit suffix replay and the exit-queue prefix; close and contest themselves are constant-size. Batched exit payment reuses the same prefix-chunking approach with transaction size as its only bound.
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
  | Uncloseable head from a large in-flight backlog | close and contest are constant-size; the only state-proportional replay lives in chunked fanout |

- Settlement liveness becomes independent of L2 liveness: even if L2 consensus halts, signed exits remain payable by anyone, unclaimed deposits recover after their deadline, and close plus fanout distributes everything, including the queue and unabsorbed deposits.
- Close and contest are constant-size. Every settlement-end transaction whose size scales with state (the unabsorbed-deposit suffix replay and the exit-queue prefix) is confined to fanout, which is chunked, so no volume of in-flight deposits or queued exits can make a head uncloseable. This is what lets deposit throughput rise without the reconciliation cost feeding back into close.
- Throughput: deposits and withdrawals batch and chain without snapshot round-trips in between.
- The node never auto-posts recovery transactions; recovering an expired deposit remains a depositor action through the existing API.

Negative:

- **Exits are final.** No cancelling an exit request (today's decommit is de-facto uncancellable once signed; this design just says so honestly).
- **Exit payment is FIFO.** An exit waits behind earlier queue entries. Every entry is L1-valid by construction and anyone can batch-pay the whole prefix, so the queue can be delayed but never stalled.
- **This is a protocol version bump**: all head validator branches change, script hashes change, the specification needs a rewrite of the incremental settlement sections, and the changes require security review. This is the cost of replacing the mechanism rather than continuing to patch it.

Neutral:

- Deposit-to-L2 latency is comparable to today (deposit → certificate → increment → observation + maturity → next snapshot), with far less machinery.
- API reshape (sketch, to be detailed at implementation time): `POST /decommit` becomes submitting an L2 exit transaction; deposit lifecycle server outputs change (certificate round instead of commit-approval via snapshot); `Snapshot` drops `version`/`utxoToCommit`/`utxoToDecommit` and gains `absorbedDeposits` and the exit-queue view.

## Security considerations

Threat-by-threat results of an adversarial pass over the design. Trust model unchanged: safety against any dishonest minority (any single honest party can contest), liveness requires all n parties. Under a malicious party, every attack below degrades to *delay*, never loss of funds.

1. **Forging or replaying a claim certificate.** Replay is impossible: the increment spends the deposit UTxO, which is one-shot by the UTxO model. Forgery requires breaking the n-of-n multisig. A certificate from another head fails the `headId` binding; swapping the claimed content fails `commitsHash` (the deposit datum is immutable once on-chain). The certificate binds the full `depositOutRef`, transaction id and index, not merely the transaction id, so it authorizes exactly one deposit output: two outputs of one transaction carrying an identical `commitsHash` need two separate certificates, and the increment validator's exact-input-set rule (pipeline 1, step 3) rejects any deposit input it has no matching certificate for. This is what stops one signing round from authorizing many appends. It matters because the certificate throttle bounds the unabsorbed suffix by counting certificates: if a single certificate could back many identical deposit outputs, the true suffix could outrun the throttle by that multiple, and while close is now constant-size, an unbounded suffix would still inflate fanout cost and recovery time. Certificates need no expiry: after the deposit deadline the `Claim` validator branch rejects anyway. Residual: nothing beyond the existing multisig assumptions.

2. **Racing the deadline (increment vs. recover).** A malicious party holding a certificate can post the increment right up to the deadline, ignoring the honest convention of stopping one deposit period earlier, while the depositor posts a recover just after it. Exactly one of them spends the deposit output, and L2 simply follows whichever it observes. Increment wins: funds enter the head, which was the depositor's original intent (they can leave via the queue). Recover wins: no increment can happen thereafter. Today this race is dangerous because L2 has *pre-signed* a snapshot referencing the deposit; here both outcomes are consistent by construction. Residual: a recovery can be front-run into a successful claim, an inconvenience, not a loss.

3. **Forging `unabsorbedDeposits`.** The list is presented at fanout, not at close, and must hash-chain from the closed datum's frozen `absorbedDeposits` to its frozen `claimedDeposits`; omitting, reordering, or substituting elements changes the chain head (collision resistance), and fanout can pay only the outputs whose preimages reproduce that exact chain. Both endpoints are frozen at close from independently authenticated sources, `absorbedDeposits` from the n-of-n snapshot and `claimedDeposits` from the increment-written datum, so neither the closer nor the fanout poster can move them. A snapshot with a fabricated `absorbedDeposits` cannot exist without n-of-n collusion, which is outside the trust model (as it is today; a colluding quorum can already sign arbitrary snapshots). Chain values are also head-specific by construction (the chain is seeded with `H("claim" <> headId)`), so a value from one head can never replay meaningfully against another.

4. **Attacking `exitsPaid`.** It is a counter with a single writer that only ever grows. A stale decrement targets indices below the counter and fails the contiguity check; skipping ahead fails it too. Redirecting an output fails the verbatim entry check (the committed queue entry binds index to output). Indices are assigned deterministically in snapshot order, and an entry keeps its index in every subsequent snapshot until observed as paid, so any recent-enough snapshot can prove any unpaid range. Assigning the same index twice would require a fully colluding quorum.

5. **Griefing the queue.** Flooding it with dust exits delays later withdrawals, but every entry costs the attacker a real min-ADA-bearing output that irrevocably leaves their own L2 balance, and batch decrements clear prefixes bounded only by transaction size, chaining across consecutive transactions. Parties may additionally rate-limit exit transactions per snapshot as policy. A single pathological output that no transaction could ever pay is rejected at entry: the per-output check is complete precisely because payment can always degrade to `k = 1`. (This also guarantees, for exit outputs, that fanout can always distribute them; L2-internal outputs whose size makes them undistributable remain a pre-existing limitation unchanged by this design.)

6. **Value conservation (no orphaned, no invented funds).** Invariant: head value = value(active elements of `A_active`) + value(exit entries with index ≥ `exitsPaid`) + value(unabsorbed deposits), for the latest signed snapshot. By induction over the four transitions: increments add exactly the logged value; decrements remove exactly the paid prefix; L2 transactions preserve value across `A_active` and the queue (an exit request only moves outputs between them), and snapshots absorb exactly the claimed deposits and exactly the observed-paid prefix; close/fanout distributes exactly the three terms. Decrements after close are impossible (the head output leaves the Open state), so unpaid exits at close are always fanned out.

7. **Rollbacks.** Both settlement observations gate signed state behind a maturity buffer sized like today's deposit-activation buffer (hours, versus rollback depths of minutes). This is a *new* buffer on the increment and decrement observation paths, not a restatement of an existing one: today's `CommitFinalized`/`DecommitFinalized` paths reflect a settlement into node state at depth one with no buffer. The spec must therefore place the buffer at increment-to-`absorbedDeposits` and at decrement-to-signed-queue-front, and size it above the maximum rollback depth. With the buffer in place, a signed `absorbedDeposits` only ever names a position below reorg depth, so it stays a genuine prefix of the datum chain even across rollbacks and the fanout replay always finds a connecting suffix. If a rollback nonetheless orphans an absorbed increment, the affected snapshot's `absorbedDeposits` no longer prefixes the (also rolled-back) datum chain, making that snapshot unusable on-chain; parties re-sign from the last consistent snapshot, and in the worst case close with it: safety holds; recent L2 transactions are lost, bounded by the buffer sizing. The queue front obeys the same discipline. A node may prune its local view on observation, but a *signed* snapshot advances the committed front only once the paying decrement is buffer-deep, because a snapshot signed at a front ahead of a rolled-back `exitsPaid` would commit a `Q_exit` that cannot open the entries between them, stranding funds that neither a later decrement nor fanout could pay. Buffering the signed front closes that gap; the local view, which only shrinks and is re-derived from the last snapshot on rollback, needs no buffer.

8. **Stale contests.** Contest requires a strictly higher snapshot number (as today) and swaps in that snapshot's commitments; it is constant-size and replays nothing, since the `absorbedDeposits`-to-`claimedDeposits` replay happens once, at fanout, against the endpoints the winning contest froze. An old snapshot cannot displace a newer one, and because a higher number carries a higher-or-equal `absorbedDeposits`, contest can only shorten the suffix fanout must replay, never lengthen it. A snapshot whose `absorbedDeposits` had diverged from the datum chain would fail at fanout rather than corrupt state, but the maturity buffer keeps `absorbedDeposits` below reorg depth, so a validly signed snapshot never diverges in the first place.

9. **Head-output contention and third parties.** Concurrent settlements contend for the head UTxO exactly like any UTxO chain: losers rebuild on the new output, and every validator branch re-checks against the current datum, so races corrupt nothing. A third party who obtains a signed snapshot can post a *correct* decrement or fanout, and only a correct one, which is harmless (they pay the fee).

10. **`maxValSize` on the merged head value.** An oversized deposit's increment simply never lands: benign by construction (expire, then recover), and the certificate acceptance check avoids even attempting it. Two individually fine concurrent certificates whose combined claims exceed the cap degrade the same way: the second increment cannot land and its deposit recovers. Head value near the cap constrains further deposits until exits shrink it: an operational consideration surfaced at certificate time, not a safety issue.

11. **Domain separation.** Exit entries (`H("exit" <> index <> output)`) and active outputs live in separate commitment structures (`Q_exit` and `A_active`), so a proof for one role cannot be replayed as the other by construction; the entry format additionally binds each output to its queue position and disambiguates identical outputs queued twice. The same discipline covers every hash construction: claim-chain steps are tagged `"claim"`, queue-chain steps `"queue"`, entry leaves `"exit"`, so no value of one role can ever equal a value of another, by input disjointness rather than by case analysis. The two multisigned message types, snapshot tuples `(headId, number, A_active, Q_exit, absorbedDeposits)` and claim certificates `(headId, depositOutRef, commitsHash)`, are signed under the same party keys and therefore carry distinct domain tags, so a signature over one can never verify as the other. Accumulator collision-resistance assumptions are unchanged from the current protocol.

12. **Data availability.** Fanout needs the preimages of the `unabsorbedDeposits` hashes, which are the deposit datums: on-chain data that every node observed, and any single party suffices to supply it. Same class of assumption as fanout needing the snapshot UTxO today.

Protocol invariants to carry into the specification: (i) snapshots reference only observed L1 state and grow-only L2 state; (ii) `claimedDeposits` is append-only while open and frozen at close; (iii) `exitsPaid` only grows and is written only from L1; (iv) an exit entry, once signed, keeps a stable index in all later snapshots until observed as paid; (v) the value-conservation identity of point 6; (vi) every `ReqSn` pins exactly one settlement-chain position, and all absorption effects (`absorbedDeposits` advance, deposited outputs entering `utxo`, pruning of queue entries below the observed `exitsPaid`) are deterministic functions of that position; absorption is prefix-only, so no increment can be skipped, and a party that has not yet observed the chain up to the pinned position waits instead of signing. Observation skew between parties therefore never diverges state: settlement order is fixed by L1 (every settlement spends the head output), and lag only delays a signature. (vii) each increment appends exactly one chain entry per deposit-script input, and the set of deposit-script inputs equals the certified set, so every unit of head value that enters is recorded by exactly one append and no certificate authorizes more than one append. (viii) a signed snapshot's `absorbedDeposits` and committed queue front name only buffer-deep (rollback-safe) L1 positions; close and contest are constant-size, and the only state-proportional chain and queue replays happen at fanout, which is chunked, so no volume of unabsorbed deposits or queued exits can make a head uncloseable.

## Out of scope / next steps

- Formalization in the Hydra specification: state machines for both pipelines, on-chain rules for certificates, the claimed-deposits chain and the exit queue, and proofs of the invariants above.
- Confirming the split `A_active`/`Q_exit` commitment layout during specification. The on-chain data model section records the cost analysis behind the preference and the single-accumulator fallback.
- Fee attribution for settlement transactions (making the requesting client fund increments, decrements, and fanout rather than the posting node): deliberately unchanged here, deferred to follow-up work.
- Adversarial review of this document by the team and researchers before any implementation.
- This draft already incorporates a first adversarial pass. Three changes came out of it and warrant focused review: constant-size close and contest with the unabsorbed-deposit replay deferred to chunked fanout (so deposit throughput times the maturity buffer can no longer drive a close transaction past the transaction-size limit and wedge the head), certificates and chain appends bound one-to-one to a deposit output reference with an exact-input-set rule on the increment (so one signing round cannot authorize many appends and no deposit value enters untracked), and a maturity buffer on the signed exit-queue front symmetric to the increment buffer (so a rolled-back decrement cannot strand queued exits). See the close section and threats 1, 3, 7, and 8.
- Implementation phasing (single protocol bump, staged): (a) exit queue replacing decommits, (b) increments by observation, (c) removal of the version machinery and `HeadLogic` simplification.
