# ADR-035 flows explained: deposit/increment and exit/decrement

Companion notes to `docs/adr/2026-08-05_035-decoupled-incremental-settlement.md`.
Every step says who acts, what data they use, and where that data comes from.

## The vocabulary (read this first)

| Term | What it is | Where it lives |
| --- | --- | --- |
| `commits` | the outputs a depositor wants to move into L2 | deposit datum |
| `h`, the deposit's own hash | `sha2_256` over one deposit's `commits` | derived from the deposit datum, never stored |
| the combined hash | running hash over all deposits claimed so far | head datum field `claimedDeposits` |
| `absorbedDeposits` | a copy of some past combined hash, meaning "my utxo already contains all deposits up to here" | signed snapshot |
| `A_active`, the accumulator | commitment over the spendable L2 outputs (money, not hashes) | signed snapshot |
| exit entry | `(index, output)` waiting to be paid on L1 | snapshot state (the exit queue) |
| `Q_exit` | commitment over the exit queue entries | signed snapshot |
| `exitsPaid`, the paid counter | count of queue entries already paid on L1 | head datum |

The two hashes to never mix up:

```
h            = sha2_256(commits of ONE deposit)          -- deposit's own hash
new combined = H("claim" <> old combined <> h)            -- covers ALL claimed deposits
```

The combined hash starts at `H("claim" <> headId)` when the head opens.
Each increment advances it by exactly one step. The head datum stores only
the newest value, 32 bytes, no list.

The accumulator never contains `h` or any combined hash. It contains outputs.
Hashes answer "which deposits happened"; the accumulator answers "what money
is spendable".

---

## Part 1: a deposit becomes spendable on L2

### Step 0. Deposit tx (on-chain, no validator runs)

A plain wallet transaction creates the deposit output at the deposit validator
address.

- Value: the deposited funds.
- Datum: `(headId, deadline, commits)`.

No script runs when an output is created. The datum written here is the single
source of truth for everything later: `h` is derived from `commits`, the
`deadline` splits the claim window from the recover window, and `headId` names
the only head allowed to claim.

### Step 1. Certificate round (off-chain)

Every node observes the deposit tx. Before signing anything, each node runs
its own acceptance checks:

- minimum ADA on each committed output;
- a dry-run increment against L1 limits and the current head value
  (`maxValSize`, tx size);
- token policy limits;
- timing: refuse if we are within one deposit period of the deadline;
- throttle: refuse while too many of this party's own certificates are still
  outstanding, meaning signed but not yet absorbed into a snapshot or expired
  (this keeps the later fanout work bounded, Part 3).

If the checks pass, the party signs the message `(headId, depositOutRef, h)`,
with a domain tag so this signature can never be confused with a snapshot
signature. `depositOutRef` is the deposit's exact output, its transaction id and
index together, so one certificate authorizes exactly one deposit output and can
never be reused for a different one. Signatures travel on their own network channel, separate from
snapshot messages. Loss or delay is harmless: signing is idempotent and
re-broadcast is free.

Result: the claim certificate, which is simply the list of all n signatures.
If any party never signs, no certificate exists, the deposit expires at its
deadline, and the depositor recovers through the existing API. The head is
untouched.

### Step 2. Increment tx (on-chain)

Anyone holding the certificate posts the increment. It spends two inputs:

- the head output (redeemer: the certificate plus the deposit reference);
- the deposit output (redeemer: `Claim`, no fields).

The head validator checks, in order, with the data source of each check:

| # | Check | Data comes from |
| --- | --- | --- |
| 1 | deposit datum's `headId` equals the head's `headId` | both datums |
| 2 | tx validity upper bound `<=` `deadline` | tx body, deposit datum |
| 3 | certificate verifies: n signatures over `(headId, depositOutRef, h)` | signatures from redeemer; message rebuilt from datums and the spent input; keys are `parties` from the head datum |
| 4 | the spent deposit inputs are exactly the certified ones (no extra deposit rides along), and head value grows by exactly their total | tx body, redeemer |
| 5 | datum out: one append per deposit in input order, `claimedDeposits' == H("claim" <> c <> h)`; all other fields byte-identical | old and new head datum |
| 6 | no mint, no burn; head tokens stay in the head output | tx body |

Check 3 is the trust anchor: the redeemer contributes only signatures. The
message is rebuilt from on-chain data, and `h` is recomputed from the deposit
datum's `commits`. Nothing in the redeemer can lie.

Check 5 does one hash per claimed deposit, usually just one. Not a fold over a
stored list: the combined hash in the datum already carries every earlier
deposit, so the validator just takes the current combined hash `c` from its own
datum, the recomputed `h`, and produces the next combined hash.

The deposit validator's `Claim` branch checks one thing: a head input carrying
its datum's `headId` is spent in the same transaction. Everything else is
delegated to the head validator.

Batching: one increment may spend several deposit outputs, one certificate
each. The validator folds their `h` values into the combined hash in
transaction input order (Cardano orders inputs canonically), so every observer
reproduces the same result.

### Step 3. Observation (off-chain)

Every node sees the increment, because it spends the head output, and a node
always tracks the head output. On observation the node records into
`pendingDeposits`:

- the deposited outputs (from the deposit datum it already knows);
- the new combined hash (read from the new head datum, and cross-checked
  locally: `H("claim" <> previous <> h)` must match, which it always does
  because the validator enforced it);
- the observation time.

Then it waits out the maturity buffer (hours-scale, same sizing as today's
deposit activation buffer, far deeper than realistic rollbacks). Until the
buffer passes, the deposit does not exist for L2.

### Step 4. Absorption snapshot (off-chain)

When matured entries exist in `pendingDeposits` (or L2 transactions are
pending), the leader sends `ReqSn` carrying the target combined hash `X`:
"this snapshot absorbs every deposit up to X".

Each party validates `X` against its own records only. No arithmetic, no fold:

1. Have I observed the head datum hold exactly the value `X` on L1?
   If not yet: wait. If my node is behind, `X` appears as my chain sync
   catches up. A fabricated `X` never appears and the round stalls, which any
   party can already cause (the standing n-of-n liveness assumption); close
   stays available regardless.
2. Is `X` at or after the previous snapshot's `absorbedDeposits` in my
   observed sequence? Going backwards would un-absorb deposits: protocol
   violation, reject.
3. Has every increment between the previous snapshot's value and `X` passed
   the maturity buffer by my own clock? If the leader saw an increment before
   I did, I wait until my own buffer expires. Delay, never divergence.

Then every party deterministically builds the same snapshot:

```haskell
absorb x st =
  let ready = depositsBetween (lastAbsorbed st) x (pendingDeposits st)
   in Snapshot
        { utxo             = utxo st <> foldMap outputs ready  -- money in
        , accumulator      = foldl insertOutput (accumulator st)
                                   (concatMap outputs ready)   -- outputs, not hashes
        , absorbedDeposits = x                                 -- the combined hash, copied
        , ...
        }
```

Each party signs only what it built itself. After confirmation the deposited
outputs are spendable on L2.

### Failure paths (all of them end the same way)

- Certificate round never completes: deposit expires, depositor recovers.
- Increment never lands (oversized, fees, anything): same.
- Head closes before the increment lands: the claim branch needs an Open head
  output, so the increment becomes impossible: same.

In every case L2 never referenced the deposit, so there is nothing to undo.

---

## Part 2: an exit becomes paid on L1

### Step 1. Exit request (off-chain, an ordinary L2 transaction)

The owner submits an L2 transaction that moves their outputs into the exit
queue. Before accepting it, each node checks every exit output individually:

- minimum ADA;
- output fits `maxValSize`;
- "a decrement paying just this one output fits L1 limits". Because a
  decrement can pay any batch size down to one, this per-output check is
  complete: if the output passes, some decrement can always pay it.

When the snapshot confirms, the entries receive consecutive indices in
confirmation order, leave `utxo` (and the accumulator), and join the queue.
Exits are final; there is no cancel.

### Step 2. The queue commitment `Q_exit` (off-chain, recomputed per snapshot)

Each entry hashes to a leaf that binds output to position:

```
leaf_i = H("exit" <> i <> output_i)
```

The queue commitment chains each leaf to the commitment of everything after
it, with the empty tail seeded by the head's identity:

```
queue [e5, e6, e7], front = 5:

Q_exit = H("queue" <> leaf_5 <>
           H("queue" <> leaf_6 <>
             H("queue" <> leaf_7 <>
               H("queue" <> headId))))
```

The snapshot signs this queue commitment, describing the queue from its first
unpaid entry onward. Note this chaining IS over data that contains outputs: that
is the exit queue's, not the deposit chain's. The deposit chain only ever chains
`h` values.

### Step 3. Decrement tx (on-chain)

Anyone can post a decrement. It spends only the head output and pays out the
next stretch of the exit queue, in order, on L1.

"The next stretch" is not a free choice. A decrement must start paying exactly
where the paid counter (`exitsPaid` in the head datum) points right now. It pays
some entries from there, in order, to their owners exactly as the queue recorded
them, shrinks the head value by their total, and moves the paid counter forward
by however many it paid. How many it pays at once is limited only by transaction
size, roughly a hundred simple entries; a longer stretch is just several
decrements one after another.

What the poster shows the validator:

- one confirmed snapshot's signature. Any confirmed snapshot works, old or new,
  because its signature vouches for the queue through the queue commitment
  (`Q_exit`) it signed;
- the actual entries it is paying, so the validator can check them against that
  commitment.

What the validator checks:

| # | Check | In plain terms |
| --- | --- | --- |
| 1 | the entries are the real ones | recompute the queue commitment from the presented entries; it must equal the one inside the snapshot's signature, so nothing can be swapped, reordered, or invented |
| 2 | payment starts at the counter | the first entry paid is exactly the one the paid counter points at: not earlier (already paid), not later (that would skip someone) |
| 3 | outputs paid verbatim | each payout goes to the exact owner and amount the queue recorded |
| 4 | value is conserved | the head shrinks by exactly the total paid; nothing minted or burned |
| 5 | only the counter moves | the paid counter goes up by the number paid; the deposit side (the combined hash) is left untouched, so the two pipelines never interfere |

Check 2 is the whole reason double-paying is impossible. The paid counter only
moves forward, and only a decrement moves it. An old decrement re-submitted later
tries to pay entries the counter has already passed, so it fails check 2 and does
nothing. No deadlines, no off-chain bookkeeping.

One wrinkle, and only when the poster uses an OLD snapshot. A snapshot's queue
commitment describes the queue as it looked when that snapshot was signed,
starting from whatever was unpaid back then. If entries have been paid since, by
a more recent decrement, the poster also has to list those already-paid entries.
Not to pay them again, but so the validator can rebuild the exact commitment that
old snapshot signed and confirm the match in check 1. Paying still starts at the
current counter; the extra listed entries only make the old commitment line up.
Use a fresh snapshot instead and there is nothing extra to list, so the wrinkle
disappears.

### Step 4. Observation prunes (off-chain)

Every node sees the decrement, because it spends the head output, and removes
the paid entries from its own view of the queue right away, so it stops offering
them on L2.

Committing that removal into a *signed* snapshot waits, though. A node will not
sign a snapshot that treats those entries as gone until the decrement is old
enough to be safe from a chain rollback: the same waiting period deposits use,
which the operator sets. The reason mirrors the deposit side. If a snapshot had
already committed to the queue as if the entries were paid, and then the
decrement were rolled back, those entries could be stranded, payable by no one,
because the snapshot's commitment would no longer open them. Waiting until the
decrement has settled removes that risk. Pruning your own view early is fine;
baking it into a signature is what waits.

Once the decrement is that old, the removal is pinned by the `ReqSn` exactly
like deposit absorption, a deterministic function of what landed on L1, so every
party computes the same snapshot.

### Failure paths

- No decrement ever lands: entries simply wait in the queue, unspendable on
  L2, until fanout pays them after close. The head is never blocked.
- Stale decrement re-lands: killed by the contiguity check.

---

## Part 3: where folds actually happen (and where they do not)

This is the table to keep. "Fold" means walking a list and hashing step by
step. Most of the protocol does NOT fold.

| Moment | Who | Folds over | Why |
| --- | --- | --- | --- |
| increment validator | on-chain | nothing: ONE hash step per deposit | append one deposit to the combined hash |
| ReqSn validation | each party, off-chain | nothing: lookup in own observed history | the combined hash was already enforced on-chain when it was created |
| absorption | each party, off-chain | nothing: copies the combined hash, inserts outputs into utxo/accumulator | the outputs are known from observation |
| decrement validator | on-chain | the queue entries being paid, plus any already-paid ones when the snapshot is old | recompute the queue commitment and match it against the signed `Q_exit` |
| close / contest validator | on-chain | nothing: both are constant-size | they only record the two endpoints (the snapshot's `absorbedDeposits` and the datum's `claimedDeposits`); the bridge between them is deferred to fanout |
| fanout validator | on-chain | queue entries (same as decrement); and, for deposits, the hashes `h`, walking `absorbedDeposits` up to `claimedDeposits` | pay the unpaid queue, then bridge and rebuild the unabsorbed deposits verbatim |

So, to the specific confusion:

- For deposits, nobody ever folds over outputs. The only deposit fold is at
  fanout, and it folds over the deposits' own hashes `h`:

  ```haskell
  foldl (\acc h -> hash ("claim" <> acc <> h))
        closedDatum.absorbedDeposits  -- start: frozen at close
        theHashesInBetween            -- the h values, supplied by the fanout poster
    == closedDatum.claimedDeposits    -- target: frozen at close
  ```

  The fanout poster gets the `h` values from its own observation of the
  increments. Both endpoints were frozen into the closed datum at close from
  authenticated sources (the signed snapshot on one side, the increment-written
  head datum on the other), so only the true list connects them.

- Along that same fanout walk, each deposit's outputs are rebuilt: the poster
  supplies each unabsorbed deposit's `commits`, the validator hashes each list
  once and compares it with the `h` the walk just folded in, then requires those
  outputs verbatim in the transaction. One comparison per deposit.

- The only fold that ever touches outputs is the exit-queue fold
  (decrement and fanout), because there the output is part of what the leaf
  commits to: `leaf_i = H("exit" <> i <> output_i)`.

## Part 4: one worked close, end to end

The head claimed three deposits, so its combined hash on L1 went through:

```
c0 = H("claim" <> headId)
c1 = H("claim" <> c0 <> h1)
c2 = H("claim" <> c1 <> h2)
c3 = H("claim" <> c2 <> h3)      <- head datum now holds c3
```

The newest signed snapshot absorbed only the first deposit, so its
`absorbedDeposits` is `c1`.

Close does no arithmetic. The closer just posts its snapshot, and the closed
datum freezes two values: `absorbedDeposits = c1` from the snapshot and
`claimedDeposits = c3` from the head datum (along with the snapshot's `A_active`,
`Q_exit`, and the paid counter). That is the whole close, whatever the gap
between the two values. Keeping close this small is what lets a head always be
closed, no matter how many deposits piled up unabsorbed. A later contest can only
swap in a higher-numbered snapshot, which moves `absorbedDeposits` closer to
`c3`, never further; `c3` itself stays frozen.

Fanout does the reconciling, and it can be split across several transactions if
the state is large. To pay the deposits the snapshot never absorbed, the poster
walks the combined hash from `c1` up to `c3`, supplying `h2` and `h3`:

```
H("claim" <> H("claim" <> c1 <> h2) <> h3) == c3   -- must land on the frozen c3
```

Omit `h2`, swap the order, or substitute anything and the walk lands elsewhere,
so fanout fails. Along the same walk the validator checks each deposit's
`commits` against `h2` and `h3` and pays those outputs verbatim.

Fanout pays three pots, which by construction sum to the head value:

1. the active outputs, proven against `A_active`;
2. the queue entries still unpaid (index at or past the paid counter), proven
   against `Q_exit`;
3. the outputs of deposits 2 and 3, from the walk above.

Nothing is orphaned, nothing is paid twice, and no step ever needed a version
counter.
