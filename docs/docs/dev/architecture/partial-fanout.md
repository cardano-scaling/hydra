# Partial fanout

When a head is closed, the plain `Fanout` transaction distributes the whole UTxO
set back to layer 1 in a single transaction. That transaction has to fit under
the layer 1 size limit, so a head with many outputs cannot be fanned out at all.
_Partial fanout_ removes this limit: it distributes the UTxO set in several
transactions, each carrying a chosen _subset_ of the outputs, until the head is
empty.

This page explains how partial fanout works under the hood. For how to drive it
from a client, see the [Selective fanout](../../how-to/selective-fanout) how-to.

## The accumulator

The head does not store the individual UTxOs on-chain while it is open. Instead
it keeps a single _commitment_ to the confirmed snapshot's UTxO set, a
[`HydraAccumulator`](https://hydra.family/head-protocol/haddock/hydra-tx/Hydra-Tx-Accumulator.html).

Each `TxOut` is serialised to its `BuiltinData` bytes and turned into one
_element_ (a scalar). The set of elements `s₁, …, sₙ` defines a polynomial

```
A(X) = (X − s₁)·(X − s₂)·…·(X − sₙ)
```

which is committed as a single `BLS12-381` G1 point `A(τ)·G1`. This point is the
_accumulator commitment_. All parties sign a hash of it (the
`accumulatorHash`), and it is stored in the `ClosedDatum`. So a whole snapshot,
however many outputs it has, is pinned by one 48-byte group element.

## Membership proofs

To fan out a subset `S` of the outputs, the node has to prove that `S` really was
part of the committed set. `S` defines its own polynomial
`P_S(X) = ∏(X − sᵢ)` over just the subset elements. Since every subset element
is a root of `A(X)`, `P_S(X)` divides `A(X)` exactly, leaving a quotient

```
A(X) = Q(X)·P_S(X)
```

The node commits the quotient over G1 as `Q(τ)·G1`, the _membership proof_, using
[`createMembershipProofFromUTxO`](https://hydra.family/head-protocol/haddock/hydra-tx/Hydra-Tx-Accumulator.html#v:createMembershipProofFromUTxO).
On-chain, the [`checkMembershipPairing`](https://hydra.family/head-protocol/haddock/hydra-plutus/Hydra-Contract-CRS.html)
validator verifies the KZG pairing identity

```
e(A(τ)·G1, G2) = e(Q(τ)·G1, P_S(τ)·G2)
```

If this holds, then `A(X) = Q(X)·P_S(X)`, so `S` is genuinely a subset of the
committed UTxO set. The same pairing check is used by both the full fanout and
each partial step.

## The CRS

To evaluate `P_S(τ)·G2` on-chain the validator needs the _powers of tau_ in G2:

```
[G2, τ·G2, τ²·G2, …]
```

This list is the _common reference string_ (CRS). The validator multiplies these
points by the coefficients of `P_S(X)` (a multi-scalar multiplication) to obtain
`P_S(τ)·G2`, then runs the pairing. The matching G1 powers are used off-chain to
build the proof.

The CRS is published on-chain as a reference input, next to the head reference
script, carrying the G2 points as its inline datum. The validator does **not**
just check that the reference input sits at the right address; it binds the exact
CRS datum. Binding only the location would let an attacker supply a different CRS
whose secret they know and forge membership proofs, so the datum itself has to be
the canonical one.

## Trusted setup

The powers of tau are generated from a secret `τ` that must be known to nobody. If
anyone learned `τ` they could forge a membership proof for a subset that was never
in the head and steal funds. The secret is therefore never held in one place: it
comes from a _powers-of-tau ceremony_ where many participants each contribute
randomness, and the setup stays safe as long as at least one participant discarded
their part.

Hydra reuses the [EIP-4844](https://eips.ethereum.org/EIPS/eip-4844) trusted setup
from the Ethereum KZG ceremony, which had a very large number of participants. The
setup bytes are embedded into the binary at compile time and integrity-checked, so
every node uses the same canonical CRS. It provides powers of tau up to a maximum
batch size, which is what bounds how many outputs one partial fanout transaction
can distribute.

## How selection decides what to fan out

Selection is driven by the client. Each `PartialFanout` client input names the
subset of UTxOs to distribute in that step. For that subset the node:

- builds the membership proof and submits the layer 1 transaction, splitting it
  across several transactions if the subset does not fit in one
- reduces the head output value by the value of the distributed outputs
- stores the commitment of the _remaining_ accumulator in the continuing head
  output's `FanoutProgressDatum`

The next step then proves its subset against that remaining commitment, and so on.
The last batch cannot be an ordinary partial step: it must be the _final_ fanout,
which distributes the rest and burns the head tokens. Selection is also sticky:
once a head has been partially fanned out, the plain `Fanout` is refused and the
head must be drained with further `PartialFanout` commands.

## Caveats

### UTxO sizing

Every fanned-out output has to satisfy the layer 1 min-UTxO rule, so the head must
hold enough ada to back each output it will produce. Outputs that carry native
tokens are larger and cost more. The number of outputs in a single step is bounded
by the trusted setup's maximum batch size, which is why large selections are split
across transactions.

### Who pays

The party that issues a step submits and pays for that layer 1 transaction. Only
the node that issued a command advances the fanout, so if it goes offline any other
party can resume by issuing `PartialFanout` for the remaining set.

### Ada overhead

A head output always holds a bit of ada beyond the sum of its layer 2 UTxOs, the
`headAdaOverhead`. This is the min-UTxO overhead of the head output itself. It is
set once at init time and stays invariant for the head's lifetime, propagated
unchanged through `Close` and every fanout step so that value conservation checks
line up.

### Unburned tokens

:::caution

Partial fanout does not fully solve the [unburned-token stuck-head problem](https://github.com/cardano-scaling/hydra/issues/2334).
A partial step never mints or burns tokens, so it still distributes its selected
UTxOs even when the head output carries a foreign token that the head cannot burn.
But the final step must burn all head tokens, and the last batch is forced to be
that final step. A foreign token cannot be burned by the head policy, so the last
batch stays stuck. In practice a participant can recover everything down to the
last batch by selecting their own UTxOs, but the residual head output holding the
unburnable token cannot be fanned out.

:::
