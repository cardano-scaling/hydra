# Agda on-chain spec vs Plutus implementation: alignment report

This report compares the **Agda on-chain model** (`spec/src/Hydra/Protocol/OnChain.lagda.typ`:
`HeadDatum`, `HeadRedeemer`, the transition relation `_⟶⟨_⟩_`, and the per-transaction
validity bundles) against the **Plutus `νHead` validator** that actually runs on-chain
(`hydra-plutus/src/Hydra/Contract/Head.hs`, with datum/redeemer types in
`HeadState.hs`).

Method: the datum/redeemer types and the validator dispatch were compared directly; each
per-transaction validity bundle was compared check-by-check against its Plutus validator
function (three independent expert passes), then cross-checked. The Agda is deliberately
concise; the Plutus carries extra "noise" (datum decoding, error codes, CRS plumbing). The
comparison judges agreement on the **substantive checks**.

Counterpart for the **off-chain** Agda (`OffChain.lagda.typ` vs `HeadLogic.hs`) is tracked
separately in [`code-spec-discrepancies.md`](./code-spec-discrepancies.md).

## Verdict

**The on-chain Agda model is faithfully aligned with the Plutus validator.** The redeemer
type and the state-transition dispatch match **exactly** (constructor-for-constructor and
transition-for-transition). Every substantive validator check has a faithful counterpart in
both directions. Two findings are worth acting on (one Agda-side modelling note, one Plutus
code gap), plus several benign abstraction boundaries; none is a fund-loss/double-spend hole.

---

## 1. Datum and redeemer types

### Redeemer: `HeadRedeemer` (Agda) ↔ `Input` (Plutus) — EXACT match

Constructor names, order, **constructor indices**, and field tuples all coincide:
`Increment(ξ,s,ref)`=0, `Decrement(ξ,s,m)`=1, `Close ct`=2, `Contest ct`=3,
`Fanout(m,π,crs)`=4, `PartialFanout(m,crs)`=5, `FinalPartialFanout(m,π,crs)`=6. The nested
`CloseType` ↔ `CloseRedeemer` (`closeInitial/closeAny/closeUnused/closeUsed`) and
`ContestType` ↔ `ContestRedeemer` (`contestUnused/contestUsed`) match exactly, including the
`(ξ, ηhash)` payloads.

### Datum: `HeadDatum` (Agda) ↔ `State` (Plutus)

Both range over `Open | Closed | Final | FanoutProgress` with **no `Initial` state** (init
produces `Open` directly). Field-level alignment:

| Agda field | Plutus field | Note |
|---|---|---|
| `cid : ℍ` | `headId : CurrencySymbol` | match |
| `hydraKey : VKey` + `n : ℕ` | `parties : [Party]` | Agda abstracts the multisig as one **aggregate** key plus an explicit count `n`; Plutus keeps the **party list** (it verifies the multisig party-by-party). `n = length parties`. Equivalent. |
| `contestationPeriod : ℕ` | `contestationPeriod : ContestationPeriod` | match |
| `version : ℕ` | `version : SnapshotVersion` | match |
| `Open.η : AccCommitment` | `OpenDatum.accumulatorHash : Hash` | **See finding A.** Agda's `Open` carries the accumulator **commitment**; Plutus's `OpenDatum` carries only its **hash**. |
| `Closed.η`/`FanoutProgress.η : AccCommitment` | `accumulatorCommitment : BLS12_381_G1` | match (the real commitment, both sides) |
| `contesters : List VKey` | `contesters : [PubKeyHash]` | Agda uses verification keys; Plutus uses key **hashes**. Cosmetic. |
| `tfinal : ℕ` | `contestationDeadline : POSIXTime` | match |
| `ada : Value` | `headAdaOverhead : Integer` | match (min-UTxO lovelace overhead) |
| (none) | `OpenDatum.headSeed : TxOutRef` | **See finding D.** Present in Plutus, absent in Agda; Plutus comment marks it `TODO: Spec?`. |

## 2. State-transition dispatch — EXACT match

`headValidator` (`Head.hs:90-109`) routes exactly the eight admissible `(state, redeemer)`
pairs of the Agda `_⟶⟨_⟩_` relation, and rejects everything else
(`InvalidHeadStateTransition`), mirroring the relation being uninhabited off-shape:

| Agda rule | Plutus arm |
|---|---|
| `increment` (Open→Open, `suc v`) | `(Open, Increment)` → `checkIncrement` |
| `decrement` (Open→Open, `suc v`) | `(Open, Decrement)` → `checkDecrement` |
| `close` (Open→Closed) | `(Open, Close)` → `checkClose` |
| `contest` (Closed→Closed) | `(Closed, Contest)` → `checkContest` |
| `fanout` (Closed→Final) | `(Closed, Fanout)` → `headIsFinalizedWith` |
| `partialFanoutStart` (Closed→FanoutProgress) | `(Closed, PartialFanout)` → `checkPartialFanout ∘ progressFromClosed` |
| `partialFanoutStep` (FanoutProgress→FanoutProgress) | `(FanoutProgress, PartialFanout)` → `checkPartialFanout` |
| `finalPartialFanout` (FanoutProgress→Final) | `(FanoutProgress, FinalPartialFanout)` → `checkFinalPartialFanout` |

## 3. Per-transaction check alignment (summary)

All four flagged correctness invariants hold in both:

- **Version discipline.** `CloseUsed`/`ContestUsed` verify the multisig against `v−1`;
  `CloseUnused`/`CloseAny`/`ContestUnused` against `v`. Increment/decrement bump the version
  by exactly `+1` and sign over the **old** version `v` (Agda `snapshotSigOK … v …` ↔ Plutus
  `verifySnapshotSignature … prevVersion …`). Match.
- **Deadlines.** Close: `tfinal ≡ validity.hi + cp` (`makeContestationDeadline`). Contest:
  `tfinal' ≡ if |C'| = n then tfinal else tfinal + cp` (`mustPushDeadline`). Fanout family:
  posted after the deadline, `tfinal < validity.lo` ↔ `afterContestationDeadline`. Match.
- **Accumulator/hash binding.** `η# ≡ hash(stored η')` via `mustMatchAccumulatorCommitmentHash`
  (close/contest); membership `accVerify` via `checkMembershipPairing`; exclusion
  `accVerifyExclude` (see finding C). Match.
- **Value conservation.** Increment **adds** the deposit (`headIn + deposit = headOut`);
  decrement **removes** the decommit (`headIn = headOut + decommit`); fanout `headIn =
  Σ outputs ⊕ burned ⊕ adaOverhead`. Directions and the `adaOverhead` term match.

Plus, in both: `noMint` on close/contest/increment/decrement/partial-fanout; participant
signature; parameter invariance (`cid/hk/n/cp`, via the Agda rule's shared binders ↔ Plutus
`mustNotChangeParameters`); ada-overhead invariance (Agda `ada` reuse ↔
`mustPreserveHeadAdaOverhead`); contest's strict snapshot increase and append-one-contester;
`CloseAny ⇒ s' > 0`; partial-fanout's "not last batch" `η' ≠ G₁` and no-burn; fanout/
final-partial-fanout burning exactly `n+1` tokens (`mustBurnAllHeadTokens` = `length parties
+ 1`). All match.

---

## 4. Findings

### Finding A — MED (Agda modelling note): `Open.η` is a commitment in Agda, a hash in Plutus

Agda's `Open` datum stores `η : AccCommitment` (the full accumulator commitment, e.g.
`accUTxO(∅)` at init). The Plutus `OpenDatum` stores only `accumulatorHash : Hash` — the BLS
commitment is materialised only later, in `ClosedDatum`/`FanoutProgressDatum`
(`accumulatorCommitment : G1`), where fanout membership proofs need it. For the checks that
touch the open state (the close/increment/decrement signature over `cid‖v‖s‖η#`), the two
**coincide**: Agda computes `hash (ηOf d')` exactly where Plutus reads the stored
`accumulatorHash`. So this is a faithful abstraction, not a bug, but the spec's `Open` datum
is structurally **richer** than the implementation's. Worth a one-line note in the spec
(`Open` carries the commitment conceptually; the implementation stores its hash and only
materialises the commitment at close).

### Finding B — MED (Plutus code gap): full `Fanout` does not enforce `m > 0`

The Agda `fanoutValid` requires `0 < m` (as do `partialFanoutValid` and
`finalPartialFanoutValid`). In Plutus, `checkPartialFanout` (`Head.hs:581`) and
`checkFinalPartialFanout` (`Head.hs:671`) both guard `numberOfPartialOutputs > 0`, but
`headIsFinalizedWith` (the full `Fanout`, `Head.hs:494-498`) has **no** `numberOfFanoutOutputs
> 0` guard. With `m = 0` the membership pairing degenerates (an attacker-supplied `proof`
equal to the public commitment satisfies it). It is **not a fund-theft hole**: `mustConserveValue`
(`Head.hs:525`) forces `headIn = burned ⊕ adaOverhead` when there are no outputs, so it only
passes for a head holding no L2 funds (the legitimate empty fanout). Still, this is a genuine
divergence from the Agda invariant and an inconsistency with the two sibling fanout
validators that *do* guard it (and whose code comments call the empty-subset case
exploitable). Recommend either adding `numberOfFanoutOutputs > 0` to `headIsFinalizedWith` or
documenting why full fanout is exempt. (Implementation-side; not fixed here.)

### Finding C — LOW (mapping note): `accVerifyExclude` is realised by reusing the membership pairing

Agda models partial-fanout's accumulator update with a distinct predicate
`accVerifyExclude η S η'`. Plutus has no separate exclusion routine: `checkPartialFanout`
reuses `checkMembershipPairing` with the old commitment as `A` and the new commitment `η'` in
the "proof" slot, which verifies `e(η, G2) = e(η', P_S(τ)·G2)`, i.e. exactly the exclusion
relation `η = η' · ∏(τ − sᵢ)`. Substantively equivalent; worth noting in the spec-to-code
map so reviewers know membership and exclusion share one pairing identity.

### Finding D — LOW (Agda omission): `headSeed`

`OpenDatum.headSeed : TxOutRef` exists in Plutus (used to tie the head to its seed for token
policy / fanout), is absent from the Agda `Open`, and is **not** in `mustNotChangeParameters`
— so it is not preserved across increment/decrement (a produced output could change it
without rejection). It appears unused after init, so likely benign, but it is both an
Agda-omission and an unpreserved Plutus field. Flagged for the implementers.

### Finding E — LOW (Agda abstraction): single-signer cardinality

Plutus `mustBeSignedByParticipant` requires **exactly one** participant signer (errors on
zero or many), and contest derives the appended contester from that single signer. The Agda
`signedByParticipant` postulate only asserts *a* participant signed. For contest the "exactly
one signer = the contester" identity is substantive; the Agda abstracts it. Worth recording.

### Coverage boundaries (Agda intentionally does not model these)

- **`μHead` minting policy** (`HeadTokens`): the `init` checks (`cid = hash(μHead(seed))`,
  minting exactly `n+1` tokens into the head output) and `abort`. The Agda relation starts at
  `Open`; init is described only in the spec prose (§init). The fanout **burn** of `n+1`
  tokens *is* modelled (`burnAllTokensOK`).
- **Deposit/recover** (`deposit.ak` / `Deposit.hs`): the deposit deadline, the
  redeemer-index coupling that ties a deposit `Claim` to an `Increment`, and recovery. Agda's
  increment passes a deposit `ref` but does not model the deposit validator.
- **CRS reference-input mechanics** (`withCRSLookup`/`resolveCRS`/`CRS.hs`): the Agda `crs :
  OutputRef` redeemer parameter is inert; Plutus enforces the CRS reference script hash,
  address, and non-empty datum, and derives subset scalars by hashing outputs. Pure
  cryptographic plumbing beneath the abstract `accVerify`/`accVerifyExclude`.
- **Value law strength.** On close/contest the Agda states `headValueIn ≤ᵛ headValue`
  (monotone), while Plutus requires exact equality (`mustPreserveHeadValue`). Plutus is the
  *stronger* of the two, so no gap; the spec law is merely looser than necessary.

---

## Summary table

| Area | Alignment |
|---|---|
| Redeemer `Input` / `CloseRedeemer` / `ContestRedeemer` | exact (incl. indices) |
| Datum `State` constructors | exact (`Open/Closed/Final/FanoutProgress`, no `Initial`) |
| Datum fields | match, except `Open.η` commitment-vs-hash (A) and `headSeed` (D) |
| Transition dispatch | exact (8 transitions + reject-all) |
| close / contest checks | full match (Plutus value law tighter; single-signer abstracted) |
| increment / decrement checks | full match (deposit deadline/claim out of scope) |
| fanout / partial / final-partial checks | match, except `0 < m` missing in full fanout (B) |
| `μHead` init/abort, deposit validator, CRS | not modelled in Agda (coverage boundary) |
