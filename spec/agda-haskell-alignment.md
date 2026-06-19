# Agda spec vs implementation: alignment report

*(Part A: on-chain Agda vs the Plutus `νHead` validator. Part B: off-chain Agda vs `HeadLogic.hs`.)*

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

The **off-chain** Agda (`OffChain.lagda.typ`) vs the Haskell off-chain implementation
(`HeadLogic.hs` and friends) is covered in Part B below; the off-chain *spec figure* vs
`HeadLogic.hs` (which found the C1 mutual-exclusion bug etc.) is in
[`code-spec-discrepancies.md`](./code-spec-discrepancies.md).

## Verdict

**The on-chain Agda model is faithfully aligned with the Plutus validator.** The redeemer
type and the state-transition dispatch match **exactly** (constructor-for-constructor and
transition-for-transition). Every substantive validator check has a faithful counterpart in
both directions. Two findings are worth acting on (one Agda-side modelling note, one Plutus
code gap), plus several benign abstraction boundaries; none is a fund-loss/double-spend hole.

### Mechanization status

The **decidable core** of this correspondence is now machine-checked rather than reviewed by
hand. A decidable reference checker (`spec/src/Hydra/Protocol/Reference.agda`) mirrors the
unit-robust, structural conjuncts of the validity bundles (version discipline, snapshot
ordering, contester initialisation, the fanout `m > 0` guard); a typecheck-only bridge
(`ReferenceBridge.agda`) proves each bundle *implies* its reference accepts. The checker is
extracted to Haskell via MAlonzo (the `hydra-agda` package) and run as a second oracle in the
`hydra-tx` mutation tests (`Hydra.Tx.Contract.CloseDifferential`,
`Hydra.Tx.Contract.Differential`), asserting *reference-reject ⇒ validator-reject* across the
close / increment / decrement / contest / fanout families. A future validator change that drops
a decidable spec check fails these tests (demonstrated: deleting `mustNotChangeVersion` makes
the close differential fail). The crypto / value / accumulator / deadline conjuncts remain
injected as mocked operations and are *not* covered by this mechanization; they are still
matched by manual review (below).

**Scope — what the differential test does and does not establish (do not over-sell):**

- *Catches:* a validator that silently drops one of the **structural** conjuncts, i.e. close: version /
  contestation-period preservation, contesters initialised empty, `closeInitial⇒v=0∧s=0`,
  `closeAny⇒0<s`; increment/decrement: version `= suc`; contest: version preserved + snapshot
  strictly increases + exactly one contester appended; fanout: `0 < m`. One end-to-end drift-catch
  is demonstrated (close `mustNotChangeVersion`).
- *Does NOT catch (mocked `const True`):* signature/multisig validity, value conservation,
  accumulator membership/exclusion, token-burn count, deadline checks. A validator could forge any of
  these and the test would still pass.
- *Direction & abstention:* the property is one-directional, `reference-reject ⇒ validator-reject`
  only. It asserts nothing when the reference *accepts*, and **abstains** (no constraint) when a
  mutation makes the datum/redeemer unreadable; so the *exercised* coverage is whatever fraction of
  generated mutations land in the reference's `Just False` branch.
- *The `spec ⇒ real-Plutus-validator` link is hand-reviewed prose* (this document, §3), not
  mechanized; only `bundle ⇒ reference` (`ReferenceBridge.agda`) and `reference ⇒ validator`
  (the differential test, structural layer) are machine-checked.

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

### Finding A — MED (ADDRESSED): `Open.η` is a commitment in Agda, a hash in Plutus

Agda's `Open` datum stores `η : AccCommitment` (the full accumulator commitment, e.g.
`accUTxO(∅)` at init). The Plutus `OpenDatum` stores only `accumulatorHash : Hash` — the BLS
commitment is materialised only later, in `ClosedDatum`/`FanoutProgressDatum`
(`accumulatorCommitment : G1`), where fanout membership proofs need it. For the checks that
touch the open state (the close/increment/decrement signature over `cid‖v‖s‖η#`), the two
**coincide**: Agda computes `hash (ηOf d')` exactly where Plutus reads the stored
`accumulatorHash`. So this is a faithful abstraction, not a bug, but the spec's `Open` datum
is structurally **richer** than the implementation's. Worth a one-line note in the spec
(`Open` carries the commitment conceptually; the implementation stores its hash and only
materialises the commitment at close). *Addressed:* an "Implementation note (datum
representation)" was added to `OnChain.lagda.typ` (§init) recording this.

### Finding B — MED (FIXED): full `Fanout` did not enforce `m > 0`

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
exploitable). *Fixed:* added `mustHaveOutputs` (`numberOfFanoutOutputs > 0`, new error code
`FanoutZeroOutputs` = H67) to `headIsFinalizedWith` in `Head.hs`, plus a
`MutateFanoutZeroOutputs` adversarial-mutation regression test in
`hydra-tx/test/Hydra/Tx/Contract/FanOut.hs`.

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
  redeemer-index coupling that ties a deposit `Claim` to an `Increment`, and recovery. The νDeposit
  validator itself is not modelled. (As of the six-direction audit, Agda's `incrementValid` now DOES
  check the claimed deposit `ref` is spent — `depositSpentOK ctx ref`, matching Plutus
  `claimedDepositIsSpent` and §5.4 — but the νDeposit-side checks remain out of scope.)
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
| fanout / partial / final-partial checks | full match (the `0 < m` gap in full fanout, B, now fixed) |
| `μHead` init/abort, deposit validator, CRS | not modelled in Agda (coverage boundary) |

---

# Part B: off-chain Agda (`OffChain.lagda.typ`) vs `HeadLogic.hs`

Direct, field-by-field / rule-by-rule comparison of the off-chain Agda **data structures and the
`_handles_↝_` relation** against the Haskell types and handlers
(`HeadLogic.hs`, `HeadLogic/State.hs`, `HeadLogic/Input.hs`, `Hydra.Network.Message`,
`Hydra.Tx.Snapshot`, `Hydra.Node.State`). (Distinct from `code-spec-discrepancies.md`, which
compares the off-chain *spec figure* against `HeadLogic.hs`.)

**Verdict: substantive agreement, no HIGH-severity divergence.** Every Agda field maps to a Haskell
field with the expected meaning; every `Message` constructor maps to a Haskell one with matching
payloads (deposit and decommit are in the correct semantic slots on both sides — no repeat of C1);
and the three `_handles_↝_` rules (`reqTx-pending`, `ackSn-collect`, `ackSn-confirm`) are *faithful
abstractions* — reshaped for the §7 Consistency proof, they assert nothing the handlers contradict,
and their elisions (the `L̂∘tx≠⊥` guard, the all-signed/multisig-verify tests, the post-confirm
posts) are explicitly documented in the Agda.

Structural mapping (all MATCH or MATCH-different-form): Agda `Snapshot` ↔ `Hydra.Tx.Snapshot`
(`txs↔confirmed`, `utxoInc↔utxoToCommit`, `utxoDec↔utxoToDecommit`, `etaHash`↔ the hash projected
from `accumulator`, `sig`↔ `ConfirmedSnapshot.signatures`); Agda `LocalState` ↔ the Haskell state
split across `CoordinatedHeadState` + `OpenState` + `Environment` + the threaded `PendingDeposits`
map (`seenVersion↔version`, `pending↔localTxs`, `confirmed↔confirmedSnapshot`,
`pendingDeposit↔currentDepositTxId`, `pendingDecrement↔decommitTx`, `deposits↔PendingDeposits`,
`seenSigs`↔`SeenSnapshot.signatories`); Agda `Message` ↔ `Hydra.Network.Message` constructors.

Clarity notes (no bugs; documentation only):

- **B-off-1 (MED, addressed in-line):** Agda `reqSn`'s payload lists deposit `txα` before decommit
  `txω` (matching the §6 figure); the Haskell `ReqSn` record lists `decommitTx` before
  `depositTxId`. By-name, so no wire bug, but a positional read could pair them wrongly — this is the slot
  the C1 bug confused. A comment now on the Agda `reqSn` constructor states the by-name
  correspondence (`txα↔depositTxId`, `txω↔decommitTx`).
- **B-off-2 (MED):** `Snapshot.headId` / `cid` is omitted from the Agda `Snapshot` record but is the
  first signed component in Haskell and appears as `cid` in the figure's signing message.
- **B-off-3 (MED):** Agda `etaHash : Maybe ℍ` collapses Haskell's always-present `HydraAccumulator`
  plus the separately-tracked signing status (`SeenSnapshot` vs `ConfirmedSnapshot`).
- **B-off-4 (MED):** `CoordinatedHeadState.allTxs` (commented `Spec: Tall` in `State.hs`) has no
  counterpart in the off-chain Agda `LocalState` or the figure — it is an implementation-only index
  for resolving tx-ids in `ReqSn`.
- **B-off-5 (MED):** the four-state `SeenSnapshot` ADT (incl. the in-flight `RequestedSnapshot`
  used by `snapshotInFlight`) is flattened in the Agda into `seenVersion`/`seenNumber`/`seenSigs`.
- **B-off-6 (LOW):** Agda `pendingDeposit : Maybe Data` is a tx; Haskell `currentDepositTxId` is a
  tx-id. Both sides already note the spec-says-tx / impl-uses-id gap.
- **B-off-7 (LOW):** `reqTx-pending` prepends to `pending` (`tx ∷`) while the handler appends
  (`Seq.|>`); irrelevant to the current abstraction, but flag if the Consistency proof ever depends
  on `pending` ordering. Also: `_handles_↝_` has rules only for `reqTx`/`ackSn`; `reqSn`, `reqDec`
  and the chain observations live only in the figure (intended illustrative scope).
