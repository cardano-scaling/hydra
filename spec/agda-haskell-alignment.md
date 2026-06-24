# Agda spec vs implementation: alignment report

*(Part A: on-chain Agda vs the Plutus `νHead` validator. Part B: off-chain Agda vs `HeadLogic.hs`.)*

> The single canonical list of all outstanding items lives in
> [`discrepancies-and-fixes.md` → "Still open (scoped)"](./discrepancies-and-fixes.md). This report is
> the findings/coverage-boundary record; it does not maintain a parallel open list.

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
`hydra-tx` mutation tests (`Hydra.Tx.Contract.CloseDifferential`, `Hydra.Tx.Contract.Differential`,
`Hydra.Tx.Contract.InitDifferential`, `Hydra.Tx.Contract.DepositDifferential`), asserting
*reference-reject ⇒ validator-reject* across the close / increment / decrement / contest / fanout /
**init** (μHead token count) / **recover** (νDeposit after-deadline) families. A future validator change
that drops a decidable spec check fails these tests (demonstrated: deleting `mustNotChangeVersion` makes
the close differential fail). The crypto / accumulator-membership conjuncts, the token-PLACEMENT check,
the νDeposit Increment-redeemer coupling, the fanout *value conservation* (the `headAda` datum-value term
has no faithful tx counterpart — a documented boundary, not mechanizable here), and the *non-lovelace /
multi-asset* component of value conservation remain injected as mocked operations and are *not* covered by
this mechanization; they are still matched by manual review (below). (NOW mechanized — see below: the
close contestation deadline AND its bounded-validity `hi−lo≤cp`; the contest before-deadline AND the
conditional deadline-UPDATE rule; the recover after-deadline; the claim before-deadline AND the head-id
binding; the init mint count; the fanout burn-count and after-deadline.)

**Scope — what the differential test does and does not establish (do not over-sell):**

- *Catches:* a validator that silently drops one of the **structural** conjuncts, i.e. close: version /
  contestation-period preservation, contesters initialised empty, `closeInitial⇒v=0∧s=0`,
  `closeAny⇒0<s`, **and the contestation deadline** (`tfinal == validity.hi + cp`, caught via the
  `deadlineDriftTx` mutation: "reference rejects a close with a drifted contestation deadline"
  === `Just False`); recover (νDeposit): **the after-deadline conjunct** (`tRecover < validityLo`,
  caught via `deadlineNotReachedRecoverTx`); init (μHead): **the token count** (`mintedCount == n+1`,
  caught via `extraTokenInitTx`); increment: version `= suc` **and lovelace value conservation**
  (`adaIn + adaDelta == adaOut`: head input + claimed deposit = head output on the ada component, read
  live off the tx); decrement: version `= suc` **and lovelace value conservation**
  (`adaOut + adaDelta == adaIn`: head output + decommitted outputs = head input, the decommit outputs
  read as `take numberOfDecommitOutputs (tail outputs)` off the tx, with a deterministic
  lovelace-perturbing mutation `DecrementChangeHeadLovelace` ensuring the catch fires); contest:
  version preserved + snapshot strictly increases + exactly one contester appended **+ posted before
  the contestation deadline** (`validityHi ≤ tfinal`, `MutateValidityPastDeadline` →
  `UpperBoundBeyondContestationDeadline`) **+ the conditional deadline-UPDATE rule** (`tfinal' = if
  all-contested then tfinal else tfinal+cp`); close ALSO **+ bounded validity** (`hi − lo ≤ cp`); fanout:
  **all `n+1` head tokens burned** (`burnedCount == n+1`) **+ posted after the deadline** (`tfinal < lo`)
  (m = 0 is permitted — empty-head finalisation, see Finding B); init: **mint count n+1 + token PLACEMENT**
  (ST present + head output carries n+1 head-policy tokens, `RemovePTsFromHead`/`removePTsInitTx` →
  `MissingPTs`); claim (νDeposit): **the before-deadline conjunct** (`validityHi ≤ tRecover`, caught
  via `deadlineSurpassedClaimTx` → `DepositPeriodSurpassed`) **+ the head-id binding** (`depositCid ==
  headCid`, caught via the cross-head deposit mutation). One end-to-end drift-catch is demonstrated
  (close `mustNotChangeVersion`).
  (The increment/decrement
  lovelace checks use the builtin `_==_`, extracted to native integer equality; the structural `_==ᵇ_`
  is O(n) unary recursion and hangs on lovelace-scale values. Their bridge reflection rests on the
  `==-sound` postulate in `ReferenceBridge.agda`.)
- *Does NOT catch (mocked `const True`):* signature/multisig validity, accumulator membership/exclusion,
  the init seed-spent + datum binding (token PLACEMENT is now caught), the νDeposit Claim **Increment-redeemer coupling** (the
  other half of `expect_increment_redeemer`; the head-id half IS caught), **fanout value conservation**
  (the `headAda` datum-value term has no faithful tx counterpart — a documented boundary), and the
  **per-token granularity** of increment/decrement value conservation (it now checks ada AND the total
  non-ada token quantity via `nonAdaOf` — catching native-token siphons, demonstrated by
  `tokenSiphonIncrementTx` — but two token sets with the same total alias, so distinguishing them needs
  the full `Value` map). A validator could forge the still-mocked ones and the test would still pass. (NOW caught — see *Catches*: the close contestation deadline + bounded validity; the
  contest before-deadline + conditional deadline-UPDATE; the fanout burn-count + after-deadline; the
  recover after-deadline; the claim before-deadline + head-id binding; the init mint count + token
  PLACEMENT (ST present + n+1 head-output token count); and the **participant signature**
  `mustBeSignedByParticipant` for close/contest/increment/decrement — the shared `participantSignedRefᵇ`
  overlap check, `SignerIsNotAParticipant`, demonstrated by `noSignerIncrementTx`.)
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

### Finding B — MED (REVERTED — the Agda invariant was the error): full `Fanout` must allow `m = 0`

The Agda `fanoutValid` required `0 < m` (as do `partialFanoutValid` and `finalPartialFanoutValid`).
In Plutus, `checkPartialFanout`/`checkFinalPartialFanout` guard `numberOfPartialOutputs > 0`, but
`headIsFinalizedWith` (the full `Fanout`) deliberately has **no** such guard. An earlier pass treated
that as a divergence and added a `mustHaveOutputs` (`FanoutZeroOutputs` = H67) guard to match the Agda
`0 < m`. **That was the bug:** the full 0-output fanout is the *legitimate and only* way to finalise an
EMPTY head (the universal initial state under ADR-033; UTxO enters only via deposit), burning its n+1
tokens — the guard makes those tokens unburnable forever. The hydra-node model tests caught it,
shrinking to `Init → Close → Fanout`. It is provably **not** a fund-theft hole: `mustConserveValue` uses
strict `==`, so with `m = 0` only a head holding no L2 funds passes (`headIn = burned ⊕ adaOverhead`,
exactly), and the KZG membership degeneracy is harmless because security rests on the `==`, not the
pairing. *Reverted:* removed the `mustHaveOutputs` guard (→ `headIsFinalizedWith` byte-identical to
master, golden hash restored, no regeneration) and its `MutateFanoutZeroOutputs` test; removed
`FanoutValid.outputsPositive` and the matching reference/bridge/differential `0 < m` for the full path.
The two sibling PARTIAL guards are KEPT (a zero-output partial *batch* makes no progress — genuinely
invalid). This is a real bug the formalization itself introduced and the differential/model layer then
surfaced and corrected.

**Regression guard (the missing dual obligation).** The safety corpus could not see this bug by
construction: it only proves *accepted ⇒ safe*, and an over-strict conjunct merely shrinks the accept set
(it can never falsify a soundness theorem). The violated property is its dual — *completeness-of-
acceptance / non-stuckness*: the terminal fanout bundle must be **inhabited** for the states that reach it.
`Hydra/Protocol/OnChainCoverage.agda` now supplies it: a `Reachable` inductive over `HeadDatum`, a proof
that the empty Closed head is reachable (`reach-empty-closed`), and the inhabitation/coverage lemmas
`fanout-empty-inhabited` / `finalize-reachable-empty` / `fanout-coverage`. Re-adding `outputsPositive : 0 <
m` makes these fail to typecheck (`mkFanoutValid` would need a term of the empty type `0 < 0`) — verified
adversarially. This is the first *coverage* obligation in the corpus; the general lesson is that each
accept path (especially terminal/finalize) should carry such a dual witness, or over-strictness stays
invisible. (Atemporal and one-step — distinct from the deferred §7-P3 temporal liveness.)

The module also adds a **valid-gated reachability** `Reachableᵛ` (each step carries its `*Valid` witness,
unlike the shape relation), and from it the structural invariant `progress-nonEmpty`: the machine **never
reaches an empty `FanoutProgress`** (`ηOf ≢ accUTxO ∅`), immediate from `PartialFanoutValid.notDoneOK`.
That settles the symmetric question about the kept partial-path guard: `FinalPartialFanoutValid.outputsPositive
: 0 < m` is **sound, not over-strict** — the remainder a final batch fans out is always non-empty, so
`m ≥ 1` is satisfiable, and `progress-finalizable` now **derives** that positivity (not assumes it):
`progress-nonEmpty` gives `η ≢ G₁`, hence (with the commits-to-set interface `η ≡ accUTxO V` and the
existing `accUTxO-∅`, by contraposition) `V ≢ ∅`, hence `setSize-pos` gives `0 < setSize V = m`.

**Simple-B6 (set-behaviour laws), done.** Rather than a concrete accumulator (the declined B6, which would
make the spec stronger than the validator), three *specifying* postulates pin the accumulator's set-level
behaviour while leaving the KZG crypto abstract: `accVerify-self` (a set is a member of its own commitment),
`setSize` (UTxO-set cardinality) and `setSize-pos` (non-empty ⇒ positive). These are typecheck-only and used
only by the coverage module — they add no check to the `*Valid` bundles, so the differential / spec⇒validator
alignment is untouched (no stronger-than-validator issue). With them the coverage obligations now **derive**
their membership witnesses and the final-partial `0 < m` instead of threading them; only the genuinely-context
antecedents (deadline, burn, value conservation) remain hypotheses. The empty-head full fanout is still the
concrete over-strictness *catch* (re-adding `outputsPositive : 0 < m` breaks the build — re-verified).

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

### Finding E — LOW (Agda abstraction): single-signer cardinality — mostly closed

Plutus `mustBeSignedByParticipant` requires a participant signer (the per-tx single/zero/many
cardinality is a separate ledger check), and contest derives the appended contester from the
signer. `signedByParticipant` was a bare postulate; it is now the structural
`∃ kh, signerKeyHash ctx kh ∧ quantityOf (headValue ctx) (cid,kh) ≡ 1` — *a* signer holding a PT —
which matches `mustBeSignedByParticipant`, and is now an extractable differential conjunct
(`participantSignedRefᵇ`, the signer-key-hashes vs PT-names overlap; wired into increment).
REMAINING: the exact-cardinality nuance (exactly-one signer = the contester identity for contest)
is still abstracted; the signer-naming half (`signerKeyHash`) stays a postulate (the opaque
set-theory model blocks `ℙ`-membership). Worth recording.

### Coverage boundaries (Agda intentionally does not model these)

- **`μHead` minting policy** (`HeadTokens`): the init token **COUNT** (minting exactly `n+1` tokens,
  `mintedCount == suc n`) is NOW mechanized — `initRefᵇ` (`Reference.agda`) + `initValid→ref`
  (`ReferenceBridge.agda`) + the `InitDifferential` differential (`extraTokenInitTx`). Still out of
  scope: token **PLACEMENT** (single ST + n unique PTs into the head output), the seed-spent check, the
  datum `headId`/`seed` binding (need multi-asset token-name lookup), and `abort`. The fanout **burn**
  of `n+1` tokens (`burnAllTokensOK`) is modelled in the bundle but still injected at the reference layer.
- **Deposit/recover** (`deposit.ak` / `Deposit.hs`): the **Recover** arm after-deadline conjunct
  (`tRecover < validityLo`) is NOW mechanized — `recoverRefᵇ` (`Reference.agda`) + `recoverValid→ref`
  (`ReferenceBridge.agda` via `<ᴮ-sound`) + the `DepositDifferential` differential
  (`deadlineNotReachedRecoverTx`). Still out of scope: the recovered-outputs serialisation-hash equality
  (mocked `recoverHashOK`), and the **Claim** arm (the before-deadline `hi ≤ tRecover` is type-encoded in
  `claimValid` but not bridged/differentially tested; the redeemer-index coupling tying `Claim` to an
  `Increment`). Agda's `incrementValid` does check the claimed deposit `ref` is spent
  (`depositSpentOK ctx ref`, matching Plutus `claimedDepositIsSpent`).
- **CRS reference-input mechanics** (`withCRSLookup`/`resolveCRS`/`CRS.hs`): the Agda `crs :
  OutputRef` redeemer parameter is inert; Plutus enforces the CRS reference script hash,
  address, and non-empty datum, and derives subset scalars by hashing outputs. Pure
  cryptographic plumbing beneath the abstract `accVerify`/`accVerifyExclude`.
- **Value law strength.** On close/contest the Agda now states `headValueIn ≡ headValue` (exact
  equality, AUDIT-5), matching Plutus `mustPreserveHeadValue`. (This was previously `≤ᵛ` (monotone)
  and is no longer.)

---

## Summary table

| Area | Alignment |
|---|---|
| Redeemer `Input` / `CloseRedeemer` / `ContestRedeemer` | exact (incl. indices) |
| Datum `State` constructors | exact (`Open/Closed/Final/FanoutProgress`, no `Initial`) |
| Datum fields | match, except `Open.η` commitment-vs-hash (A) and `headSeed` (D) |
| Transition dispatch | exact (8 transitions + reject-all) |
| close / contest checks | full match (Plutus value law tighter; participant signature now structural, only exact-cardinality abstracted) |
| increment / decrement checks | full match (deposit **Recover** after-deadline now mechanized; deposit **Claim** before-deadline + redeemer coupling out of scope) |
| fanout / partial / final-partial checks | full match (full-fanout `0 < m` (B) reverted: m=0 allowed for empty-head finalisation; partial-batch `0 < m` kept) |
| `μHead` init token count, νDeposit recover after-deadline | mechanized (reference + bridge + differential) |
| `μHead` token placement/abort, νDeposit Claim arm, CRS | not modelled in Agda (coverage boundary) |

---

# Part B: off-chain Agda (`OffChain.lagda.typ`) vs `HeadLogic.hs`

Direct, field-by-field / rule-by-rule comparison of the off-chain Agda **data structures and the
`_handles_↝_` relation** against the Haskell types and handlers
(`HeadLogic.hs`, `HeadLogic/State.hs`, `HeadLogic/Input.hs`, `Hydra.Network.Message`,
`Hydra.Tx.Snapshot`, `Hydra.Node.State`). (Distinct from `code-spec-discrepancies.md`, which
compares the off-chain *spec figure* against `HeadLogic.hs`.)

**Verdict: substantive agreement, no HIGH-severity divergence.** Every Agda field maps to a Haskell
field with the expected meaning; every `Message` constructor maps to a Haskell one with matching
payloads (deposit and decommit are in the correct semantic slots on both sides, no repeat of C1).
The handler model now covers all five network handlers (`_handles_↝_`: `reqTx-pending`,
`reqDec-pending`, `ackSn-collect`, `ackSn-confirm`, `reqSn-sign`) and the six chain observations
(`_observes_↝_`: `deposit-add`, `recover-del`, `tick-update`, `increment-obs`, `decrement-obs`,
`initialTx-obs`), with every network-handler guard de-abstracted to an operational premise the node
checks (no-in-flight for `reqDec`, `(j,·) ∉ Σ̂` for collect, n-of-n for confirm, `v = v̂ ∧ s = ŝ+1 ∧
leader(s)=j` for sign). These are *faithful refinements* of the node: reshaped for the §7 Consistency
proof, they assert nothing the handlers contradict, and the deposit/decommit flow is now wired into
the §7 system (the `offChain` step relation), where `NoBothInFlight` is proved an invariant. The
extracted decisions are additionally checked against a Haskell transcription of the figure and, for
leader selection, against the real `Hydra.HeadLogic.isLeader` (see Part C).

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
- **B-off-2 (RESOLVED):** the Agda `Snapshot` now carries `cid : ℍ` (the head currency id), and the §7
  signing message `msgOf = snapMsg cid v s η#` matches the Haskell first-signed-component / figure
  `cid‖v‖s‖η#`. cid is constant within a head, so the §7 safety proofs are unchanged (they use `msgOf`
  abstractly); the message is now faithful.
- **B-off-3 (RESOLVED):** the Agda `Snapshot` now carries `etaHash : ℍ` (always present, like the node's
  `HydraAccumulator`), with signing status tracked solely by `sig : Maybe AggSig`. `snapMsg`/`msgOf` take
  the always-present `ℍ`; the §7 proofs use `msgOf` abstractly, so they are unchanged (typecheck green).
- **B-off-4 (MED):** `CoordinatedHeadState.allTxs` (commented `Spec: Tall` in `State.hs`) has no
  counterpart in the off-chain Agda `LocalState` or the figure — it is an implementation-only index
  for resolving tx-ids in `ReqSn`.
- **B-off-5 (MED, deferred by design):** the four-state `SeenSnapshot` ADT (incl. the in-flight
  `RequestedSnapshot` used by `snapshotInFlight`) is flattened in the Agda into
  `seenVersion`/`seenNumber`/`seenSigs`. This is a FAITHFUL abstraction for the safety proofs: the
  seen-snapshot number `ŝ` (= `seenNumber`) carries all the safety-relevant content `signNumBound` /
  `sigDedup` / Consistency rely on. De-flattening would mean re-proving those over a richer state
  (41 use-sites across the model, §7 invariants, and the just-completed proofs) for no safety gain, so
  it is deliberately left as a documented abstraction rather than destabilise the verified result.
- **B-off-6 (RESOLVED):** the Agda field is now named `currentDepositTxId : Maybe Data` (matching the
  node) and documented as the deposit tx-id keying the `𝒟` registry. It stays `Data`-typed (the spec's
  opaque tx/id representation).
- **B-off-7 (RESOLVED):** `reqTx-pending` now appends to `pending` (`pending ++ [tx]`), matching the
  node's oldest-first `Seq.|>`. The §7 proofs are agnostic to `pending` ordering (typecheck unchanged),
  so this is a pure fidelity alignment.

# Part C: off-chain differential (extracted reference vs transcription / node)

`OffChainReference.agda` is the executable, decidable half of the off-chain model: kept self-contained
over `Agda.Builtin` types so MAlonzo extracts it to clean Haskell (`Hydra.Agda.OffChainReference`),
exactly as `Reference.agda` does for the on-chain validators. Each decidable function is the guard of
one handler arm, run as a second oracle in two test families:

- `hydra-tx` `OffChainDifferential` pins each extracted decision to a faithful Haskell transcription
  of the §6 figure (golden cases + QuickCheck): the `tick` deposit-status transition
  (`depositStatusRef`), reqSn signing eligibility (`signEligibleRef`), reqDec eligibility
  (`reqDecEligibleRef`), ackSn-collect no-double-sign (`notAlreadySignedRef`), ackSn-confirm n-of-n
  (`allSignedRef`), and contest re-post (`contestEligibleRef`).
- `hydra-node` `OffChainLeaderSpec` binds the extracted round-robin leader (`leaderRef`, the figure's
  `leader(s)`) to the REAL `Hydra.HeadLogic.isLeader`, closing the figure↔Agda↔node loop for leader
  selection. They agree for every party index at every `sn ≥ 1`; they diverge only at `sn = 0` (Nat
  truncated `0 ∸ 1 = 0` vs `Int` `-1 mod n = n-1`), which is outside the protocol's domain (snapshot
  numbers start at 1). This `sn = 0` boundary is exactly the kind of edge a real-node differential
  surfaces that a same-language transcription would not.
