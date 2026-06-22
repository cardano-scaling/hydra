# Discrepancies & fixes found during the Typst + Agda conversion

A running log of spec bugs, inconsistencies, and Agda/prose mismatches found (and
fixed) while migrating the specification from LaTeX to Typst and growing the Agda
formalisation. Newest sections appended at the bottom; keep updating as work continues.

Legend: **status** is `fixed`, `noted` (documented in the spec but not yet
type-enforced), or `open` (not yet addressed).

## A. Spec-content bugs (genuine errors in the specification itself)

| # | Location | Problem | Fix | How found | Status |
|---|---|---|---|---|---|
| A1 | OnChain §5.7 (Contest) | Prose said the state is "advanced from `datumHead ∼ open` to `datumHead' ∼ closed`", but contest is `closed → closed` (its own arrow, the figure caption, and the Agda rule all agree). Stale copy-paste from §5.6 (Close). Pre-existing in the original `.tex`. | Corrected to "from `closed` to `closed`". | Consistency review #1 | fixed |
| A2 | OnChain §5 (datum tuples) | The `datumHead` tuple had **inconsistent arity**: the Open tuple included a `seed` field at init, a participant count `nop` at increment, and neither elsewhere. | Reconciled to one schema. `seed` dropped (not persisted). **`n` (participant count): later carried in the datum** (Open/Closed/FanoutProgress) — the initial "derive from PTs" choice was revised so the `n`-dependent validator checks (`\|contesters'\|=n`, `n+1` burn) are type-checkable; `contesters` is modelled as a `List VKey`. | Consistency review #1 (revised in Item 1) | fixed |
| A3 | OnChain §5.8 (Fanout / Partial fan-out) | The `Closed` source tuple in `fanout` and `partialFanout` **dropped the `adaO` field**, contradicting the adjacent value-conservation prose that relies on it. | Restored `adaO` to both Closed source tuples. | Consistency review #1 | fixed |
| A4 | OnChain §5.4–5.8 (signature checks) | ~10 multisignature checks read the **tautology** `(η')# = (η')#` ("where … is the hash of η'"). Vacuous as written; the Prelude keeps `η` (commitment) distinct from `η# = hash η`. | Changed all to `(η')# = hash(η')`. | Consistency review #2 | fixed |
| A5 | `short.bib` (`CCS:MicOhtRey01`) | Entry referenced undefined BibTeX `@string` abbreviations (`ccs01ed`, `ccs01name`, …) that render blank even under LaTeX and make Typst's parser error. | Inlined factual `booktitle`/`publisher` (CCS 2001, ACM), dropped the uncertain fields. | First Typst build | fixed |
| A6 | OnChain §5.4 / §5.5 (value conservation) | Increment and decrement combined multi-asset values with `∪` (set union), which does **not** add same-asset quantities; the fan-out family correctly uses additive `⊕`. Inconsistent and semantically wrong for values. | Changed increment/decrement to additive `⊕` (`valHead ⊕ valDeposit = valHead'`, `valHead' ⊕ (⊕ valⱼ) = valHead`). | Item 1 spec cross-check | fixed |

## B. Agda ↔ prose mismatches (the formal model not matching the spec text)

| # | Location | Problem | Fix | How found | Status |
|---|---|---|---|---|---|
| B1 | OnChain — `HeadDatum` `η` | The datum's accumulator `η` was modelled as `ℍ` (a hash), collapsing the spec's distinction between the commitment `η` and its hash `η#`. | Postulated `AccCommitment` and made `η : AccCommitment` (with `η# = hash η`). | Agda faithfulness review | fixed |
| B2 | OnChain — transition relation | The relation was missing the `partialFanout` / `finalPartialFanout` transitions and the `FanoutProgress` flow; `version` preservation/bump was not encoded. | Added the missing rules; encoded `v' = v+1` (increment/decrement via `suc v`) and `v' = v` (close/contest, reused variable). | Agda faithfulness review | fixed |
| B3 | OnChain — `HeadRedeemer` | The `Fanout`/`PartialFanout`/`FinalPartialFanout` redeemers omitted the `crsRef` (CRS reference) payload the prose lists. | Added `(crs : OutputRef)` to all three and threaded it through the relation. | Consistency review #2 | fixed |
| B4 | OnChain — `deadlineOK` | A comment claimed the predicate covered **both** close and contest, but contest's deadline is conditional (`tfinal' = tfinal` if all parties contested, else `tfinal + T`). | Renamed to `closeDeadlineOK`, scoped the comment to close, noted contest's conditional deadline is a separate predicate. | Consistency review #2 | fixed |
| B5 | OnChain — `close` rule | The rule allowed an arbitrary contester set on the produced `Closed` datum, but the spec says close initialises `contesters = ∅`. | Target now uses `∅ˢ` — empty contesters is type-enforced. | Consistency review #2 | fixed |
| B6 | OnChain — `contest` rule | The rule permitted the new key `kh ∈ C` (so the set need not actually grow); the spec requires `keyHash ∉ contesters`. | Added a `¬ (kh ∈ C)` premise — type-enforced (added `¬_` to the Prelude). | Consistency review #2 | fixed |
| B7 | Preliminaries — `ValidityInterval` | The record dropped the spec's `lo ≤ hi` constraint (a structurally-valid but inverted interval was admissible). | Added a `lo≤hi : lo ≤ hi` proof field. | Agda faithfulness review | fixed |
| B8 | OnChain — relation prose | The prose claimed "a rule that violated any of these would fail to type-check" for invariants the types do **not** enforce (contester growth/emptiness, signatures, deadlines), overstating the guarantee. | Rewrote to state exactly what is enforced (state-machine shape, version discipline, close-inits-∅) vs. what is a separate predicate. | Consistency review #2 | fixed |
| B9 | OffChain — `_handles_↝_` reqTx rule | The single concrete off-chain rule appends to `pending` unconditionally, omitting §6.4's `wait (L̂ ∘ tx ≠ ⊥)` guard and the local-ledger update. | Kept as a deliberate simplification with an explicit `NOTE` comment (full handler needs an `applytx` model). | Consistency review #2 | noted |
| B10 | OffChain ↔ OnChain (close/contest) | Off-chain `postTx(close/contest, …)` passes `v, s, η#, ξ`, but the on-chain `CloseType`/`ContestType` redeemer carries only `(ξ, η#)`. | Added a note: `v`/`s` are authenticated by the multisignature over `cid‖v‖s‖η#` and recorded in the datum, not separate redeemer fields. | Consistency review #2 | noted |

## B′. Found by the critical review of the validity bundles (all fixed)

These were bugs in the per-transaction validity bundles, caught by an adversarial review
that diffed each bundle's conjuncts against the §5.x prose checks.

| # | Location | Problem | Fix | Status |
|---|---|---|---|---|
| B11 | `closeSigOK`/`contestSigOK` | The `Used` close/contest case signs over the *previous* version `v-1` (a pending delta is applied), but the helper used the current `v` uniformly — so it **rejected every valid `Used` transaction**. | `Used` branch now verifies over `v ∸ 1` (§5.6/§5.7). | fixed |
| B12 | `contestValid` | Missing the spec's `s' > s` (the contested snapshot number must strictly increase). | Added `s < snapNum d'`. | fixed |
| B13 | close/contest/increment/decrement bundles | Missing the "signed by a participant" check (`keyHash_i ∈ txKeys`); the comments over-claimed "only inhabited for valid txs". | Added `signedByParticipant cid ctx`, later un-mocked to a structural `∃ kh, signerKeyHash ∧ quantityOf valHead (cid,kh) ≡ 1` (see `signedByParticipant` in the "Still open (scoped)" list). | fixed |
| B14 | `contestValid` | Missing "posted before deadline" (`txValidityMax ≤ tfinal`) and value-preservation (`valHead' ⊇ valHead`). | Added `ValidityInterval.hi … ≤ tfin` and `headValueIn ≤ᵛ headValue`. | fixed |
| B15 | fanout / partial-fanout / final-partial-fanout bundles | Missing the **anti-theft** checks: `m > 0` (no zero-output batch), `txValidityMin > tfinal` (after deadline), `noMint` (partial fan-out), and value conservation. | Added `0 < m`, `tfinalOf d < ValidityInterval.lo …`, `noMint` (partial), and `fanoutValueOK`/`partialFanoutValueOK` (conservation abstracted). NB the FULL-fanout `0 < m` was later REVERTED — see B17. | fixed (full-fanout `0 < m` reverted) |
| B17 | `FanoutValid` (full fanout) + the νHead validator `headIsFinalizedWith` | **Bug introduced by the formalization, caught by the integration tests.** B15's `0 < m` on the FULL fanout (and a matching branch-new validator guard `FanoutZeroOutputs`/H67) made an **empty head un-finalizable**: an empty head (the universal initial state under ADR-033; UTxO enters only via deposit) finalizes via a 0-output fanout, which the guard rejects, stranding the n+1 ST/PT tokens forever. Surfaced by the hydra-node model tests shrinking to `Init→Close→Fanout`. m=0 full fanout is safe — value conservation uses strict `==`, so nothing can be stolen. The corpus's safety proofs could NOT see this (over-strictness only shrinks the accept set), so a coverage/inhabitation obligation was added as a regression guard. | Reverted the validator guard (→ master-identical, golden hash restored, no regen) and its `MutateFanoutZeroOutputs` test; removed `FanoutValid.outputsPositive` + the reference/bridge/differential `0 < m` for the full path. KEPT the partial / final-partial `0 < m` (a zero-output partial batch makes no progress). Added `Hydra/Protocol/OnChainCoverage.agda`: a `Reachable` inductive over datums, a proof the empty Closed head is reachable, and `fanout-empty-inhabited`/`finalize-reachable-empty`/`fanout-coverage` — the dual (inhabitation) obligation; re-adding `0 < m` now FAILS the spec build (adversarially verified). | fixed |

### Second review pass

| # | Location | Problem | Fix | Status |
|---|---|---|---|---|
| B16 | `closeValid`/`contestValid` | **Soundness hole:** the signature was verified over the redeemer-supplied `η#`, never tied to the `η'` actually stored in the produced datum — so a close/contest could be signed over an unrelated accumulator. | Added `closeηOK`/`contestηOK` requiring `η# ≡ hash(ηOf d')` (spec §5.6/§5.7 `(η')# = hash(η')`). | fixed |
| B17 | `closeValid` (`Any`) | The `closeAny` case did not enforce the spec's `s' > 0`. | Added `closeAnyOK` (`0 < snapNum d'` for `closeAny`). | fixed |
| B18 | `OffChain` `Message` | Had three constructors not in §6 (`ackTx`, `confSn`, `reqInc` — the latter are *output* events / driven by `tx_α`, not messages), and `reqSn` dropped its payload. | Reduced to the real §6 messages `reqTx`/`reqDec`/`reqSn`/`ackSn`; `reqSn` now carries `(v, s, txReq, txα, txω)`. | fixed |
| B19 | `OffChain` `LocalState` | Missing the §6.3 variables `tx_α` (pending deposit), `tx_ω` (pending decrement) and `𝒟` (deposit registry). | Added `pendingDeposit`, `pendingDecrement` (`Maybe Data`), `deposits`. | fixed |
| B20 | `OffChain` `Snapshot` | The snapshot object `S̄` was missing `S̄.(η')#` and `S̄.σ` (needed by `hpClose`). | Added `etaHash : Maybe ℍ`, `sig : Maybe AggSig`. | fixed |

Low/noted from the second pass (not changed): `Used` close/contest computes `v ∸ 1` which at `v=0`
gives `0` rather than being rejected — faithful to the spec's implicit `v−1` (`v≥1` always holds for a
`Used` case); `ackSn` reuses `AggSig` for an individual signature; `_‖_` left-nests (matches the spec's
binary `‖` literally, though a flat concat may be intended — a spec-level question for the authors).

### Third review pass (Agda + Hydra expert lens)

| # | Location | Problem | Fix | Status |
|---|---|---|---|---|
| B21 | `increment`/`decrement` rules (`OnChain` `_⟶⟨_⟩_`) | **adaO not conserved:** both rules bound a *fresh* `ada'` in the target `Open … (suc v) η' ada'`, so the type permitted the min-UTxO overhead `adaO` to change across an increment/decrement. Spec §5.4/§5.5 require `adaO' = adaO`. | Dropped the `ada'` binder; the target now reuses the source `ada` (`Open … (suc v) η' ada`), making `adaO` conservation hold by construction. | fixed |
| B22 | Init datum prose (`OnChain`, §init) | Text said the head output's datum has `η = hash(∅)` ("the hash of the empty initial UTxO set"). This conflates the accumulator commitment `η` with its hash `η#`; the datum field `η` is the *commitment*, and close-Initial requires `η ≡ accUTxO(∅)`. | Changed prose to `η = accUTxO(∅)` (with a note that `η# = hash(η)` is what later snapshot signatures attest to), matching `closeInitialOK` and §5.6. | fixed |

| B23 | `Message.ackSn` / `LocalState.seenSigs` (`OffChain`) | Both used `AggSig` (resp. `ℍ`) for what §6 calls an *individual* party signature `σⱼ` — `ackSn` carries one party's signature, and `Σ̂` accumulates the individual `σⱼ` later combined via `msComb` into the aggregate stored in `S̄.σ`. Conflating individual and aggregate signature types. | Added a distinct `PartySig` postulate (individual signature, ≠ aggregate `AggSig`); `ackSn` now carries `PartySig` and `seenSigs : List (ℕ × PartySig)`. (`Sig` was taken by the `MultiSignatureScheme` record field, hence `PartySig`.) | fixed |
| B24 | `_handles_↝_` relation (`OffChain`) | The relation encodes only the `reqTx` handler, but its name/comment did not state that the §6.4 *figure* is the normative source — risking a reader treating the thin relation as the whole off-chain semantics. | Reworded the header comment to mark the relation ILLUSTRATIVE and name the `Protocol flow` figure as authoritative for all §6.4 handlers. | fixed |

Confirmed correct by this pass (no change): version discipline (`suc v` on inc/dec, preserved on close/contest); signature version sources (`v` for Unused/Any, `v ∸ 1` for Used); `η#` binding to stored `η'` (B16); conserved head parameters (`cid`/`hk`/`n`/`cp`); contest list growth + dedup (`¬ kh ∈ˡ C`, `kh ∷ C`); all snapshot/deadline/value/burn predicates; `_‖_` associativity; symbol fidelity throughout. Low/by-design (not changed): inc/dec leave `s`/`m`/deposit-ref unconstrained at the type level (constrained by separate predicates); `v ∸ 1` underflow at `v=0` (unreachable for a `Used` case); validity bundles are `Set`-valued rather than decidable `Bool` (deliberate spec-level choice).

## C. Build / tooling / presentation fixes

| # | Area | Problem | Fix | Status |
|---|---|---|---|---|
| C1 | Build warnings | `./build.sh` emitted a `path` deprecation warning from cetz 0.3.2. | Bumped vendored `fletcher` 0.5.5→0.5.8 / `cetz` 0.3.2→0.3.4. | fixed |
| C2 | Agda rendering | Agda code blocks rendered without syntax colouring (no upstream Agda→Typst backend). | Added a vendored `agda.sublime-syntax` grammar + `set raw(syntaxes: …)`. | fixed |
| C3 | Links | Hyperlinks (refs/citations/URLs) were not visually distinguished. | `show link` → blue + underline. | fixed |
| C4 | Table of contents | ToC entries were not blue (they are `outline.entry`, not `link`, so `show link` missed them). | Added `show outline.entry: set text(fill: …)`. | fixed |

## D. Still open / deferred (found, not yet fully fixed)

> Historical log: D1–D3 below are now resolved (the D2 here is the §5.7 *contest deadline* in the
> bundle, fixed — not the μHead "D2" or the live `C3-contest-deadline` reference-layer item). The
> authoritative outstanding list is "Still open (scoped)" near the end of this file.

- **D1 — Initial close case (§5.6):** *fixed* — `closeInitialOK` now encodes `v = 0`, `s' = 0`,
  **and** `η' = accUTxO(∅)` (via the postulated `accUTxO`).
- **D2 — contest deadline (§5.7):** *fixed* — `contestDeadlineOK` now fully computes the
  conditional: `tfinal' = tfinal` if `length contesters' ≡ n` else `tfinal + T`. Enabled by
  modelling `contesters` as `List VKey` (so `length`) and carrying `n` in the datum.
- **D3 — predicate-level checks:** *done* — all encoded and **wired into per-transaction validity
  bundles** (`closeValid`, `contestValid`, `incrementValid`, `decrementValid`, `fanoutValid`,
  `partialFanoutValid`, `finalPartialFanoutValid`), each conjoining the state-machine step with its
  checks (deadlines, `noMint`, `closeInitialOK`, `snapshotSigOK`, value conservation, `burnAllTokensOK`,
  `fanoutMembersOK`/`fanoutExcludeOK`/`partialFanoutNotDoneOK`). Head-output identification (finding
  the head/deposit/decommit values in the `Context`) was abstracted by postulated extractors;
  `headValue`/`headValueIn` are now DERIVED via `valueAtOut`/`valueAtIn` (per D5/AUDIT-6); concretizing
  the remaining `depositValue`/`decommitValue` by an output-search over `Context.outputs` is the
  residual refinement (tracked in the canonical "Still open (scoped)" list below).
- **D4 — security lemmas (§7):** *P0 done; P1-real DONE: the §7 safety core is now fully DERIVED from
  a signature model, not assumed; plan in
  [`security-formalisation-plan.md`](./security-formalisation-plan.md).*
  `Security.lagda.typ` was rebuilt around a signature model: the global `System` records individual
  signatures (`sigs : List (Fin parties × Snapshot)`, no `chainTxs`); a snapshot is `Certified` once
  EVERY party signed it (full multisignature); the step relation `_⟶ˢ_` is `signHonest` (an honest
  party signs only an *applicable* snapshot, ≤1 per number, **extending its own confirmed snapshot**),
  `signCorrupt` (corrupt parties sign arbitrarily), `confirm` (adopt a certified snapshot), `corrupt`.
  The machine-checked `invariant` now **DERIVES** all three safety lemmas (no longer assumes any):
  (L1) two certified snapshots of the same number are equal (`agree`, from the "≤1 per number" guard);
  (L3) every honest party's confirmed snapshot is applicable to `U₀` (`conf-applicable` — the `confirm`
  case discharges it from the "sign only applicable" guard, since a certified snapshot carries the
  confirmer's own signature); and (L2) confirmed snapshots nest by number (`confirmed-nest` via
  `cert-nest` — a gap induction using the `sigChain` invariant + L1 + `cert-pos`; **no longer a
  postulate**). **Consistency** (`consistency`), **Soundness** (`soundness`) and **Completeness**
  (`completeness`) all follow with NO safety postulate. `confirm` checks the §3.2 aggregate
  multisignature via the scheme's verifier `msVfy` (`AggVerified snap = msVfy aggKey (msgOf snap)
  (sigOf snap) ≡ true`), and the named axiom `ms-unforgeable : AggVerified snap → Certified sys snap`
  (the scheme's unforgeability) bridges that operational check to "every party signed". Residual
  postulates: only the ledger `applyTxs`/nil/compose, the bridge glue `outsOf`, `ms-unforgeable`, and
  `Liveness`. **The on-chain and off-chain halves remain
  linked** (`Reflects` rebased to a finalized snapshot + `reflect-sound`/`reflect-fanout-⊆`): the
  on-chain Closed datum's accumulator commits to `U₀ ∘ (txs snap)` and the fanout distributes only its
  outputs — via posited accumulator laws (`accUTxO-∅`, `accVerify-sound`, `accVerify-complete`; KZG not
  modelled). This REPLACES the earlier single-chain model that simply assumed the confirmed chain is
  applicable. Liveness (P3) remains abstract (`TODO(D4-P3)`). `SnapshotMonotone` is a concrete example
  property. (`P1-real DONE: L1/L2/L3 + Consistency/Soundness/Completeness all derived — only P3 open`)

- **D5 — Agda deepening (post-audit, 2026-06-19):** three on-chain coverage gaps narrowed.
  - **Value conservation made real:** `headValue`/`headValueIn` are no longer postulated — they are
    DERIVED by summing the value at the validated script (`Context.ownHash`) over the produced outputs
    / resolved inputs (`valueAtOut`/`valueAtIn`), à la Plutus `valueLockedBy`. Needed enriching
    `Input` with its `resolved : Output` and `Context` with `ownHash : ℍ` (+ inputs as a `List`, +
    `_≟ℍ_` decidable hash equality in the Prelude). So **close/contest value conservation is now fully
    real** (`valHead' ⊇ valHead` over real values), and increment/decrement are real **modulo**
    `depositValue`/`decommitValue`, which stay postulated (concretizable next via the resolved inputs +
    the increment `ref`). The value *arithmetic* (`_+ᵛ_`/`_≤ᵛ_`/`εᵛ`) is still the abstract `Value`
    algebra.
  - **§5.1 init / μHead modelled:** new `initValid` creation predicate — `cid = hash(μHead(seed))`,
    seed spent (`depositSpentOK`), `mintedCount = n+1`, produced Open initial (`v=0`, `η=accUTxO ∅`).
    (Token placement into the head value left abstract.)
  - **§5.2–5.3 deposit/recover (νDeposit) modelled:** `DepositDatum`/`DepositRedeemer`, `recoverValid`
    (post-deadline `t_recover < txValidityMin` concrete; the recovered-outputs hash-equality abstracted
    as `recoveredMatchesDeposited`), and `depositClaimedBy` linking a Claim to its head's increment.
  - Remaining (next): see the canonical "Still open (scoped)" list below. (NB increment/decrement
    LOVELACE value conservation is now extracted + bridged + differentially tested via
    `incRefᵇ`/`decRefᵇ`; only the multi-asset / non-lovelace component stays abstract.)

- **Code-vs-spec (implementation alignment):** tracked separately in
  [`code-spec-discrepancies.md`](./code-spec-discrepancies.md) (off-chain `HeadLogic.hs`) and
  [`agda-haskell-alignment.md`](./agda-haskell-alignment.md) (on-chain Agda ↔ Plutus `νHead`).

## Six-direction consistency audit (Spec ↔ Agda ↔ Haskell, both ways)

A bidirectional audit across all three artifacts (parallel pass per direction). Verdict: **no HIGH
unresolved inconsistency.** Agda→Haskell found every Agda conjunct enforced by the validator;
Haskell→Agda found one real Agda gap (now fixed) plus documented scope boundaries. Findings:

- **AUDIT-1 [fixed]:** §5.4 increment "claimed deposit is spent" (`txOutRef_increment = txOutRef_deposit`)
  is required by the spec and enforced by Plutus (`claimedDepositIsSpent`) but was MISSING from the
  Agda `incrementValid` (the `ref` field was unused). **Fix:** added a concrete `depositSpentOK ctx ref`
  conjunct (`∃ input spending ref`) to `incrementValid`.
- **AUDIT-2 [false alarm]:** an audit pass flagged `headAdaOverhead`/`adaO` preservation as missing from
  Agda. It is NOT missing — the `_⟶⟨_⟩_` rules reuse the same `ada` variable in source and target, so
  preservation is type-enforced (matches spec §5.x `adaO' = adaO` and Plutus `mustPreserveHeadAdaOverhead`).
- **AUDIT-3 [clarified]:** §5.6/§5.7 present the per-case unified-accumulator construction
  (`accUTxO(U')` / `accCombine(accUTxO(U'), η_Δ)`) as if on-chain checks. They are OFF-CHAIN
  constructions authenticated by the multisignature; on-chain (Plutus AND Agda `closeValid`) verifies
  only `msVfy` + `(η')# = hash(η')` (and the Initial-case constant). Added an "Implementation note
  (accumulator construction)" to §5.6 making this explicit. So Agda and Haskell are consistent here.
  (The HeadLogic snapshot-creation comments `η ← combine(U)` accurately describe that off-chain
  construction; the close/contest-*posting* comments are the separate cosmetic staleness C4 below.)
- **AUDIT-4 [clarified]:** the §7 security model's `signHonest` discipline (sign the snapshot one above
  your own confirmed, extending it, ≤1 per number) is a faithful but more-explicit statement of §6.2's
  operational snapshot regime. Added a "Modelling note (honest signing discipline)" to §7.
- **AUDIT-5 [by-design]:** close/contest head-value check is `valHead' ⊇ valHead` in BOTH the spec
  (§5.6 prose) and the Agda (`≤ᵛ`); Plutus enforces the stronger `==`. Stronger impl = safe; no change.
- **Documented scope boundaries (no fix, by design):** Deposit/Recover validators (νDeposit) and the
  init minting policy are not in the Agda (on-chain head state machine first); off-chain `_handles_↝_`
  is illustrative (the §6.4 figure is normative); the off-chain `requireValidAccumulatorSize` DoS bound
  and deposit lifecycle are protocol-liveness, not consensus-safety. KZG is abstracted (laws only).
- **Reaffirmed known gaps (tracked in `code-spec-discrepancies.md`, NOT changed here):** impl-C2
  (rollback does not restore full off-chain state history Ω — pre-existing, acknowledged in
  `State.hs`), and impl-C3 (close/contest rely on the unchecked invariant
  `confirmedSnapshot.version ∈ {version, version-1}`; holds today, recommend a runtime assertion in a
  focused follow-up rather than bundling here). NB tag collision: these IMPLEMENTATION-alignment
  C2/C3 are unrelated to the formalisation-deepening "C2" (the close contestation-deadline conjunct,
  DONE/bridged/tested above) and to the "C3" mechanize-spec⇒Plutus item in the canonical list below.

## Security review (deposit/recover + increment value conservation)

A targeted theft/lockout/coverage pass over the three artifacts (Typst spec, Agda, extracted
reference checker), prompted by three HIGH deposit-theft hypotheses. All three hypotheses are
REFUTED with quotable evidence; one genuine (safe) spec/impl modeling gap surfaced (AUDIT-6).

- **Cross-head deposit claim [refuted on-chain; NOT enforced in Agda]:** `deposit.ak` Claim gates on
  `expect_increment_redeemer(self, datum.head_id)`: the deposit datum's `head_id` (a PolicyId) must
  match a tx input holding that head's `hydra_head_v2` NFT, spent with the Increment redeemer. A
  deposit cannot be claimed into a different head (different μHead currency symbol). SUPERSEDED (this
  earlier note said the Agda did NOT model it): as of the deepening pass `depositClaimedBy (cid ≡ hcid)`
  is now LIVE — a type-encoded conjunct of `claimTxValid` (which conjoins νHead `incrementValid` with
  νDeposit `claimValid`, OnChain.lagda.typ §5.2). `incrementValid` itself still enforces only
  `depositSpentOK ctx ref` (head-side); the cid-binding lives in `claimValid` (deposit-side), as on
  chain. It remains type-encoded only — NOT bridged or differentially tested (see the Claim-arm item in
  the canonical "Still open (scoped)" list below).
- **Recover destination pinning [refuted]:** `deposit.ak recover_outputs` requires
  `hash_tx_outs(take n outputs) == hashPreSerializedCommits(datum.commits)`; each committed
  `preSerializedOutput` is the full serialised `Output` (address + value + datum), so the recovered
  outputs are byte-pinned to the original owner's address. Modeled by `recoveredMatchesDeposited` +
  the after-deadline bound `tRec < validity.lo`.
- **Multiple-deposit value siphon [refuted]:** `Head.hs mustPreserveValue` requires
  `headInValue <> totalNonHeadInputValue == headOutValue`, where `totalNonHeadInputValue` sums the
  value of ALL non-head SCRIPT inputs (deposits are script inputs; pub-key/fee inputs excluded). The
  exact equality forbids routing any extra spent deposit to an attacker-controlled output: every
  non-head script input's value must land in the head output. No external theft vector.
- **AUDIT-6 [SUPERSEDED — see "AUDIT-6 [RESOLVED]" in the deepening section below]:** originally the
  Agda `incrementValueOK` summed only the SINGLE redeemer-referenced deposit. It now sums `depositsValue`
  (all νDeposit-script inputs), so this gap is closed; the text here is kept only for provenance.
  NB even the fix is a SUBSET of Plutus's check: Plutus `totalNonHeadInputValue` sums every non-head
  SCRIPT input (`isScriptInput`), whereas `depositsValue` sums inputs at the νDeposit script only — a
  strictly weaker (abstain-safe) condition, since a non-deposit script input would be counted by the
  validator but not the model.
- **Concurrent-deposit dilution → potential lockout [upstream, out of scope, traced; NOT theft]:**
  `mustPreserveValue` forces ALL non-head script-input value INTO the head output, while
  `checkSnapshotSignature` binds only the accumulator hash and `claimedDepositIsSpent` checks only the
  ONE redeemer-referenced deposit. So a malicious participant can spend a second pending same-head
  deposit (its Claim passes: same `head_id`, before deadline) and on-chain validation forces that
  value into the head UNCREDITED (no accumulator entry). Traced consequences:
  - NOT theft: increment `mustPreserveValue` (==), close `mustPreserveHeadValue`, and fanout
    `mustConserveValue` (==, outputs must be accumulator members via the KZG subset proof) together
    forbid extracting the surplus to any party. The attacker gains nothing.
  - Potential LOCKOUT (liveness): the off-chain `observeIncrementTx` reads only the FIRST deposit
    input (`findTxOutByScript`), so honest nodes never credit the extra deposit; the head value stays
    permanently above its accumulator; fanout `mustConserveValue` can then never balance (the surplus
    is not an accumulator member) → the head may be impossible to fan out, locking funds (the extra
    depositor's, and potentially everyone's). Requires a malicious participant; costs nothing if a
    victim's pending deposit is reused. A cooperative corrective decrement MIGHT recover, untraced.
  Production behaviour, NOT introduced by this PR; a LIVENESS property (deferred P3). The single-deposit
  Agda model abstracts it away (see AUDIT-6). Recommend upstream confirmation (known accepted risk vs
  fix: e.g. an on-chain "exactly the snapshot's deposits are consumed" / value-matches-accumulator-delta
  check at increment).
- **Coverage:** init (μHead), increment, decrement, close, contest, fanout, partial/finalPartial
  fanout, recover are modeled. abort/commit/collectCom are NOT modeled (this coordinated-head variant
  inits directly to Open with incremental commits via deposit/increment; intent worth confirming).
- **Lockout:** deposits are recoverable after the deadline (Recover, byte-matching the committed
  outputs); before the deadline only Claim (needs an increment). No permanent lockout, only the
  inherent pre-deadline wait. Known trust boundary unchanged: the off-chain PT key-hash check is a
  MEDIUM boundary mitigated by the honest quorum (see security-formalisation-plan.md).

## How discrepancies are caught going forward

- `agda` type-checks every `.lagda.typ` (build fails on type errors).
- `spec/check-refs.sh` (run by `build.sh`) verifies the cited transition-rule names and that the
  head-state **diagram** matches the Agda `_⟶⟨_⟩_` relation.
- The known gap: prose math and predicate-level conditions are only checked once *encoded* in Agda
  — see section D for what remains.

## Formalisation deepening (remediation of the wholistic-review findings)

A pass closing several review findings. All keep `nix build .#spec` green and the hydra-tx differential
suite at 15/15; the Haskell workspace builds `-Werror` clean.

- **AUDIT-5 [RESOLVED]:** close/contest value preservation strengthened from `headValueIn ≤ᵛ headValue`
  (containment) to `≡` (exact), matching Plutus `mustPreserveHeadValue`. Prose updated to `=`.
- **AUDIT-6 [RESOLVED]:** `incrementValid` value conservation now uses `depositsValue` = the value at
  the νDeposit script (`Context.depHash`) summed over all spent deposit inputs, instead of the single
  redeemer-referenced deposit. The differential `incRefVerdict` correspondingly sums every deposit
  input (`findTxOutsByScript`), so the multi-deposit siphon mutation is now caught rather than
  abstained. NB this is a SUBSET of Plutus `totalNonHeadInputValue` (which sums every non-head
  *script* input via `isScriptInput`, not only νDeposit-script inputs) — a strictly weaker,
  abstain-safe condition, not exact parity.
- **Deposit→head binding [RESOLVED, correctly placed]:** added `claimValid` (νDeposit Claim path):
  `depositClaimedBy` (deposit datum's `cid` ≡ the claimed head's `cid`, now LIVE, mirroring deposit.ak
  `expect_increment_redeemer`) + the Claim deadline (`txValidityMax ≤ tRecover`, deposit.ak
  `before_deadline`). These are νDeposit checks, NOT head-validator checks, so they live in `claimValid`,
  not `incrementValid` (which correctly checks only `depositSpentOK` + value + sig, per Head.hs).
- **Value algebra [DEEPENED]:** `Prelude` now postulates the commutative-monoid + partial-order laws
  for `_+ᵛ_`/`_≤ᵛ_`/`εᵛ` (assoc, comm, identities, ≤ᵛ refl/trans/antisym, +ᵛ-monoˡ) -- the algebra the
  value-conservation predicates reason over. (NB quantities are ℤ, so `a ≤ᵛ a +ᵛ b` is deliberately NOT
  a law: values may be negative.)
- **Security model [DEEPENED]:** `Reflects` constructed (not assumed) via a `finalize` step + the
  per-call `ηEq` hypothesis; `msgOf` made content-derived (`snapMsg`); the unused `snapMsg-inj`
  postulate deleted; and `AggVerified`/`ms-unforgeable` made system-relative (`aggSigOf : System →
  Snapshot → AggSig`) to remove the confirmation-layer model-vacuity (so an execution can genuinely
  confirm past genesis). See `security-formalisation-plan.md`.
- **Security model [DEEPENED further]:** seen-sets for full §7 Soundness `T̃ ⊆ ⋂ honest seen` (A3, via
  `sigSeen-inv` + a `signHonest` seen guard + `see` step); Consistency extended to once-honest-then-
  corrupt parties (A5, `consistency-uncorrupted` via `confCert-all`); aggregate unforgeability now a
  DERIVED theorem factored through per-signature EUF-CMA `sigUnforge` + n-of-n decomposition `aggSound`
  (A2). All adversarially verified.
- **Fanout value conservation [DEEPENED]:** `fanoutValueOK`/`partialFanoutValueOK` were fully postulated
  `Context → Set`; now CONCRETE — `headValueIn ≡ Σ(first m outputs) +ᵛ burnedValue +ᵛ ada` (full/final,
  mirroring Plutus `mustConserveValue`) and `headValueIn ≡ headValue +ᵛ decommitValue m` (partial). Only
  the burned-token VALUE (`burnedValue`) stays abstract (mint multiset not modelled).
- **νDeposit binding made live:** `claimValid`/`depositClaimedBy` were dead; added `claimTxValid`
  (conjoins νHead `incrementValid` with νDeposit `claimValid` for the claimed deposit), so the cid-binding
  + Claim deadline are type-enforced conjuncts of a valid claim tx. NB type-ENCODED, not yet bridged or
  differentially tested (`deposit.ak` remains a hand-reviewed coverage boundary).
- **Close deadline differential [UNMOCKED, C2]:** the close-deadline conjunct was previously absorbed
  into the reference's mock `Ops`. Now `Closedᶜ` carries `tfinalC` and `closeRefᵇ` takes a `validityHi`
  param and checks `tfinalC ≡ validityHi + cp` (builtin `_==_`/`_+_`, since POSIXTime ms is far too
  large for the structural `_==ᵇ_`). The bridge `closeValid→ref` discharges it from `closeDeadlineOK`
  (the 2nd `closeValid` conjunct) via `==-sound`, in all four close cases. The differential
  (`CloseDifferential.hs`) supplies the real datum deadline and the real tx upper validity bound
  (`posixFromUTCTime ∘ slotNoToUTCTime systemStart slotLength`, proven bit-identical to the validator's
  `makeContestationDeadline`/`tMax` under the fixture `fixedEpochInfo`), abstaining when the bound is
  infinite. A deterministic 1ms-drift test shows BOTH reference and validator reject (non-vacuity).
  Adversarially verified: no false-reject scenario exists under the linear fixture epoch map.
- **νDeposit Recover differential [NEW, cdeposit]:** added an extractable `recoverRefᵇ` (deposit.ak
  Recover arm) checking the decidable after-deadline conjunct `tRecover < validityLo` (builtin `_<_`,
  POSIXTime ms), bridged from `recoverValid`'s `tRec < lo` conjunct (`recoverValid→ref` via a new
  `<ᴮ-sound` postulate, same trust tier as `==-sound`); the recovered-outputs hash equality stays the
  injected mock. New `DepositDifferential.hs` hydra-tx suite (`recoverRefVerdict` reads the deposit
  datum deadline + the tx lower validity bound, abstains if no finite lower bound) with a deterministic
  equal-boundary demo (lower bound pulled onto the deadline slot → BOTH reference and validator reject,
  `DepositPeriodNotReached`). Adversarially verified: no false-reject; lower-bound slot→POSIXTime is
  bit-identical to the ledger's translation. The Claim arm (`claimTxValid`) stays type-encoded only.
- **μHead init token-count differential [NEW, D2 partial]:** added an extractable `initRefᵇ` checking
  the decidable μHead conjunct `mintedCount == suc n` (exactly n+1 tokens minted: 1 ST + n PTs, the
  policy's `checkNumberOfTokens`), bridged from `initValid`'s 3rd conjunct (`initValid→ref` via
  `==-sound`, builtin `_==_` so a large-quantity mutation can't hang the structural `_==ᵇ_`). New
  `InitDifferential.hs` hydra-tx suite (`initRefVerdict` reads `n` from the head datum + sums the unique
  mint policy's quantities) with a deterministic extra-PT demo (mint n+2 → BOTH reference and policy
  reject, `WrongNumberOfTokensMinted`). Adversarially verified: `reference_verdict ≡ validator's
  checkNumberOfTokens` structurally (same count, same `n`), so no false-reject. SCOPE: only the token
  COUNT is modelled; token PLACEMENT (ST/PT into the head output) + seed-spent + datum binding need the
  multi-asset Value-map lookup the spec deliberately abstracts over, so they stay injected/mock (the
  spec does not expose Axiom.Set.Map's lookup/membership on `Value = (CId × Token) ⇀ Quantity`).
- **Readability refactor [NEW]:** the 11 on-chain validity bundles in `OnChain.lagda.typ` were anonymous
  `×`-products destructured positionally in `ReferenceBridge` (`(close , dl , _ , ini , _ , …)` — a reader
  had to count commas). They are now RECORDS with one named field per conjunct (`CloseValid`,
  `IncrementValid`, …, `ClaimTxValid`); the bridge projects `b .deadlineOK` / matches `record { step =
  close ; … }`. Paper notation (η/cp/v/s/cid/…) is deliberately KEPT (it is the rendered-spec/paper symbol
  set). Adversarially verified semantics-preserving (every conjunct retained, field types identical, no-⊥
  records uninhabited for wrong shapes via the `step` field). `Reference.agda` field order untouched (FFI
  fence → no MAlonzo regen). The "Reading the Agda" primer's bundle paragraph was updated to match. A
  follow-up pass also record-ified the §7 `Security.lagda.typ` proof internals: the `Inv` invariant
  (6-way `×` with up-to-5-deep `proj` towers → `record Inv` + `Inv.field` extractors), its `sigChain`
  predecessor witness (`record PredecessorWitness`, parameterised by the `Certified` predicate so it
  carries across non-`sigs` steps), and the `Reflects` Σ; plus binder renames (`sigDed→sigDedup`,
  `sgn→signer`, the `cert-nest-aux` glyph-soup, one shared `trueNotFalse`). Adversarially verified
  semantics-preserving (the 6 `Inv` fields are pairwise-distinct propositions, so the green typecheck
  rules out any field swap). `HoldsAt`/`Soundness` Σ-results left as-is (they are §7 theorem statements).
- **Helper-file split [NEW]:** boilerplate factored into typecheck-only `.agda` helpers (imported by
  `Main`, NOT rendered): (1) `RefReflection` — the Bool-check⇄proposition reflection lemmas
  (`==ᵇ-refl`/`≡→==ᵇ`/`≤→≤ᵇ`/`<→<ᵇ`/`&&-intro` + the `==-sound`/`<ᴮ-sound` postulates) moved out of
  `ReferenceBridge`, which now reads as just the `*Valid → ref` correspondence; (2) `SecurityProofs` —
  the §7 machine-checked PROOF TERMS (the `invariant` + L1/L2/L3 corollaries,
  `consistency`/`soundness`/`completeness`, the once-honest-then-corrupt extension, `sigSeen-inv`, and
  the `reflects` bridge) moved out of `Security.lagda.typ` (944 → 611 lines). The rendered §7 now shows
  the model + the property STATEMENTS + the human-readable proof prose; the proof terms are still
  verified by `nix build` (a prose note points to the module). The literate §5 OnChain accessors/value
  helpers were left inline (they are normative value-computation, entangled with the rendered datum).
- **Readability + honesty docs:** added a "Reading the Agda (for Haskell programmers)" primer
  (@sec:reading-agda) that, among other idioms, flags `postulate` as ASSUMPTION (the rendered PDF does
  not otherwise distinguish assumed from proved); added a §7 "Scope" note stating plainly that the
  safety proofs and the on-chain validity bundles meet only at datum-field accessors (`finalize` admits
  an arbitrary datum), that non-vacuity is a meta-level argument, and that νDeposit/off-chain are
  coverage boundaries.
### Still open (scoped) — CANONICAL outstanding-items list

This is the single authoritative list of everything outstanding across the PR; the other spec docs
(`security-formalisation-plan.md`, `agda-haskell-alignment.md`, `code-spec-discrepancies.md`) hold the
detail for their respective items and should cross-reference here rather than maintain parallel lists.
Tag-collision note: the formalisation-deepening "C2" (close deadline, DONE) and the implementation
"impl-C2/impl-C3" below are different numbering schemes — descriptive ids are used here to avoid clash.

**A. Extractable reference / bridge / differential — conjuncts still injected (`const True`):**
- **contest conditional deadline UPDATE** (`C3-contest-deadline`): *done* — both the posted-before
  conjunct (`validityHi ≤ᴮ tfinal`) AND the conditional update rule `contestDeadlineOK`
  (`tfinal' == if (lenOut ==ᵇ n) then tfinal else tfinal + cp`) are now in `contestRefᵇ`, bridged via
  `contestValid→ref` (the update reflected from the stdlib `⌊ length C' ≟ n ⌋` to the structural
  `_==ᵇ_` using the new `¬→==ᵇfalse` lemma) and differentially tested (`MutateValidityPastDeadline`).
- **fanout value/accumulator conjuncts** (`fanout-conjuncts`): accumulator membership/exclude remain
  injected. Fanout VALUE conservation (`fanoutValueOK`) is **not a sound differential candidate** and is
  left as a documented boundary: the model term `headAda d` (the datum's `ada : Value` field) has no
  faithful transaction-level counterpart (real Plutus datums do not carry the UTxO value), unlike the
  increment/decrement deposit/decommit values which are real tx outputs — mirroring it in the reference
  would force the differential to fabricate `headAda`. (DONE earlier: `0 < m`, `burnAllTokensOK`,
  after-deadline.) The value conservation IS type-concrete and proved in the spec bundle itself.
- **close bounded-validity** (`close-bounded-validity`): *done* — `closeRefᵇ` checks `hi − lo ≤ᴮ cp`
  (the bundle's `validityBounded`), bridged via `closeValid→ref` (`≤ᴮ-sound`) and differentially tested
  (the close differential supplies the tx lower bound).
- **νDeposit Claim own-head binding** (`claim-differential`): *done* — the before-deadline conjunct AND
  the head-id binding (`depositCid == headCid`, the head-id half of `expect_increment_redeemer`) are in
  `claimRefᵇ`, bridged via `claimValid→ref` (the cid equality from `cong cidToNat` over
  `claimedByOwnHead`; only a typecheck-only `cidToNat : ℍ → ℕ` encoding postulate, **no injectivity
  axiom** — the one-directional bridge needs only `cong`). Differentially tested in `DepositDifferential`
  (the cross-head deposit mutation in `genIncrementMutation` exercises it). What REMAINS mocked is the
  Increment-redeemer coupling half of `expect_increment_redeemer`.
- **multi-asset value conservation** (`lovelace-vs-multiasset`): *partially closed* — increment/decrement
  value conservation is now checked on TWO additive projections, `adaOf` (lovelace) AND the new
  `nonAdaOf` (total non-ada token quantity), both bridged from the same value equation and exercised by a
  differential demo (`tokenSiphonIncrementTx`: a pure native-token siphon that passes the old ada-only
  check, is rejected by the new non-ada conjunct, and rejected by the validator). REMAINING: per-token
  granularity (the total-quantity projection aliases distinct token sets with the same total — full
  per-asset checking needs the abstract `Value` map exposed); fanout value conservation stays the
  `headAda` boundary above. This is the pilot un-mock of a previously-`const True` value conjunct.
- **participant signature** (`signedByParticipant`): *done* — was a BARE postulate `Context → Set`; now a
  structural predicate `∃ kh, signerKeyHash ctx kh ∧ quantityOf (headValue ctx) (cid,kh) ≡ 1` (the PT-
  presence half via the new per-asset projection `quantityOf`, trust family of `adaOf`/`nonAdaOf`; the
  signer-naming half stays the thin `signerKeyHash` postulate because the opaque set-theory `List-Model`
  blocks `ℙ`-membership instance resolution — no spec code queries a `ℙ` set). Threaded `cid` through the
  four bundles (close/contest/increment/decrement). Pulled OUT of the per-tx crypto mock into the shared,
  fully-extractable `participantSignedRefᵇ` (overlap of extracted signer key-hashes vs PT names),
  bridged via `participantSigned→ref` (a POSTULATED extraction-faithfulness boundary — heavier than
  `cidToNat` since it's a postulated implication, not a `cong`; same trust family as `==-sound`).
  Differentially wired into `incRefVerdict` (signers from `txExtraKeyWits`, PT names from the head output;
  PT name IS the key-hash, so the encodings match) with a non-vacuity demo (`noSignerIncrementTx`:
  version+value intact, participant conjunct + validator both reject `NoSigners`). The shared checker is
  ready to wire into the contest/decrement/close verdicts too (not yet done). This closes finding E below.
- **μHead token PLACEMENT** (`D2-token-placement`): *done (form (a), count-in-output)* — `InitValid` now
  has `stPlaced : stQty (headValue ctx) cid ≡ 1` and `tokensPlaced : headTokenCount (headValue ctx) cid ≡
  suc n` (two new ℕ value projections `stQty`/`headTokenCount`, trust family of `adaOf`). Together with the
  already-bridged n+1 MINT count this pins that every minted token is placed in the head output: the ST is
  present and the output carries exactly n+1 head-policy tokens. Bridged via `initValid→ref` (both are
  `≡`-of-ℕ-projection conjuncts, discharged by plain `==-sound`, NO new axiom — cleaner than
  `signedByParticipant`, which needed an extraction-faithfulness postulate), and differentially tested
  (`InitDifferential`: `RemovePTsFromHead` is now ASSERTED not abstained, + a `removePTsInitTx` non-vacuity
  demo — mint count n+1 still passes but placement count = 1 ≠ n+1 rejects, validator `MissingPTs`).
  REMAINING (boundary, not form (a)): naming the individual PTs (form (b)) needs the per-party key list,
  which the on-chain `Open` datum abstracts into `hk`/`n`; seed-spent + datum binding stay injected.
- **`depositValue`/`decommitValue` extractors** — *done (already)*: `depositValueAt`/`depositsValue`
  (input-search via `valueAtIn`) and `decommitValue` (output-search via `takeSumᵛ`) are concrete, not
  postulated (done in the increment/decrement value work). The remaining `postulate`s are the abstract
  `Value`-algebra/crypto/accumulator LAWS, not these extractors.

**B. §7 security (`security-formalisation-plan.md` has the detail):**
- **Liveness (P3)** — **DEFERRED / not planned (owner decision, 2026-06):** the temporal/fairness layer
  (`Eventually`/fairness, message delivery, leader rotation, `reqconf → eternal → liveness`) is out of
  scope for the foreseeable future. `Liveness` stays a postulated empty `Set` and the §7 prose lemmas
  remain prose. The illustrative reqDec/deposit/leader-election handler arms feed into this and are
  likewise parked. (Safety — `consistency`/`soundness`/`completeness`/`reflects` — is complete and
  machine-checked independently of P3.)
- **off-chain HeadLogic + `signHonest`** (`A4/D1`): *done* — `signHonest` now FIRES the `reqSn-sign`
  handler (`_handles_↝_`) with a no-in-flight precondition (ŝ = s̄) and bumps the signer's `seenNumber`,
  so the four honest-signing guards are DERIVED, not free premises: applicability-to-U₀ (via the new
  `applyTxs-compose` ledger law + `confApp`), chain-extension (the handler's s = s̄+1 + snap.txs =
  confirmedTxs ++ Δ), one-per-round (new `Inv.signNumBound`), and only-seen (new `Inv.sigSeen` + the
  Δ-observed input). All safety theorems (`consistency`/`soundness`/`completeness`/`reflects`) are
  unchanged in statement; the only new assumption is the one ledger law. Residual: the full
  reqDec/deposit/leader-election handler arms remain illustrative (liveness-relevant, deferred to P3).
- **multisignature refinement**: a fully concrete `aggSigOf` from individual signatures + a
  `combine`/`verify` algebra (safety-cosmetic; the abstract `msVfy`/unforgeability suffices for the proofs).
- **definition-of-done residual**: replace the placeholder `postulate`s with the real `Reachable`/fair-trace
  statements once P3 lands — **gated on P3, therefore also deferred.**
- *Irreducible axioms (NOT work, by design):* `ms-unforgeable` (factored into `aggSound`+`sigUnforge`),
  ledger `applyTxs`/nil/compose (the `compose` law added by A4/D1 to derive the honest signer's
  applicability guard), the bridge glue `outsOf` + per-finalization `ηEq`, and `msVfy`/KZG laws; §7
  model-existence/non-vacuity is a meta-level argument (`msVfy` abstract).

**C. spec ⇒ real Plutus validator (`C3-mechanize`):** the `spec ⇒ validator` link (agda-haskell-alignment
§3) is hand-reviewed prose, not mechanized. Recommended to stay prose.

**D. Implementation alignment (detail in `code-spec-discrepancies.md`):** `impl-C2` rollback does not
restore full off-chain state history Ω; `impl-C3` close/contest rely on the unchecked invariant
`confirmedSnapshot.version ∈ {version, version-1}` (recommend a runtime assertion); `impl-C4` stale
`-- Spec: η ← combine(S.U)` comments (cosmetic).

**E. Coverage boundaries / deferred (not planned work):** `B6` concretizing the accumulator + constraining
η' is **declined** — it would make the Agda STRONGER than the validator (νHead authenticates η via `msVfy`,
not by recomputing `accUTxO`); abstract Value algebra + crypto/accumulator laws stay postulated (KZG not
modelled); finding D (`headSeed` absent from Agda `Open`) and the B-off-2..7 off-chain clarity notes are
documented coverage boundaries (finding E, the Plutus signer check vs Agda `signedByParticipant`, is now
mostly closed — `signedByParticipant` is structural `∃ signer holding a PT`, matching
`mustBeSignedByParticipant`; only the exact-cardinality nuance for specific txs remains a boundary);
`abort`/`commit`/`collectCom` are not modelled (this variant inits directly to `Open` — confirm intent);
the concurrent-deposit dilution/lockout is an upstream liveness concern (deferred, confirm upstream).

**F. Agda↔prose discrepancy — RESOLVED (owner decision, 2026-06):** `closeValid`/`contestValid`
type-enforce `headValueIn ctx ≡ headValue ctx` (value preserved EXACTLY, matching Plutus
`mustPreserveHeadValue`'s `==`), but the rendered prose for close (§5.6) and contest (§5.7) had said
`valHead' ⊇ valHead` (superset/monotone). The owner confirmed the prose should match the Agda and the
validator, so both §5.6 and §5.7 prose were tightened to `valHead' = valHead` ("preserved exactly").
