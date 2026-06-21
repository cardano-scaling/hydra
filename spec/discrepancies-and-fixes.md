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
| B13 | close/contest/increment/decrement bundles | Missing the "signed by a participant" check (`keyHash_i ∈ txKeys`); the comments over-claimed "only inhabited for valid txs". | Added `signedByParticipant ctx` (obligation abstracted; head-value/key search not modelled). | fixed |
| B14 | `contestValid` | Missing "posted before deadline" (`txValidityMax ≤ tfinal`) and value-preservation (`valHead' ⊇ valHead`). | Added `ValidityInterval.hi … ≤ tfin` and `headValueIn ≤ᵛ headValue`. | fixed |
| B15 | fanout / partial-fanout / final-partial-fanout bundles | Missing the **anti-theft** checks: `m > 0` (no zero-output batch), `txValidityMin > tfinal` (after deadline), `noMint` (partial fan-out), and value conservation. | Added `0 < m`, `tfinalOf d < ValidityInterval.lo …`, `noMint` (partial), and `fanoutValueOK`/`partialFanoutValueOK` (conservation abstracted). | fixed |

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
  the head/deposit/decommit values in the `Context`) is abstracted by postulated extractors
  (`headValue`, `headValueIn`, `depositValue`, `decommitValue`); replacing those with concrete
  output-search over `Context.outputs` is the remaining refinement.
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
  postulates: only the ledger `applyTxs`/nil, the bridge glue `outsOf`, `ms-unforgeable`, and
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
  - Remaining (next): concretize `depositValue`/`decommitValue`; extend the extractable `Reference.agda`
    + differential test with a real value-conservation `Op` (needs a value representation at the
    MAlonzo boundary); off-chain handler model; P3 Liveness.

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
- **Reaffirmed known gaps (tracked in `code-spec-discrepancies.md`, NOT changed here):** C2 (rollback
  does not restore full off-chain state history Ω — pre-existing, acknowledged in `State.hs`), and C3
  (close/contest rely on the unchecked invariant `confirmedSnapshot.version ∈ {version, version-1}`;
  holds today, recommend a runtime assertion in a focused follow-up rather than bundling here).

## Security review (deposit/recover + increment value conservation)

A targeted theft/lockout/coverage pass over the three artifacts (Typst spec, Agda, extracted
reference checker), prompted by three HIGH deposit-theft hypotheses. All three hypotheses are
REFUTED with quotable evidence; one genuine (safe) spec/impl modeling gap surfaced (AUDIT-6).

- **Cross-head deposit claim [refuted on-chain; NOT enforced in Agda]:** `deposit.ak` Claim gates on
  `expect_increment_redeemer(self, datum.head_id)`: the deposit datum's `head_id` (a PolicyId) must
  match a tx input holding that head's `hydra_head_v2` NFT, spent with the Increment redeemer. A
  deposit cannot be claimed into a different head (different μHead currency symbol). CORRECTION (was
  previously overstated here): the Agda does NOT model this. `incrementValid` enforces only
  `depositSpentOK ctx ref` (the claimed ref is spent); the predicate `depositClaimedBy (cid ≡ hcid)`
  IS defined in OnChain.lagda.typ but is never a conjunct of any validity bundle (dead/documentary
  only). So the deposit→head binding is enforced by the real validator (deposit.ak) but is currently
  unmodelled in Agda. Safe (Plutus is the enforcing oracle), but a faithfulness gap, not a match.
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
- **AUDIT-6 [modeling gap, safe, NOT fixed]:** the Agda `incrementValueOK` models head-value growth
  as `headValueIn +ᵛ depositValue(ref) ≡ headValue` (the SINGLE redeemer-referenced deposit), whereas
  Plutus `mustPreserveValue` sums ALL non-head script inputs. The Agda is a faithful model of the
  honest single-deposit increment and Plutus is strictly stronger (safe), but the Agda value
  conjunct is weaker than the validator: it does not express the all-inputs sum that is what actually
  forbids the siphon on-chain. A full fix needs modeling the input multiset; deferred. (Analogous to
  AUDIT-5's ≤ᵛ-vs-== for close/contest.)
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
  the νDeposit script (`Context.depHash`) summed over ALL spent inputs, mirroring Plutus
  `totalNonHeadInputValue`, instead of the single redeemer-referenced deposit. The differential
  `incRefVerdict` correspondingly sums every deposit input (`findTxOutsByScript`), so the multi-deposit
  siphon mutation is now caught rather than abstained.
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
- **Still open (scoped, not done here):** seen-sets for full Soundness (A3); Consistency over
  once-honest-then-corrupt parties (A5); a fully concrete `aggSigOf`/per-signature unforgeability (A2);
  μHead/token-placement modelling (D2); unmocking the differential deadline checks (C2); constraining
  the accumulator η' (B6); formalising the off-chain HeadLogic + deriving `signHonest` (A4/D1);
  mechanizing spec⇒Plutus (C3, recommended to stay prose); and Liveness (P3).
