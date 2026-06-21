# D4 — Plan: formalising the §7 security properties in Agda

Status: **P0 complete. P1-real DONE: the §7 safety core is now fully DERIVED from a signature model,
not assumed.** `Security.lagda.typ` was rebuilt: the global `System` records each party's individual
signatures (`sigs : List (Fin parties × Snapshot)`, no pre-ordained chain); a snapshot is `Certified`
only once EVERY party signed it (the coordinated head's full multisignature); the step relation
`_⟶ˢ_` has `signHonest` (an honest party signs only an *applicable* snapshot, at most one per number,
**extending its own confirmed snapshot**), `signCorrupt` (a corrupt party signs arbitrarily),
`confirm` (adopt a certified snapshot), and `corrupt`. From these the machine-checked invariant
`invariant` DERIVES all three safety lemmas:

- **L1** (`agree`): two certified snapshots of the same number are equal.
- **L3** (`conf-applicable`): every honest party's confirmed snapshot is applicable to `U₀`.
- **L2** (`confirmed-nest`, via `cert-nest`): confirmed snapshots nest by number, proved by a gap
  induction using L1 + the extend-your-own-confirmed guard.

**Consistency** (`consistency`), **Soundness** (`soundness`) and **Completeness** (`completeness`)
all follow with NO further safety assumption. `confirm` checks the §3.2 aggregate multisignature
via the scheme's verifier `msVfy` (`AggVerified snap = msVfy aggKey (msgOf snap) (sigOf snap) ≡ true`),
and the named **unforgeability** axiom `ms-unforgeable : AggVerified snap → Certified sys snap` bridges
that operational check to "every party signed". The only residual postulates are then the ledger
semantics (`applyTxs`/`applyTxs-nil`), the bridge glue (`outsOf`), `ms-unforgeable` (the irreducible
crypto assumption), and `Liveness` (P3). The whole tree and `nix build .#spec` are green.

This REPLACES the earlier single-confirmed-chain model, which simply *assumed* the agreed chain is
applicable (the entire §7 safety guarantee) as an `Initial` premise. That blanket assumption is gone:
applicability AND agreement AND nesting are now theorems about the protocol's signing discipline.

**The signature model (P1-real substrate).** `System` records each party's individual signatures as
`sigs : List (Fin parties × Snapshot)`. `Signed sys i snap = (i , snap) ∈ sigs`, and a snapshot is
`Certified sys snap = ∀ i → Signed sys i snap` (EVERY party signed it — the coordinated head's full
multisignature). The step relation `_⟶ˢ_`:
- `signHonest` (honest `i` signs `snap`): guarded by `Applicable U₀ (txs snap)` (the reqSn 'wait'
  guard — an honest party signs only an applicable snapshot) and "`i` has not signed any snapshot of
  this number" (one signature per round);
- `signCorrupt` (corrupt `i` signs any `snap`): no guard (the adversary forges nothing *honest*);
- `confirm` (party adopts a `Certified` snapshot as its confirmed snapshot);
- `corrupt` (honest set only shrinks).
`Initial`: no signatures, every party's confirmed snapshot is the genesis (number 0, txs `[]`).

**L3 + L1 are DERIVED (machine-checked invariant `invariant`).** `Inv` bundles three properties,
each maintained across every step: `sigApp` (every honest signature is on an applicable snapshot —
from `signHonest`'s guard), `sigDed` (an honest party signs ≤1 snapshot per number — from
`signHonest`'s guard), and `confApp` (**L3**: every honest party's confirmed snapshot is applicable
to `U₀`). `confApp`'s `confirm` case is the crux and is genuinely derived: the confirmer adopts a
`Certified` snapshot, so it carries the confirmer's OWN signature; the confirmer is honest, so by
`sigApp` that snapshot is applicable. Corollaries: `conf-applicable` (L3), `cert-applicable`
(certified ⇒ applicable, via any honest signer), and `agree` (**L1**: two certified snapshots of the
same number are equal — from `sigDed`, since an honest party signed both). Unforgeability needs no
axiom: `Certified` *means* every party signed, so the honest confirmer's signature is present by
construction.

**Consistency / Soundness / Completeness.** `consistency`: for honest `i, j`, their confirmed sets
are each applicable (`conf-applicable`, derived) and nested (`confirmed-nest`, the L2 assumption), so
the union is the larger applicable set — no conflict. `soundness`: a certified finalized snapshot is
applicable (`cert-applicable`), so `Ufinal = U₀ ∘ (txs snap) ≠ ⊥` — fully derived, NO further
assumption. `completeness` IS `confirmed-nest` (a more-advanced honest party's confirmed set contains
a less-advanced one's).

**L2 — the one remaining honest-core assumption** (`confirmed-nest`, a postulate): honest parties'
confirmed snapshots nest by number. This is the snapshot-extension discipline (a snapshot of number
`suc m` extends the certified snapshot of number `m`). To DERIVE it, `signHonest` must record each
signature's predecessor snapshot (so the extension is checkable against the signer's confirmed
snapshot locally), and the invariant must thread cross-party nesting via `agree`. That operational
step is the remaining P1-real work; everything else above is now derived, not assumed.

**The two Agda halves are linked, and the link is now CONSTRUCTED (not assumed).** `Reflects sys snap`:
the on-chain datum's snapshot number matches `snap` and its accumulator `OC.ηOf` commits
(`OC.accUTxO`) to `U₀ ∘ (txs snap)`. A `finalize` step in `_⟶ˢ_` connects the previously-frozen
`onChain` field to the dynamics (it posts a datum for an `AggVerified` snapshot, carrying its number);
`reflects` then PRODUCES a `Reflects` witness: the conflict-freedom conjunct is derived from
`soundness`, the number match from the finalize witness, and only the accumulator commitment remains
assumed --- supplied as `reflects`'s explicit per-finalization hypothesis `ηEq` (irreducible because
νHead authenticates η via `msVfy`, not by recomputing `accUTxO`). It is a hypothesis, NOT a global
axiom: `finalize` admits any matching-number datum, so a global `∀ sys → … → ηOf ≡ accUTxO (outsOf U)`
would have no model (two finalizations, same final U, different stored η); the finalizer discharges
`ηEq` from the η it committed. `reflect-sound`/`reflect-fanout-⊆` then re-express
Soundness / the fanout-subset fact (`OC.fanoutMembersOK ⇒ outs ⊆ outsOf U`) via the accumulator
soundness law (`accUTxO-∅`/`accVerify-sound`/`accVerify-complete`; KZG itself not modelled, only its
laws). Also: `msgOf` is now `snapMsg (version, number, η#)`, so the verified message depends only on
the snapshot's identifying fields (the signature↔snapshot binding itself is carried by `ms-unforgeable`).

Remaining work: **P3 (Liveness)** --- the temporal/fairness layer (eventual delivery under a network
adversary + head stays open). Also: model seen-sets to complete Soundness's `T̃ ⊆ ⋂ honest seen`;
discharging `ηEq` constructively would need an on-chain accumulator recomputation νHead does not
perform (so it stays a signature-trust hypothesis). L2 (`confirmed-nest`) is DONE (derived, no longer a postulate). The whole thing keeps
`nix build .#spec` green. The §7 security properties are statements about **whole protocol
executions** in the presence of an **adversary**, so they need this execution/adversary model
(distinct from the validator-level type predicates / per-transaction validity bundles).

## P1-real: deriving the agreement premise (the actual Consistency content)

This was the substantive safety work: instead of *assuming* the coordinated head's agreement (the old
`chainTxs` model asserted "the confirmed chain is applicable" as an `Initial` premise and propagated
it trivially), DERIVE it from the protocol's signing/quorum rules. **Status: DONE** (L1, L2, L3,
Consistency, Soundness, Completeness all derived; no safety postulate remains). What was built:

**Model (P1a — done).** Replaced the assumed single chain with a signature model:
1. **Per-party confirmed sets** (`LocalState.confirmed`); `chainTxs` dropped entirely.
2. **Certification + the §3.2 multisignature.** `System` records signatures
   `sigs : List (Fin parties × Snapshot)`; `Certified sys snap = ∀ i → Signed sys i snap` is the
   SEMANTIC condition (every party signed) the proofs reason with. Operationally, `confirm` checks the
   AGGREGATE multisignature via the §3.2 verifier `msVfy`: `AggVerified snap = msVfy aggKey (msgOf snap)
   (sigOf snap) ≡ true`. The named axiom `ms-unforgeable : AggVerified snap → Certified sys snap`
   (the scheme's unforgeability — a verifying aggregate signature implies every party signed) bridges
   the two; `confirm`'s invariant case obtains `Certified` from it. So the model uses the real `msVfy`
   check, and unforgeability is the single explicit crypto assumption rather than something baked in.
3. **Honest-signing rule.** `signHonest` requires the signed snapshot be `Applicable U₀ (txs snap)`
   (the reqSn "wait" guard), that the signer has not signed any snapshot of that number (dedup), AND
   that it is number `suc` of and extends (`txs ⊆`) the signer's own confirmed snapshot (the chain
   discipline that yields L2). `signCorrupt` lets corrupt parties sign arbitrarily.

**Lemmas — status.**

- **L1 — Agreement (DONE, `agree`).** Two certified snapshots of the same number are equal: an honest
  witness signed both (by `Certified`), and signs ≤1 per number (by the derived `sigDed` invariant).
- **L3 — Applicability (DONE, `conf-applicable` / `cert-applicable`).** Every honest party's confirmed
  snapshot is applicable to `U₀`: it is certified, so it carries the (honest) confirmer's signature,
  and honest signatures are only on applicable snapshots (the derived `sigApp` invariant). This is the
  property the old model simply *assumed*; it is now machine-checked in `invariant`'s `confirm` case.
- **L2 — Monotone nesting (DONE, `confirmed-nest` via `cert-nest`).** Honest parties' confirmed
  snapshots nest by number. Derived: the invariant `sigChain` records, for each honest signature, an
  extending certified-or-genesis predecessor one number below (from `signHonest`'s extend-own-confirmed
  guard + `confCert`); `cert-nest` then runs a gap induction (on `number s2 ∸ number s1`), at each step
  descending to the certified predecessor (by L1 the unique snapshot at that number) and composing
  `txs ⊆`; the genesis case is ruled out by `cert-pos` (certified ⇒ number > 0). No predecessor field
  was needed in `System` — the fact is carried as the `sigChain` invariant.
- **Consistency (DONE, `consistency`).** Derived applicability (L3) + nesting (L2) ⇒ the two honest
  confirmed sets' union is the larger, applicable, set. **Soundness (DONE, `soundness`)** uses only
  L3 (a certified finalized snapshot is applicable). **Completeness (DONE, `completeness`)** is L2.

**Residual assumptions** (no longer including any safety property): the ledger `applyTxs` semantics +
nil law; the bridge glue `outsOf` + the per-finalization `ηEq` accumulator-commitment hypothesis; the
honest-signing discipline in `signHonest`; and the §3.2 multisignature's **unforgeability** -- now
FACTORED (A2) into per-signature unforgeability `sigUnforge` (EUF-CMA) + the aggregation scheme's
n-of-n decomposition `aggSound`, from which the aggregate-level `ms-unforgeable : AggVerified sys snap
→ Certified sys snap` is DERIVED (no longer a postulate). These are the irreducible crypto/ledger
axioms. (A3 added the seen-set discipline used by full Soundness; A5 added `confCert-all` for the
once-honest-then-corrupt Consistency extension `consistency-uncorrupted`.)

**RESOLVED: confirmation-layer model-vacuity.** Previously `ms-unforgeable` was `∀ sys snap →
AggVerified snap → Certified sys snap` with a snapshot-only `AggVerified snap = msVfy aggKey (msgOf snap)
(sigOf snap)`. Because `Certified sys snap` requires every party's recorded signature in `sigs sys`,
instantiating `ms-unforgeable` at a freshly-built empty-signature system proved `¬ AggVerified snap` for
_every_ snapshot, so `AggVerified` was unsatisfiable in every model and the whole confirmation layer
(`confirm`/`soundness`/`reflects`/`finalize`) was model-vacuous (true but never exhibiting an execution
that confirms past genesis). FIX: the verified aggregate is now SYSTEM-RELATIVE. `sigOf : Snapshot →
AggSig` was replaced by `aggSigOf : System → Snapshot → AggSig` (the aggregate assembled from the
signatures the system has recorded on `snap`), and `AggVerified sys snap = msVfy aggKey (msgOf snap)
(aggSigOf sys snap) ≡ true`. The `∀ sys` unforgeability axiom is now sound and NON-vacuous: for a system
missing signatures `AggVerified sys snap` is correctly false (the intended meaning), while for a system
in which every party signed it is satisfiable (model: interpret `aggSigOf`/`msVfy` so the aggregate
verifies iff `Certified sys snap`), so a model with genuine confirmations exists. All call sites
(`confirm`, `finalize`, `Soundness`/`soundness`, `reflects`, the `confirm` invStep case) were rethreaded
through `AggVerified sys snap`; `nix build .#spec` stays green. This was the sharpened "model
disconnected from the handlers/`msVfy`" gap; it is now closed at the abstraction level (a fully concrete
`aggSigOf` from individual signatures + a `combine`/`verify` algebra remains future refinement).

**Remaining (post-P1-real):** the multisignature refinement above (cosmetic for safety), and **P3
(Liveness)**.

## What §7 asks us to prove

The spec (`Security.lagda.typ`, §7) states four properties over a run of the protocol
with `n` parties, an initial UTxO set `U₀` (`Uinit`), the per-party *confirmed*
transaction sets `T̄ᵢ` and *seen* sets `T̂ᵢ`, ledger application `_∘_` (apply txs to a
UTxO set, `⊥` on conflict), the honest-party set `H`, and the final UTxO set
`Ufinal`:

- **Consistency (Head).** ∀ honest `i, j`: `U₀ ∘ (T̄ᵢ ∪ T̄ⱼ) ≠ ⊥` — no two honest
  parties confirm conflicting transactions. *(safety)*
- **Soundness (Chain).** `∃ T̃ ⊆ ⋂_{i∈H} T̂ᵢ : Ufinal = U₀ ∘ T̃ ≠ ⊥` — the final
  on-chain UTxO set is reachable by applying a set of transactions all honest parties
  have seen. *(safety, ties off-chain ↔ on-chain)*
- **Completeness (Chain).** `⋃_{i∈H_cont} T̄ᵢ ⊆ T̃` — every transaction an honest party
  confirmed is included in the fanned-out result. *(safety)*
- **(Oblivious) Liveness (Head).** Under the *liveness condition* (a network adversary;
  the head stays open long enough), a tx entered by an honest party *eventually* is
  confirmed by everyone or is in conflict with everyone's confirmed set. *(liveness)*

## Why it needs new machinery

These quantify over *reachable states / fair executions* and a *bounded adversary*.
Nothing in the current model has: a ledger-application function, a multi-party global
state, a network with in-flight messages, a corruption model, or an execution/step
relation producing traces. Liveness additionally needs a notion of "eventually" (a
temporal / fairness layer). So D4 is a build, not a few predicates.

## Components to build (bottom-up)

1. **Ledger application `_∘_`.** `apply : UTxO → List Tx → Maybe UTxO` (`⊥ = nothing`),
   with conflict = `nothing`. Reuse formal-ledger's UTxO transition (`Ledger.Utxo`) if it
   maps cleanly; otherwise define a thin EUTxO `apply` over the `Output`/`Input` records.
   This underlies *every* property (`U₀ ∘ T ≠ ⊥`, `Ufinal = U₀ ∘ T̃`).
2. **Complete off-chain handler.** Extend `_handles_↝_` (today only `reqTx-pending`) to all
   §6.4 handlers (`reqTx`/`reqDec`/`reqSn`/`ackSn`/`confSn` and the on-chain observations
   `initialTx`/`incrementTx`/…/`closeTx`/`contestTx`/`tick`), *with* the guards
   (`wait L̂∘tx≠⊥`, leader checks, signature accumulation). Derive `T̄ᵢ`/`T̂ᵢ`/`Û` from
   `LocalState`.
3. **Global system state.** `record System` = a vector `Fin n → LocalState`, the on-chain
   `HeadDatum` (+ posted txs), and a network buffer of undelivered `(sender, receiver,
   Message)`. Plus the static `HeadParameters` and an honest/corrupt partition `H ⊆ Fin n`.
4. **Adversary model.**
   - *Network adversary*: may reorder/delay/drop-then-redeliver messages but (for liveness)
     must *eventually deliver* every sent message (a fairness assumption); does not corrupt
     parties.
   - *Active adversary*: additionally corrupts a set `C ⊆ Fin n` (`H = Fin n \ C`); corrupt
     parties may send arbitrary (channel-authenticated) messages.
   Model as the allowed adversary *moves* in the step relation, parameterised by `C`.
5. **Execution / step relation + traces.** `_⟶ˢ_ : System → System → Set` with moves:
   honest party handles a delivered message (via `_handles_↝_`), a tx/observation is
   posted on-chain, time advances (`tick`), adversary delivers/injects. `Reachable =`
   reflexive-transitive closure from an initial `System₀`. A *trace* is a (possibly
   infinite) run; *fair* traces deliver every message eventually.
6. **Derived quantities.** `U₀`, `Ufinal` (from terminal on-chain state), `T̄ᵢ`/`T̂ᵢ`
   (from `LocalState` in a state), `H`, `H_cont` (honest contesters).

## Stating the properties (Agda shapes)

```
Consistency  : (sys : System) → Reachable sys → ∀ {i j} → Honest i → Honest j
             → apply U₀ (T̄ i sys ++ T̄ j sys) ≢ nothing
Soundness    : (sys : System) → Reachable sys → Terminal sys
             → Σ[ T̃ ∈ List Tx ] (T̃ ⊆ ⋂-seen-honest sys) × (Ufinal sys ≡ apply U₀ T̃) × (apply U₀ T̃ ≢ nothing)
Completeness : … (⋃ honest-contesters T̄) ⊆ T̃ …
Liveness     : Fair trace → LivenessCondition trace → HonestEnters i tx trace
             → Eventually (λ sys → tx ∈ ⋂ᵢ T̄ i sys  ⊎  ∀ i → apply U₀ (T̄ i sys ++ [ tx ]) ≡ nothing)
```
`Eventually` over a trace is the temporal/fairness layer (P3).

## Proof strategy (mirrors the spec's sketches)

- **Consistency** (safety): invariant that honest parties never sign conflicting txs (the
  `reqSn` "wait" guard) + "a tx is confirmed only if every honest party signed" ⇒
  `T̄ᵢ ∪ T̄ⱼ ⊆ T̂ᵢ` and `U₀ ∘ T̂ᵢ ≠ ⊥`. Prove by induction over `Reachable` (a state
  invariant). Most tractable — start here.
- **Soundness / Completeness** (safety): connect the on-chain close/fanout (the validity
  bundles already encoded) to the off-chain confirmed sets — the closed snapshot's UTxO is
  `U₀ ∘ (some T̃ seen by all honest)`. Induction over reachable + the close/contest/fanout
  rules.
- **Liveness** (liveness): under fairness (eventual delivery) + head-stays-open, show the
  `reqSn`→`ackSn`→`confSn` round always completes (`lem:reqconf`), then snapshots advance
  unboundedly (`lem:eternal`), so the tx is eventually confirmed or conflicting. Needs the
  `Eventually`/fair-trace layer. Hardest.

## Phasing, milestones, effort

- **P0 — substrate** (`_∘_`, finish `_handles_↝_`, `System`, `Reachable`). *Done.* Enables everything.
- **P1 — Consistency reduction** (state invariant over the single-confirmed-chain model). *Done, but
  assumes the agreement premise.*
- **P2 — Soundness + Completeness** (over the same model). *Done, on the same premise.*
- **P1-real — derive the agreement premise** (signature model + honest-sign rule). *Done:* L1
  (agreement at a number), L3 (applicability of confirmed snapshots) and L2 (cross-number nesting,
  `confirmed-nest` via `cert-nest`) are all machine-checked derivations; Consistency, Soundness and
  Completeness follow with no safety postulate.
- **P3 — Liveness** (build `Eventually`/fairness; prove `reqconf` → `eternal` → liveness). *Open.*

Effort: P0/P1/P2/P1-real done (safety fully derived); P3 large (temporal reasoning). This is
multi-week, research-flavoured work — much larger than the validator predicates.

## P3 (Liveness): what it would take (deferred)

Liveness is categorically different from the three safety properties already proved. Consistency /
Soundness / Completeness are **invariants over reachable states** ("in every reachable state X
holds") — `Reachable` (a finite reflexive-transitive closure) is exactly the right tool and the
proofs are inductions over it. Liveness is **a temporal property over fair, infinite executions**
("*eventually* the tx is confirmed or universally conflicting"). That `eventually` cannot be
expressed against `Reachable` at all (reachability says what *can* happen, not what *must*), and it
is only true relative to a fairness assumption (against a message-dropping adversary it is simply
false). So most of P3 is **new machinery and model enrichment**, not a proof on top of what we have.

What is missing, in order of weight:

1. **A temporal layer (traces + "eventually").** Move from states to runs: a trace `σ : ℕ → System`
   with `σ n ⟶ˢ σ (suc n)` and `Initial (σ 0)` (an ℕ-indexed run is likely simpler in Agda than
   coinductive streams here), then `Eventually P σ = Σ[ n ] P (σ n)`, and probably `leads-to`
   (`P ⇝ Q`) and `Always`.

2. **Fairness / the liveness condition (a hypothesis on the trace).** The honest core is that
   liveness holds only under the network adversary's fairness: *eventual delivery* (every message
   put in `inFlight` is eventually consumed by its recipient — `∀ n m, m ∈ inFlight (σ n) → ∃ k ≥ n,
   "m delivered at k"`), *no corruption* (for oblivious liveness everyone stays honest, so `corrupt`
   is excluded), and *head stays open* "long enough" (no `close` in the window). Encoding `Fair σ`
   is the central new definition and is what powers every "eventually".

3. **Model enrichments (the largest piece).** Our model was built for safety, so its dynamics are
   deliberately permissive/abstract; liveness needs them made operational:
   - *Message delivery is untracked* — `deliver` reads from `inFlight` but does not remove the
     message (at-least-once); fairness needs sent-but-undelivered messages tracked precisely.
   - *No proactive honest sending* — we only have the adversary's `inject`; liveness needs honest
     parties to *generate* the protocol's messages (leader sends `reqSn` after a snapshot confirms,
     every party `ackSn`s, …): "good things happen because honest parties act".
   - *`confirm` is "magic"* — any honest party can currently confirm any chain-aligned snapshot; for
     liveness, confirmation must be the *result* of a completed round (`reqSn` → all `ackSn` →
     `confirm`) so the round can be shown to *complete* under fairness (track collected signatures,
     fire `confirm` when all are in). The single-chain agreement might then be *derived* rather than
     assumed.
   - *No leader schedule* — `eternal` needs `leader(s) = s mod n`, honest leaders, and the next
     leader requesting the next snapshot.
   - *No time / "head open" notion* — the liveness condition references a time window; we have
     neither time nor a `close` move.
   - *`localLedger`/`pending` dynamics and `L̂ ∘ tx`* — the final "confirmed *or* conflicting"
     dichotomy rests on the local-ledger applicability check, which we abstracted (`applyTxs`) and do
     not connect to per-party `pending`/`L̂`.

4. **The three-lemma proof (on top of the above).** Mirrors the §7 sketch: `reqconf` (under fairness
   a requested snapshot is eventually confirmed by everyone — needs eventual delivery + the round
   model), `eternal` (while new txs are issued, every snapshot number `k` is eventually confirmed —
   induction on `k` using `reqconf` + leader rotation + head-stays-open), then liveness itself (the
   party re-issues `reqTx tx`; by `eternal` snapshots advance unboundedly, so `tx` is eventually in a
   confirmed snapshot, or it never applies and is universally conflicting — needs the `L̂ ∘ tx`
   dichotomy).

What we can reuse: the state structure (`System`, the single confirmed chain, the existing moves as
a starting point) and — load-bearing — the **safety results** themselves (liveness arguments lean on
"confirmation never produces a conflict" and on the chain/agreement invariant). So P1/P2 are
premises for P3, not throwaway.

Rough scale: P3 ≈ a small temporal-logic library (traces / `Eventually` / `leads-to`) + a fairness
model + an operational refactor of the message/round/leader dynamics + the three-lemma proof. The
model refactor is the bulk (arguably as much as P0–P2 combined); the temporal/fairness scaffolding
is genuinely new. Sensible sequencing: start with **oblivious** liveness (the party re-enters the tx
after each snapshot, avoiding the wait-queue), build the trace + `Eventually` + `Fair` layer, make
the snapshot round operational, then prove `reqconf` → `eternal` → liveness. Decide the abstraction
level (synchronous rounds vs asynchronous-with-fairness) up front. Honest caveat: just as the safety
proofs rest on a clearly-localized `Initial` premise (the agreed chain is applicable), a liveness
proof rests on a clearly-localized `Fair`/liveness-condition premise — the value is making that
assumption explicit and showing progress genuinely follows from it.

**Status: deferred.** `Liveness` stays `postulate`d (`TODO(D4-P3)`) until this is taken on.

## Reuse / tooling

- `formal-ledger` (`Ledger.Utxo`, `Ledger.Transaction`) for `_∘_` / EUTxO semantics if it
  fits the spec's simplified model; else a bespoke `apply`.
- `abstract-set-theory` for the `T̄`/`T̂` sets and `⋂`/`⋃`/`⊆`.
- stdlib `Data.Vec`/`Fin` for the party vector; `Data.List.Relation` for `⊆`.
- Temporal layer: either an inductive "eventually" over a coinductive trace, or sized
  types; decide in P3.

## Risks / decisions

- **Model fidelity vs effort.** A faithful network+adversary+time model is large; agree the
  abstraction level (e.g. synchronous rounds vs asynchronous-with-fairness) before P0.
- **Liveness is the cliff.** Temporal/fairness reasoning in Agda is involved; P1/P2 (safety)
  deliver most of the value and should land first.
- **Keep the build green.** Each phase ends with `agda` + `nix build .#spec` passing;
  unproven lemmas stay `postulate`d with a `-- TODO(D4)` phase marker until discharged.

## Definition of done

`Security.lagda.typ` states Consistency/Soundness/Completeness/Liveness as the propositions
above over `Reachable`/fair traces, with P1–P2 proved and P3 either proved or reduced to a
small, clearly-marked set of `postulate`d temporal lemmas. The current placeholder
`postulate`s (`Consistency : Set`, …) are replaced by these real statements.
