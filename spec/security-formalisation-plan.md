# D4 — Plan: formalising the §7 security properties in Agda

Status: **planned** (not started). This is the long-tail "D4" item from
`discrepancies-and-fixes.md`. The validator-level (on-chain) checks are already encoded
as type-level predicates and per-transaction validity bundles; the §7 security
properties are different in kind — they are statements about **whole protocol
executions** in the presence of an **adversary**, so they need an execution/adversary
model that does not yet exist in the formalisation.

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

- **P0 — substrate** (`_∘_`, finish `_handles_↝_`, `System`, `Reachable`). Enables everything.
- **P1 — Consistency** (state invariant; pure safety induction). *First real proof.*
- **P2 — Soundness + Completeness** (tie to the on-chain bundles).
- **P3 — Liveness** (build `Eventually`/fairness; prove `reqconf` → `eternal` → liveness).

Effort: P0 medium, P1 medium, P2 medium-large, P3 large (temporal reasoning). This is
multi-week, research-flavoured work — much larger than the validator predicates.

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
