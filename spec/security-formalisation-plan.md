# D4 — Plan: formalising the §7 security properties in Agda

Status: **P0 substrate complete; P1 (Consistency) PROVED outright** (single-confirmed-chain model).
As of the latest pass,
`Security.lagda.typ` defines the ledger-application operation (`applyTxs`/`Applicable`, with
the `applyTxs-nil` law), the global `System` state (party vectors for local states and the
honest/corrupt partition, the on-chain `HeadDatum`, the in-flight network buffer, `U₀`), a
**concrete** step relation `_⟶ˢ_` with constructors `deliver` (an honest party handles a
delivered in-flight message via `_handles_↝_`), `inject` (network adversary (re)delivers), and
`corrupt` (active adversary corrupts a party), a **concrete** `Initial` predicate (all honest,
nothing in flight, nothing confirmed), and `Reachable`. **Consistency** is a concrete
proposition over `Reachable`; its **base case is proved** (`consistency-base`), and the full
`consistency` is postulated with the inductive (step) case open (`TODO(D4-P1)`).
Soundness/Completeness/Liveness remain abstract (`TODO(D4-P2)`/`TODO(D4-P3)`).

**Confirmation is now modelled.** `_handles_↝_` (in `OffChain.lagda.typ`) gained `ackSn-collect`
(records a party's signature) and `ackSn-confirm` (the round completes: the seen snapshot becomes
the confirmed snapshot `S̄`, with `S̄.T = T̂` and `S̄.s = ŝ`). So a party's confirmed set now
genuinely changes, making the Consistency step case non-trivial. `reqTx-preserves-confirmed`
proves the `reqTx`/`ackSn-collect`/`inject`/`corrupt` moves leave confirmed sets untouched; the
open obligation is the `ackSn-confirm` case, which needs the §7 invariant that confirmation only
fires for a snapshot every honest party signed and whose txs are jointly applicable to `U₀`.

**Consistency is now PROVED OUTRIGHT (no postulate)** via the single-confirmed-chain model.
`System` carries one agreed chain `chainTxs : ℕ → List Data` (cumulative confirmed transactions by
snapshot number); modelling one shared chain — rather than independent per-party confirmed sets —
captures the protocol's agreement guarantee (a snapshot confirms only via a full multisignature, so
every honest party confirms along the same chain). The step relation has a dedicated `confirm` move
whose premise `Snapshot.txs snap ≡ chainTxs (Snapshot.number snap)` keeps confirming parties on the
chain, and `deliver` carries a `confirmed-unchanged` premise (so it covers only the non-confirming
reqTx/ackSn-collect handlers). The carried invariant `Inv` — (1) every chain prefix is applicable to
`U₀`, (2) each honest party's confirmed txs equal the chain at its confirmed number — is proved to
hold at every reachable system (`invariant`, a real safety induction: base unfolds `Initial`;
inject/corrupt leave the chain in place, corrupt via `honest-mono`; deliver keeps the confirmed
snapshot; confirm lands on the chain by its premise). `consistency` is then immediate: a party's
confirmed set is `chainTxs (ŝᵢ)`, so two honest parties' sets are nested prefixes whose union is
`chainTxs (ŝᵢ ⊔ ŝⱼ)`, and that prefix applies to `U₀` by invariant (1). The §7 cryptographic
content is now the explicit `Initial` premise "every prefix of the agreed chain is applicable to
`U₀`", which faithfully encodes "honest parties only ever sign applicable snapshots". A corollary
`confirmed-on-chain` ties the abstract chain back to each party's confirmed transactions.

Remaining work: this closes **P1 (Consistency)** for the coordinated model. Next is P2
(Soundness/Completeness — tie the chain's final state to the on-chain close/fanout bundles) and P3
(Liveness — the temporal/fairness layer). Optionally enrich the model further: derive `chainTxs`'s
applicability from the per-snapshot reqSn validity guard rather than asserting it at `Initial`
(pushing the guarantee one level deeper), tie signatures to snapshots explicitly, and add the
on-chain-posting/tick moves. The whole thing keeps `nix build .#spec` green. This is the long-tail
"D4" item from
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
