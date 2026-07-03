# D4 — Security-properties formalisation: completed; P3 (liveness) deferred

The plan this file used to hold is DONE: the §7 safety properties (Consistency,
Soundness, Completeness, plus the certificate/reflection corollaries and the
lifted handler invariants) are machine-checked in
`src/Hydra/Protocol/SecurityProofs` over the system model in
`src/Hydra/Protocol/Security.lagda.typ`, with the trust base enumerated in
`agda-haskell-alignment.md` (trust ledger) and drift-checked by
`check-trust-ledger.sh`. See `discrepancies-and-fixes.md` for the canonical
outstanding-items list.

The one deliberately-deferred piece is P3 (temporal liveness). The analysis of
what it would take is kept below, verbatim, as the record for whoever picks it
up.

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
