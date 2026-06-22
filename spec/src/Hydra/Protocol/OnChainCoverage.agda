-- On-chain COVERAGE / non-stuckness obligations — the dual of the soundness-only corpus.
--
-- Every other on-chain lemma runs `accepted ⇒ safe`: the `*Valid` bundles are only ever DESTRUCTURED
-- (the ReferenceBridge proves `*Valid ⇒ refᵇ`, read contrapositively as reference-reject ⇒ reject), and
-- the §7 theorems prove `certified ⇒ safe`. Nothing ever proves a `*Valid` bundle is INHABITED for the
-- states that reach it. That is a blind spot: an over-strict conjunct (one stronger than the validator
-- needs) only SHRINKS the accept set, so it can never falsify a soundness theorem — yet it can strand a
-- reachable head forever. The `FanoutValid.outputsPositive : 0 < m` regression (an empty head could not be
-- finalised, its n+1 tokens unburnable) was exactly such a bug; the safety proofs were structurally unable
-- to see it. This module adds the missing direction for the finalize path: a reachability inductive over
-- `HeadDatum` and the obligation that the terminal `Fanout` bundle is inhabited for the reachable empty
-- head (and, generally, for every reachable Closed head given the abstract crypto/value antecedents).
--
-- This is NOT temporal liveness (§7 P3: fairness, message delivery, eventual confirmation). It is an
-- atemporal, one-step EXISTENCE statement — completeness-of-acceptance / non-stuckness — provable with no
-- traces, no network, no honest-majority. Typecheck-only (imports OnChain); imported by Main so the build
-- verifies it. Re-adding `outputsPositive : 0 < m` to `FanoutValid` makes the lemmas below fail to compile
-- (the empty head forces `m = 0`, and `mkFanoutValid` would then demand a term of the empty type `0 < 0`).
module Hydra.Protocol.OnChainCoverage where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Hydra.Protocol.OnChain
open import Data.Product using (_×_; _,_; ∃-syntax)
open import Relation.Binary.PropositionalEquality using (trans; cong)

-- ── reachability over on-chain datums (the missing infrastructure) ───────────────────────────────
-- A datum is reachable if it is the well-formed initial `Open` produced by a valid init (version 0,
-- η = accUTxO ∅, the shape `initValid` forces) or follows from a reachable datum by one protocol step.
-- This is datum-SHAPE reachability over the (context-free) step relation `_⟶⟨_⟩_`; it is exactly the set
-- of on-chain states the machine can structurally occupy.
data Reachable : HeadDatum → Set where
  reach-init : ∀ {cid hk n cp ada}
    → Reachable (Open cid hk n cp 0 (accUTxO ∅ˢ) ada)
  reach-step : ∀ {d r d'}
    → Reachable d → d ⟶⟨ r ⟩ d' → Reachable d'

-- The empty Closed head: a head opened with no committed UTxO (η = accUTxO ∅) and closed on its initial
-- snapshot (v = s = 0, η preserved). It IS reachable — init opens it empty, close-initial keeps it empty.
emptyClosed : ℍ → VKey → ℕ → ℕ → ℕ → Value → HeadDatum
emptyClosed cid hk n cp tfin ada = Closed cid hk n cp 0 0 (accUTxO ∅ˢ) [] tfin ada

reach-empty-closed : ∀ {cid hk n cp tfin ada}
  → Reachable (emptyClosed cid hk n cp tfin ada)
reach-empty-closed = reach-step reach-init (close {ct = closeInitial})

-- ── the minimal catcher: the empty head admits a valid (m = 0) fanout ────────────────────────────
-- Given the enabling guard (past the deadline) and the abstract accumulator/value antecedents (the same
-- crypto boundary the differential mocks), the terminal `Fanout` bundle is INHABITED at m = 0, outs = ∅.
-- The 0-output membership `accVerify (accUTxO ∅) ∅ π ≡ true` is the empty-set instance of the accumulator
-- (`accVerify-complete {∅} {∅}`); we take it as a hypothesis to keep the crypto boundary explicit.
-- HERE IS THE CATCH: re-add `outputsPositive : 0 < m` and `mkFanoutValid` needs a sixth field `0 < 0`,
-- which is uninhabited (`suc _ ≤ 0` matches no `_≤_` constructor) — this lemma then fails to typecheck.
-- The 0-output membership is now DERIVED (the empty set is a member of its own commitment, `accVerify-self`)
-- rather than threaded, so only the genuinely-context antecedents (deadline, burn, value) remain.
fanout-empty-inhabited : ∀ {ctx cid hk n cp tfin ada} {crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → fanoutValueOK ctx ada 0
  → ∃[ π ] FanoutValid ctx (emptyClosed cid hk n cp tfin ada) ∅ˢ 0 π crs
fanout-empty-inhabited aft burn val =
  let π , mem = accVerify-self ∅ˢ
   in π , mkFanoutValid fanout burn mem aft val

-- ── the real thing: non-stuckness grounded in reachability ───────────────────────────────────────
-- The machine REACHES an empty Closed head, AND that reachable state can be finalised (at m = 0). This is
-- the property the safety proofs cannot express: a too-strict fanout rule makes the second component
-- unprovable while leaving every safety theorem intact.
finalize-reachable-empty : ∀ {ctx cid hk n cp tfin ada} {crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → fanoutValueOK ctx ada 0
  → Reachable (emptyClosed cid hk n cp tfin ada)
    × (∃[ π ] FanoutValid ctx (emptyClosed cid hk n cp tfin ada) ∅ˢ 0 π crs)
finalize-reachable-empty aft burn val =
  reach-empty-closed , fanout-empty-inhabited aft burn val

-- ── general coverage: every reachable Closed head can finalise ───────────────────────────────────
-- For ANY reachable Closed head past its deadline, given the abstract fanout antecedents (burn, the
-- members witness for the distributed set, value conservation), the terminal `Fanout` bundle is
-- inhabited. NB the `fanout` step is structurally available from EVERY Closed head (the step relation is
-- premise-free), so reachability is not needed to exhibit the step — what makes a state finalisable
-- rather than stuck is PURELY the bundle's conjuncts. An over-strict conjunct (`0 < m`) is therefore
-- exactly what turns a coverable reachable state into a permanently-stuck one, and this obligation is
-- what fails to typecheck under it (it cannot discharge `0 < m` for the m = 0 finalisation an empty head
-- forces). Reachability is kept in the statement to mark the intended scope (states the machine occupies).
-- A reachable Closed head COMMITTING to a known set V (η ≡ accUTxO V) admits a valid full fanout of V
-- (m ≔ `setSize V`). The membership witness is now DERIVED (`accVerify-self`, V is a member of its own
-- commitment) rather than threaded; only the genuinely-context antecedents (deadline, burn, value) remain.
-- No `0 < m` needed (the full fanout permits m = 0, e.g. an empty head V = ∅). The `η ≡ accUTxO V`
-- "commits-to-set" interface is what the crypto already guarantees; baking it in lets coverage reason.
fanout-coverage : ∀ {ctx cid hk n cp v s C tfin ada V crs}
  → Reachable (Closed cid hk n cp v s (accUTxO V) C tfin ada)
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (Closed cid hk n cp v s (accUTxO V) C tfin ada)
  → fanoutValueOK ctx ada (setSize V)
  → ∃[ π ] FanoutValid ctx (Closed cid hk n cp v s (accUTxO V) C tfin ada) V (setSize V) π crs
fanout-coverage {V = V} _ aft burn val =
  let (π , mem) = accVerify-self V
   in π , mkFanoutValid fanout burn mem aft val

-- ── valid-gated reachability + a structural invariant the shape relation cannot see ─────────────
-- `Reachable` above follows the bare (premise-free) step relation, so it admits states reachable only
-- via INVALID transitions. `Reachableᵛ` instead closes under VALID transitions — each step carries the
-- corresponding `*Valid` bundle — so it is exactly the set of states a real (validating) chain occupies.
-- This is the faithful notion, and it carries the bundle witnesses, which lets us derive invariants the
-- shape relation cannot. (Fanout / final-partial go to `Final`, the terminal state, so they need no
-- constructor here — `Final` is never a source.)
data Reachableᵛ : HeadDatum → Set where
  initᵛ : ∀ {cid hk n cp ada}
    → Reachableᵛ (Open cid hk n cp 0 (accUTxO ∅ˢ) ada)
  closeᵛ : ∀ {ctx hk cid v cp s' d d' ct}
    → Reachableᵛ d → CloseValid ctx hk cid v cp s' d d' ct → Reachableᵛ d'
  contestᵛ : ∀ {ctx hk cid v s tfin d d' ct}
    → Reachableᵛ d → ContestValid ctx hk cid v s tfin d d' ct → Reachableᵛ d'
  incrementᵛ : ∀ {ctx hk cid v d d' ξ s ref}
    → Reachableᵛ d → IncrementValid ctx hk cid v d d' ξ s ref → Reachableᵛ d'
  decrementᵛ : ∀ {ctx hk cid v d d' ξ s m}
    → Reachableᵛ d → DecrementValid ctx hk cid v d d' ξ s m → Reachableᵛ d'
  partialᵛ : ∀ {ctx d d' S m crs}
    → Reachableᵛ d → PartialFanoutValid ctx d d' S m crs → Reachableᵛ d'

-- The machine NEVER reaches an empty `FanoutProgress`: a validly-reached `FanoutProgress` always has a
-- non-empty remaining accumulator (η ≢ G₁ = accUTxO ∅). The only way to reach one is a valid
-- `PartialFanout`, whose `notDoneOK` field IS exactly this fact — so the invariant is immediate once
-- reachability is valid-gated (the bare shape relation cannot prove it: a shape `PartialFanout` step may
-- produce η = G₁). This is also why `FinalPartialFanoutValid.outputsPositive : 0 < m` is SOUND, not
-- over-strict: the remainder a final batch fans out is always non-empty, so m ≥ 1 is satisfiable — the
-- guard only blocks finalising while UTxOs remain to distribute. (Contrast `FanoutValid`, the FULL path,
-- where the empty head forces m = 0 and the guard had to go: see B17.)
progress-nonEmpty : ∀ {cid hk n tfin η ada}
  → Reachableᵛ (FanoutProgress cid hk n tfin η ada) → ¬ (η ≡ G₁)
progress-nonEmpty (partialᵛ _ pv)     = PartialFanoutValid.notDoneOK pv
progress-nonEmpty (closeᵛ _ cv)       with CloseValid.step cv
... | ()
progress-nonEmpty (contestᵛ _ cv)     with ContestValid.step cv
... | ()
progress-nonEmpty (incrementᵛ _ iv)   with IncrementValid.step iv
... | ()
progress-nonEmpty (decrementᵛ _ dv)   with DecrementValid.step dv
... | ()

-- FanoutProgress non-stuckness (the final-partial counterpart of `fanout-coverage`): a reachable
-- FanoutProgress past its deadline admits a valid final batch. The remainder is non-empty (by
-- `progress-nonEmpty`), so the `0 < m` it requires is satisfiable — supplied here as the `pos` premise
-- (the abstract accumulator does not expose the remainder's cardinality to derive it, so it is threaded,
-- as are the crypto/value antecedents). Together with `progress-nonEmpty` this says: the machine never
-- gets stuck mid-fanout — a FanoutProgress is always non-empty AND always finalisable.
-- FanoutProgress non-stuckness, now FULLY DERIVED. A reachable FanoutProgress committing to set V admits
-- a valid final batch fanning out V (m ≔ `setSize V`), and — crucially — its `outputsPositive : 0 < m` is
-- DERIVED, not assumed: `progress-nonEmpty` gives η ≢ G₁, so (with η ≡ accUTxO V and the existing
-- `accUTxO-∅`, by contraposition) V ≢ ∅, hence `setSize-pos` gives 0 < setSize V. So "the machine is never
-- stuck mid-fanout" is a genuine theorem (not a witnessed implication): the non-empty-remainder invariant
-- supplies the positivity the final-partial guard requires. Membership is derived (`accVerify-self`);
-- only deadline/burn/value (context facts) remain threaded.
progress-finalizable : ∀ {ctx cid hk n tfin ada V crs}
  → Reachableᵛ (FanoutProgress cid hk n tfin (accUTxO V) ada)
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (FanoutProgress cid hk n tfin (accUTxO V) ada)
  → fanoutValueOK ctx ada (setSize V)
  → ∃[ π ] FinalPartialFanoutValid ctx (FanoutProgress cid hk n tfin (accUTxO V) ada) V (setSize V) π crs
progress-finalizable {V = V} reach aft burn val =
  let (π , mem) = accVerify-self V
   in π , mkFinalPartialFanoutValid finalPartialFanout burn mem
            (setSize-pos (λ V≡∅ → progress-nonEmpty reach (trans (cong accUTxO V≡∅) accUTxO-∅)))
            aft val
