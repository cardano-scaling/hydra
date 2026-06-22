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
open import Data.Product using (_×_; _,_)

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

-- A Closed head (the only datum shape the full fanout finalises).
data IsClosed : HeadDatum → Set where
  isClosed : ∀ {cid hk n cp v s η C tfin ada}
    → IsClosed (Closed cid hk n cp v s η C tfin ada)

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
fanout-empty-inhabited : ∀ {ctx cid hk n cp tfin ada} {π crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → accVerify (accUTxO ∅ˢ) ∅ˢ π ≡ true
  → fanoutValueOK ctx ada 0
  → FanoutValid ctx (emptyClosed cid hk n cp tfin ada) ∅ˢ 0 π crs
fanout-empty-inhabited aft burn mem val = mkFanoutValid fanout burn mem aft val

-- ── the real thing: non-stuckness grounded in reachability ───────────────────────────────────────
-- The machine REACHES an empty Closed head, AND that reachable state can be finalised (at m = 0). This is
-- the property the safety proofs cannot express: a too-strict fanout rule makes the second component
-- unprovable while leaving every safety theorem intact.
finalize-reachable-empty : ∀ {ctx cid hk n cp tfin ada} {π crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → accVerify (accUTxO ∅ˢ) ∅ˢ π ≡ true
  → fanoutValueOK ctx ada 0
  → Reachable (emptyClosed cid hk n cp tfin ada)
    × FanoutValid ctx (emptyClosed cid hk n cp tfin ada) ∅ˢ 0 π crs
finalize-reachable-empty aft burn mem val =
  reach-empty-closed , fanout-empty-inhabited aft burn mem val

-- ── general coverage: every reachable Closed head can finalise ───────────────────────────────────
-- For ANY reachable Closed head past its deadline, given the abstract fanout antecedents (burn, the
-- members witness for the distributed set, value conservation), the terminal `Fanout` bundle is
-- inhabited. NB the `fanout` step is structurally available from EVERY Closed head (the step relation is
-- premise-free), so reachability is not needed to exhibit the step — what makes a state finalisable
-- rather than stuck is PURELY the bundle's conjuncts. An over-strict conjunct (`0 < m`) is therefore
-- exactly what turns a coverable reachable state into a permanently-stuck one, and this obligation is
-- what fails to typecheck under it (it cannot discharge `0 < m` for the m = 0 finalisation an empty head
-- forces). Reachability is kept in the statement to mark the intended scope (states the machine occupies).
fanout-coverage : ∀ {ctx d outs m π crs}
  → Reachable d → IsClosed d
  → tfinalOf d < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx d
  → fanoutMembersOK (ηOf d) outs π
  → fanoutValueOK ctx (headAda d) m
  → FanoutValid ctx d outs m π crs
fanout-coverage _ isClosed aft burn mem val = mkFanoutValid fanout burn mem aft val
