```
module Hydra.Protocol.OnChainCoverage where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Hydra.Protocol.OnChain
open import Data.Product using (_×_; _,_; ∃-syntax)
open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥-elim)
open import Data.Nat using (_*_; z≤n; s≤s)
open import Data.Integer using (1ℤ)
open import Data.Nat.Properties using (≤-reflexive; ≤-trans; m≤m+n; +-monoˡ-≤; +-monoʳ-≤; *-monoˡ-≤; +-assoc; +-comm)
open import Data.List.Relation.Unary.Any using (here; there)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (trans; cong; subst; sym)
-- For the rendered bridge flagship (the spec ⇒ extracted-reference statement below).
import Hydra.Protocol.Reference as R
import Hydra.Protocol.ReferenceBridge as RB
```

#import "/template.typ": *
#import "/macros.typ": *

== Machine-checked on-chain properties <sec:onchain-theorems>

The per-transaction conditions of the preceding sections are _soundness_
statements: they are only ever consumed in the direction "accepted
$arrow.r.double$ safe". None of them says that a validity bundle is
_inhabited_ for the states that reach it. That direction matters: an
over-strict condition (one stronger than the validator needs) only shrinks
the accept set, so it can never falsify a soundness theorem, yet it can
strand a reachable head forever (an empty head that cannot be finalised
leaves its $n+1$ state tokens unburnable). This section states the
machine-checked properties that close that gap, together with the safety
invariants and value-conservation theorems of the on-chain state machine.
All of them are proved in the Agda module `OnChainCoverage`
(@agda-appendix shows the statements; the proof terms are typechecked as
part of this document's build but not rendered). None of this is temporal
liveness (fairness, message delivery, eventual confirmation are out of
scope): every statement is an atemporal existence or invariance statement
over the transition relation of @sec:on-chain.

Two reachability notions ground the statements: `Reachable` is datum-shape
reachability over the bare step relation, and `Reachableᵛ` closes under
steps that additionally carry their validity bundles, so it is exactly the
set of states a validating chain can occupy. The contest step of
`Reachableᵛ` is stated on concrete datums so the bundle's contester is tied
to the datum's newly-appended one.

```agda
data Reachable : HeadDatum → Set where
  reach-init : ∀ {cid hk n cp ada}
    → Reachable (Open cid hk n cp 0 (accUTxO ∅ˢ) ada)
  reach-step : ∀ {d r d'}
    → Reachable d → d ⟶⟨ r ⟩ d' → Reachable d'

-- The empty Closed head: opened with no committed UTxO, closed on its initial snapshot.
emptyClosed : ℍ → VKey → ℕ → ℕ → ℕ → Value → HeadDatum
emptyClosed cid hk n cp tfin ada = Closed cid hk n cp 0 0 (accUTxO ∅ˢ) [] tfin ada

reach-empty-closed : ∀ {cid hk n cp tfin ada}
  → Reachable (emptyClosed cid hk n cp tfin ada)
```

```
reach-empty-closed = reach-step reach-init (close {ct = closeInitial})
```

```agda
data Reachableᵛ : HeadDatum → Set where
  initᵛ : ∀ {cid hk n cp ada}
    → Reachableᵛ (Open cid hk n cp 0 (accUTxO ∅ˢ) ada)
  closeᵛ : ∀ {ctx hk cid v cp s' d d' ct}
    → Reachableᵛ d → CloseValid ctx hk cid v cp s' d d' ct → Reachableᵛ d'
  contestᵛ : ∀ {ctx hk cid n cp v s η C tfin ada s' η' kh tfin' ct}
    → Reachableᵛ (Closed cid hk n cp v s η C tfin ada)
    → ContestValid ctx hk cid v s tfin
        (Closed cid hk n cp v s η C tfin ada)
        (Closed cid hk n cp v s' η' (kh ∷ C) tfin' ada) ct kh
    → Reachableᵛ (Closed cid hk n cp v s' η' (kh ∷ C) tfin' ada)
  incrementᵛ : ∀ {ctx hk cid v d d' ξ s ref}
    → Reachableᵛ d → IncrementValid ctx hk cid v d d' ξ s ref → Reachableᵛ d'
  decrementᵛ : ∀ {ctx hk cid v d d' ξ s m}
    → Reachableᵛ d → DecrementValid ctx hk cid v d d' ξ s m → Reachableᵛ d'
  partialᵛ : ∀ {ctx d d' m crs}
    → Reachableᵛ d → PartialFanoutValid ctx d d' m crs → Reachableᵛ d'
```

```
-- Valid-gated reachability embeds into shape reachability: every `*Valid` bundle carries its `step`.
reachableᵛ→reachable : ∀ {d} → Reachableᵛ d → Reachable d
reachableᵛ→reachable initᵛ            = reach-init
reachableᵛ→reachable (closeᵛ r cv)     = reach-step (reachableᵛ→reachable r) (CloseValid.step cv)
reachableᵛ→reachable (contestᵛ r cv)   = reach-step (reachableᵛ→reachable r) (ContestValid.step cv)
reachableᵛ→reachable (incrementᵛ r iv) = reach-step (reachableᵛ→reachable r) (IncrementValid.step iv)
reachableᵛ→reachable (decrementᵛ r dv) = reach-step (reachableᵛ→reachable r) (DecrementValid.step dv)
reachableᵛ→reachable (partialᵛ r pv)   = reach-step (reachableᵛ→reachable r) (PartialFanoutValid.step pv)
```

=== Non-stuckness (coverage)

#theorem(name: "Empty head is finalisable")[
  The machine reaches an empty `Closed` head (init opens empty, close-initial
  keeps it empty), and that reachable state admits a valid $m = 0$ full
  fan-out: given the context facts (past the deadline, the $n+1$ tokens
  burned, value conserved), the terminal `Fanout` bundle is inhabited, with
  the $0$-output membership witness derived from the accumulator laws
  (@agda-appendix: `fanout-empty-inhabited`, `finalize-reachable-empty`).
] <thm:empty-finalisable>

An `outputsPositive : 0 < m` conjunct on `FanoutValid` would make these
lemmas fail to compile (the empty head forces $m = 0$), so the build itself
now rejects re-introducing that over-strict guard, which is exactly the
defect class this direction exists to catch.

```agda
fanout-empty-inhabited : ∀ {ctx cid hk n cp tfin ada} {crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → fanoutValueOK ctx ada 0
  → ∃[ π ] FanoutValid ctx (emptyClosed cid hk n cp tfin ada) 0 π crs
```

```
fanout-empty-inhabited aft burn val =
  let π , mem = accVerify-self ∅ˢ
   in π , mkFanoutValid fanout burn mem aft val
```

```agda
finalize-reachable-empty : ∀ {ctx cid hk n cp tfin ada} {crs}
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (emptyClosed cid hk n cp tfin ada)
  → fanoutValueOK ctx ada 0
  → Reachable (emptyClosed cid hk n cp tfin ada)
    × (∃[ π ] FanoutValid ctx (emptyClosed cid hk n cp tfin ada) 0 π crs)
```

```
finalize-reachable-empty aft burn val =
  reach-empty-closed , fanout-empty-inhabited aft burn val
```

#theorem(name: "Reachable heads can finalise")[
  Any reachable `Closed` head committing to a known UTxO set $V$
  ($eta = accUTxO(V)$) admits a valid full fan-out of $V$, provided the
  transaction actually distributes $V$; the membership witness is derived
  from the accumulator laws, so only the genuinely contextual antecedents
  (deadline, burn, value conservation, pays-out-$V$) remain. Likewise, a
  reachable `FanoutProgress` is never stuck: its remaining accumulator is
  provably non-empty (`progress-nonEmpty`), which _derives_ the final
  batch's $0 < m$ requirement rather than assuming it
  (@agda-appendix: `fanout-coverage`, `progress-finalizable`).
] <thm:coverage>

```agda
fanout-coverage : ∀ {ctx cid hk n cp v s C tfin ada V crs}
  → Reachable (Closed cid hk n cp v s (accUTxO V) C tfin ada)
  → distributedOuts ctx (setSize V) ≡ V
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (Closed cid hk n cp v s (accUTxO V) C tfin ada)
  → fanoutValueOK ctx ada (setSize V)
  → ∃[ π ] FanoutValid ctx (Closed cid hk n cp v s (accUTxO V) C tfin ada) (setSize V) π crs
```

```
fanout-coverage {V = V} _ outsEq aft burn val =
  let (π , mem) = accVerify-self V
   in π , mkFanoutValid fanout burn
            (subst (λ z → accVerify (accUTxO V) z π ≡ true) (sym outsEq) mem) aft val
```

```agda
progress-nonEmpty : ∀ {cid hk n tfin η ada}
  → Reachableᵛ (FanoutProgress cid hk n tfin η ada) → ¬ (η ≡ G₁)
```

```
progress-nonEmpty (partialᵛ _ pv)     = PartialFanoutValid.notDoneOK pv
progress-nonEmpty (closeᵛ _ cv)       with CloseValid.step cv
... | ()
-- (contestᵛ produces a Closed datum by construction, so it cannot reach a FanoutProgress index.)
progress-nonEmpty (incrementᵛ _ iv)   with IncrementValid.step iv
... | ()
progress-nonEmpty (decrementᵛ _ dv)   with DecrementValid.step dv
... | ()
```

```agda
progress-finalizable : ∀ {ctx cid hk n tfin ada V crs}
  → Reachableᵛ (FanoutProgress cid hk n tfin (accUTxO V) ada)
  → distributedOuts ctx (setSize V) ≡ V
  → tfin < ValidityInterval.lo (Context.validity ctx)
  → burnAllTokensOK ctx (FanoutProgress cid hk n tfin (accUTxO V) ada)
  → fanoutValueOK ctx ada (setSize V)
  → ∃[ π ] FinalPartialFanoutValid ctx (FanoutProgress cid hk n tfin (accUTxO V) ada) (setSize V) π crs
```

```
progress-finalizable {V = V} reach outsEq aft burn val =
  let (π , mem) = accVerify-self V
   in π , mkFinalPartialFanoutValid finalPartialFanout burn
            (subst (λ z → accVerify (accUTxO V) z π ≡ true) (sym outsEq) mem)
            (setSize-pos (λ V≡∅ → progress-nonEmpty reach (trans (cong accUTxO V≡∅) accUTxO-∅)))
            aft val
```

=== Safety invariants of reachable states

The coverage statements above feed the bundle conditions to a constructor,
so they are agnostic to the conditions' definitions. The invariants below
instead _consume_ the structural premises of the transition relation across
a reachable run: corrupting a premise breaks the corresponding proof at
compile time.

#invariant(name: "No double contest, no resurrection")[
  The contester list of any reachable head is duplicate-free (each contest
  step's freshness premise $keyHash in.not "contesters"$ is exactly what
  discharges the induction), and a head that has fanned out is terminal: no
  transition leaves `Final` (@agda-appendix:
  `reachable-contesters-distinct`, `final-is-terminal`).
] <inv:contest-final>

```agda
contestersOf : HeadDatum → List ℍ
contestersOf (Closed _ _ _ _ _ _ _ C _ _) = C
contestersOf _                            = []

-- No-duplicate predicate: each element is absent from the tail.
Distinct : List ℍ → Set
Distinct []       = ⊤
Distinct (x ∷ xs) = ¬ (x ∈ˡ xs) × Distinct xs

reachable-contesters-distinct : ∀ {d} → Reachable d → Distinct (contestersOf d)
```

```
reachable-contesters-distinct reach-init                              = tt
reachable-contesters-distinct (reach-step r increment)               = tt
reachable-contesters-distinct (reach-step r decrement)               = tt
reachable-contesters-distinct (reach-step r close)                   = tt
reachable-contesters-distinct (reach-step r (contest kh∉C))          = kh∉C , reachable-contesters-distinct r
reachable-contesters-distinct (reach-step r fanout)                  = tt
reachable-contesters-distinct (reach-step r partialFanoutStart)      = tt
reachable-contesters-distinct (reach-step r partialFanoutStep)       = tt
reachable-contesters-distinct (reach-step r finalPartialFanout)      = tt
```

```agda
final-is-terminal : ∀ {r d'} → ¬ (Final ⟶⟨ r ⟩ d')
```

```
final-is-terminal ()
```

=== Value conservation (no theft)

#theorem(name: "Every transaction accounts for the head value")[
  A fan-out (full or final-partial) fully accounts for the head's input
  value on the ada axis: input ada equals distributed ada plus burned ada
  plus the carried overhead. Close and contest preserve the head value
  exactly on both the ada and non-ada axes, and an intermediate partial
  fan-out splits it exactly between the continuing head output and the
  distributed batch. Each statement consumes its bundle's value conjunct
  through the additivity of the value projections
  (@agda-appendix: `fanout-conserves-ada`,
  `finalPartialFanout-conserves-ada`, `close-preserves-value`,
  `contest-preserves-value`, `partialFanout-conserves-ada`).
] <thm:value-conservation>

```agda
fanout-conserves-ada : ∀ {ctx d m π crs}
  → (b : FanoutValid ctx d m π crs)
  → adaOf (headValueIn ctx)
    ≡ adaOf (takeSumᵛ m (Context.outputs ctx)) + (adaOf (burnedValue ctx) + adaOf (headAda d))
```

```
fanout-conserves-ada {ctx} {d} {m = m} b =
  trans (cong adaOf (FanoutValid.valueOK b))
  (trans (adaOf-+ᵛ (takeSumᵛ m (Context.outputs ctx)) (burnedValue ctx +ᵛ headAda d))
         (cong (adaOf (takeSumᵛ m (Context.outputs ctx)) +_) (adaOf-+ᵛ (burnedValue ctx) (headAda d))))
```

```agda
finalPartialFanout-conserves-ada : ∀ {ctx d m π crs}
  → (b : FinalPartialFanoutValid ctx d m π crs)
  → adaOf (headValueIn ctx)
    ≡ adaOf (takeSumᵛ m (Context.outputs ctx)) + (adaOf (burnedValue ctx) + adaOf (headAda d))
```

```
finalPartialFanout-conserves-ada {ctx} {d} {m = m} b =
  trans (cong adaOf (FinalPartialFanoutValid.valueOK b))
  (trans (adaOf-+ᵛ (takeSumᵛ m (Context.outputs ctx)) (burnedValue ctx +ᵛ headAda d))
         (cong (adaOf (takeSumᵛ m (Context.outputs ctx)) +_) (adaOf-+ᵛ (burnedValue ctx) (headAda d))))
```

```agda
close-preserves-value : ∀ {ctx cid hk n cp v η ada s′ η′ C tfin ct}
  → (b : closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct)
  → (adaOf (headValueIn ctx) ≡ adaOf (headValue ctx)) × (nonAdaOf (headValueIn ctx) ≡ nonAdaOf (headValue ctx))
```

```
close-preserves-value b = cong adaOf (CloseValid.valuePreserved b) , cong nonAdaOf (CloseValid.valuePreserved b)
```

```agda
contest-preserves-value : ∀ {ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct}
  → (b : contestValid ctx (Closed cid hk n cp v s η C tfin ada) (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct)
  → (adaOf (headValueIn ctx) ≡ adaOf (headValue ctx)) × (nonAdaOf (headValueIn ctx) ≡ nonAdaOf (headValue ctx))
```

```
contest-preserves-value b = cong adaOf (ContestValid.valuePreserved b) , cong nonAdaOf (ContestValid.valuePreserved b)
```

```agda
partialFanout-conserves-ada : ∀ {ctx d d′ m crs}
  → (b : PartialFanoutValid ctx d d′ m crs)
  → adaOf (headValueIn ctx) ≡ adaOf (headValue ctx) + adaOf (decommitValue ctx m)
```

```
partialFanout-conserves-ada {ctx} {m = m} b =
  trans (cong adaOf (PartialFanoutValid.valueOK b)) (adaOf-+ᵛ (headValue ctx) (decommitValue ctx m))
```

#theorem(name: "No output fabrication at fan-out")[
  The outputs a fan-out transaction actually pays (the anchored
  `distributedOuts ctx m`, the first $m$ transaction outputs) are a subset
  of the accumulator-committed set $V$: consuming the membership conjunct
  together with the accumulator soundness law, a fan-out cannot distribute
  an output the head did not commit to
  (@agda-appendix: `fanout-distributes-committed`,
  `finalPartialFanout-distributes-committed`).
] <thm:no-fabrication>

```agda
fanout-distributes-committed : ∀ {ctx cid hk n cp v s V C tfin ada m π crs}
  → FanoutValid ctx (Closed cid hk n cp v s (accUTxO V) C tfin ada) m π crs
  → distributedOuts ctx m ⊆ V
```

```
fanout-distributes-committed b = accVerify-sound (FanoutValid.membersOK b)
```

```agda
finalPartialFanout-distributes-committed : ∀ {ctx cid hk n tfin V ada m π crs}
  → FinalPartialFanoutValid ctx (FanoutProgress cid hk n tfin (accUTxO V) ada) m π crs
  → distributedOuts ctx m ⊆ V
```

```
finalPartialFanout-distributes-committed b = accVerify-sound (FinalPartialFanoutValid.membersOK b)
```

A validly-initialised head is moreover a reachable state, tying the
$muHead$ init conditions (`versionZero`, `etaEmpty`) to the reachability
all of the above is stated over (@agda-appendix: `init-reachable`).

```agda
init-reachable : ∀ {ctx seed cid hk n cp v η ada}
  → initValid ctx seed (Open cid hk n cp v η ada)
  → Reachable (Open cid hk n cp v η ada)
```

```
init-reachable b rewrite InitValid.versionZero b | InitValid.etaEmpty b = reach-init
```

=== The contest game is bounded

The security section's completeness theorem (@sec:security) assumes "the
latest multi-signed snapshot wins the close/contest game"; the on-chain
half of that game is bounded by two machine-checked facts. First, the
contestation deadline of any validly-reachable `Closed` head is at most the
close-time deadline plus one contestation period per recorded contester:
each contest extends the deadline by at most $T$, consuming the close and
contest deadline equations across the whole run.

```agda
deadline-bounded : ∀ {cid hk n cp v s η C tfin ada}
  → Reachableᵛ (Closed cid hk n cp v s η C tfin ada)
  → ∃[ hi ] (tfin ≤ (hi + cp) + length C * cp)
```

```
deadline-bounded (closeᵛ {ctx = ctx} r cv) with CloseValid.step cv
... | close =
  ValidityInterval.hi (Context.validity ctx) ,
  ≤-trans (≤-reflexive (CloseValid.deadlineOK cv)) (m≤m+n _ 0)
deadline-bounded {n = n} {cp = cp} (contestᵛ {C = C₀} {kh = kh} r cv)
  with deadline-bounded r
... | hi , ih = hi , bound
  where
    step≤ : ∀ {tfin₀ tfin} → tfin ≡ (if ⌊ length (kh ∷ C₀) ≟ n ⌋ then tfin₀ else (tfin₀ + cp))
          → tfin ≤ tfin₀ + cp
    step≤ {tfin₀} eq with length (kh ∷ C₀) ≟ n
    ... | yes _ = ≤-trans (≤-reflexive eq) (m≤m+n tfin₀ cp)
    ... | no  _ = ≤-reflexive eq

    reassoc : ∀ a b c → (a + b) + c ≡ a + (c + b)
    reassoc a b c = trans (+-assoc a b c) (cong (a +_) (+-comm b c))

    bound : _ ≤ (hi + cp) + (cp + length C₀ * cp)
    bound = ≤-trans (step≤ (ContestValid.deadlineOK cv))
            (≤-trans (+-monoˡ-≤ cp ih)
                     (≤-reflexive (reassoc (hi + cp) (length C₀ * cp) cp)))
deadline-bounded (incrementᵛ r iv) with IncrementValid.step iv
... | ()
deadline-bounded (decrementᵛ r dv) with DecrementValid.step dv
... | ()
deadline-bounded (partialᵛ r pv) with PartialFanoutValid.step pv
... | ()

-- List pigeonhole plumbing for the cardinality bound: a duplicate-free list that injects into `ys`
-- is no longer than `ys` (via first-occurrence removal, decidable by `_≟ℍ_`).
private
  ∉ˡ[] : ∀ {kh : ℍ} → ¬ (kh ∈ˡ [])
  ∉ˡ[] ()

  removeʰ : ℍ → List ℍ → List ℍ
  removeʰ x []       = []
  removeʰ x (y ∷ ys) with x ≟ℍ y
  ... | yes _ = ys
  ... | no  _ = y ∷ removeʰ x ys

  length-removeʰ : ∀ {x} ys → x ∈ˡ ys → length ys ≡ suc (length (removeʰ x ys))
  length-removeʰ {x} (y ∷ ys) (here px)   with x ≟ℍ y
  ... | yes _  = refl
  ... | no ¬p  = ⊥-elim (¬p px)
  length-removeʰ {x} (y ∷ ys) (there mem) with x ≟ℍ y
  ... | yes _  = refl
  ... | no  _  = cong suc (length-removeʰ ys mem)

  ∈-removeʰ : ∀ {a x} ys → a ∈ˡ ys → ¬ (a ≡ x) → a ∈ˡ removeʰ x ys
  ∈-removeʰ {a} {x} (y ∷ ys) (here px)   a≢x with x ≟ℍ y
  ... | yes x≡y = ⊥-elim (a≢x (trans px (sym x≡y)))
  ... | no  _   = here px
  ∈-removeʰ {a} {x} (y ∷ ys) (there mem) a≢x with x ≟ℍ y
  ... | yes _ = mem
  ... | no  _ = there (∈-removeʰ ys mem a≢x)

  distinct-⊆-length : ∀ {xs ys} → Distinct xs → (∀ {a} → a ∈ˡ xs → a ∈ˡ ys) → length xs ≤ length ys
  distinct-⊆-length {[]}     _           _   = z≤n
  distinct-⊆-length {x ∷ xs} {ys} (x∉xs , dxs) sub =
    ≤-trans (s≤s (distinct-⊆-length dxs sub'))
            (≤-reflexive (sym (length-removeʰ ys (sub (here refl)))))
    where
      sub' : ∀ {a} → a ∈ˡ xs → a ∈ˡ removeʰ x ys
      sub' {a} mem = ∈-removeʰ ys (sub (there mem)) (λ a≡x → x∉xs (subst (_∈ˡ xs) a≡x mem))
```

Second, the contesters are drawn from the head's $n$ participation-token
names, so there are at most $n$ of them. The participant-set facts are
taken as _module hypotheses_ rather than global postulates on purpose:
"a unit quantity of asset $(cid, keyHash)$ in a head-output value names one
of the head's init-minted participation tokens" is true on-chain by
once-only minting (the $muHead$ seed is spent, so policy-$cid$ tokens
beyond the $n+1$ minted at init can never exist), but it is not true of the
raw value model, where any map literal can carry any asset. Scoping the
fact as a hypothesis keeps the theory honest: the theorems hold in any
model or run where the values at the head are ledger-produced.

#theorem(name: "The contest window is bounded")[
  Under the once-only-mint hypotheses, every recorded contester of a
  validly-reachable `Closed` head is a participant, so (with
  no-double-contest) at most $n$ contests can ever be recorded, and the
  contestation deadline is at most $(t_"hi" + T) + n dot T$ for the close
  transaction's upper validity bound $t_"hi"$: the deadline moves at most
  $n$ times, and fan-out is enabled by close plus $n+1$ contestation
  periods (@agda-appendix: `contesters-are-participants`,
  `contesters-bounded`, `contest-window-bounded`). This is the atemporal
  safety half of the contest game; no fairness or liveness is assumed.
] <thm:contest-window>

```agda
module ContestBound
  (ptKeysOf : ℍ → List ℍ)
  (pt-mem   : ∀ {ctx cid kh} → quantityOf (headValue ctx) (cid , kh) ≡ 1ℤ → kh ∈ˡ ptKeysOf cid)
  where

  contesters-are-participants : ∀ {cid hk n cp v s η C tfin ada}
    → Reachableᵛ (Closed cid hk n cp v s η C tfin ada)
    → ∀ {kh} → kh ∈ˡ C → kh ∈ˡ ptKeysOf cid
```

```
  contesters-are-participants (closeᵛ r cv) mem with CloseValid.step cv
  ... | close = ⊥-elim (∉ˡ[] mem)
  contesters-are-participants (contestᵛ {ctx = ctx} r cv) (here refl) =
    pt-mem {ctx = ctx} (ContestValid.contesterIsParticipant cv)
  contesters-are-participants (contestᵛ r cv) (there mem) = contesters-are-participants r mem
  contesters-are-participants (incrementᵛ r iv) mem with IncrementValid.step iv
  ... | ()
  contesters-are-participants (decrementᵛ r dv) mem with DecrementValid.step dv
  ... | ()
  contesters-are-participants (partialᵛ r pv) mem with PartialFanoutValid.step pv
  ... | ()
```

```agda
  contesters-bounded : ∀ {cid hk n cp v s η C tfin ada}
    → Reachableᵛ (Closed cid hk n cp v s η C tfin ada)
    → length C ≤ length (ptKeysOf cid)
```

```
  contesters-bounded reach =
    distinct-⊆-length (reachable-contesters-distinct (reachableᵛ→reachable reach))
                      (contesters-are-participants reach)
```

```agda
  contest-window-bounded : ∀ {cid hk n cp v s η C tfin ada}
    → length (ptKeysOf cid) ≡ n
    → Reachableᵛ (Closed cid hk n cp v s η C tfin ada)
    → ∃[ hi ] (tfin ≤ (hi + cp) + n * cp)
```

```
  contest-window-bounded {cp = cp} {C = C} pn reach =
    let (hi , le) = deadline-bounded reach
    in hi , ≤-trans le (+-monoʳ-≤ (hi + cp) (*-monoˡ-≤ cp (subst (length C ≤_) pn (contesters-bounded reach))))
```

=== The bridge to the extracted reference checker <sec:bridge-flagship>

The `spec ⇒ extracted-reference` half of the differential chain (the Scope note
of @sec:security) is proved per conjunct in the typecheck-only `ReferenceBridge`
module. Its flagship composition lemma is re-stated here so the rendered
document carries one representative: a single `closeValid` bundle (shown for the
`closeInitial` case) discharges the extracted close checker, the value
preservation checker and the no-mint checker at once, on the same inputs the
`hydra-tx` `HeadValidatorAgreement` suite feeds the real validator. The other
transaction families compose identically; their statements live in
`ReferenceBridge`, and the injected mocks and encoding postulates the bridge
rests on are the drift-checked ledger of @sec:assumption-inventory.

```agda
close-spec⇒reference : ∀ ctx cid hk n cp v η ada s′ η′ C tfin
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) closeInitial
  → (R.closeRefᵇ RB.mockOps (R.mkOpenᶜ v cp) (R.mkClosedᶜ v cp s′ (length C) tfin)
        (RB.closeTagOf closeInitial)
        (ValidityInterval.hi (Context.validity ctx)) (ValidityInterval.lo (Context.validity ctx)) ≡ true)
    × (R.valuePreservedᵇ (adaOf (headValueIn ctx)) (adaOf (headValue ctx))
                         (nonAdaOf (headValueIn ctx)) (nonAdaOf (headValue ctx)) ≡ true)
    × (R.noMintRefᵇ (RB.mintEntryCount ctx) ≡ true)
```

```
close-spec⇒reference = RB.closeChainInitial→ref
```
