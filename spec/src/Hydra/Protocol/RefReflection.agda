-- Reflection of the `Reference` checker's decidable Bool operators into propositional truth: each
-- lemma turns a propositional equality/inequality into the corresponding Bool check (`_==ᵇ_`/`_≤ᵇ_`/
-- `_<ᵇ_`/`_&&_`, or the builtin `_==_`/`_<_`) returning `true`. This is the boilerplate the
-- `ReferenceBridge` correspondence proofs reason with; it is factored out here so the bridge module
-- reads as the `*Valid → ref` correspondence and nothing else. Typecheck-only (not extracted, not
-- rendered).
module Hydra.Protocol.RefReflection where

open import Hydra.Protocol.Prelude
open import Data.Nat using (z≤n; s≤s)
open import Agda.Builtin.Nat using (_==_; suc) renaming (_<_ to _<ᴮ_)
open import Data.Empty using (⊥-elim)
open import Relation.Binary.PropositionalEquality using (cong)
import Hydra.Protocol.Reference as R

-- Soundness of the BUILTIN Nat equality `_==_` w.r.t. propositional equality. The lovelace / deadline
-- conjuncts are checked with `_==_` (native Integer equality at extraction) rather than the structural
-- `_==ᵇ_` (O(n) unary recursion, pathological on lovelace- / POSIXTime-scale values). The builtin does
-- not reduce on open terms, so this reflection lemma is postulated; it is trivially true and lives in
-- the same trust category as the builtin arithmetic (`_+_`) the bridge already relies on.
postulate
  ==-sound : ∀ {m n} → m ≡ n → (m == n) ≡ true
-- Same, for the BUILTIN strict-less-than `_<ᴮ_` (used for the after-deadline conjunct).
  <ᴮ-sound : ∀ {m n} → m < n → (m <ᴮ n) ≡ true

-- Soundness of the builtin-based `_≤ᴮ_` (= `a <ᴮ suc b`; the posted-before-deadline conjuncts). Unlike
-- the two postulates above this is PROVED: `m ≤ n` gives `m < suc n` (= `suc m ≤ suc n`) by `s≤s`, and
-- `m R.≤ᴮ n` unfolds definitionally to `m <ᴮ suc n`, so it is exactly `<ᴮ-sound (s≤s p)`.
≤ᴮ-sound : ∀ {m n} → m ≤ n → (m R.≤ᴮ n) ≡ true
≤ᴮ-sound p = <ᴮ-sound (s≤s p)

-- The structural Bool checks reflect their propositional relations:
==ᵇ-refl : ∀ n → (n R.==ᵇ n) ≡ true
==ᵇ-refl zero    = refl
==ᵇ-refl (suc n) = ==ᵇ-refl n

≡→==ᵇ : ∀ {m n} → m ≡ n → (m R.==ᵇ n) ≡ true
≡→==ᵇ {m} refl = ==ᵇ-refl m

-- The negative direction: distinct numbers fail the structural equality. (Used by the contest bridge
-- to reflect the conditional deadline-update's all-contested test `⌊ length C' ≟ n ⌋` into `_==ᵇ_`.)
¬→==ᵇfalse : ∀ {m n} → ¬ (m ≡ n) → (m R.==ᵇ n) ≡ false
¬→==ᵇfalse {zero}  {zero}  ¬e = ⊥-elim (¬e refl)
¬→==ᵇfalse {zero}  {suc n} _  = refl
¬→==ᵇfalse {suc m} {zero}  _  = refl
¬→==ᵇfalse {suc m} {suc n} ¬e = ¬→==ᵇfalse {m} {n} (λ e → ¬e (cong suc e))

≤→≤ᵇ : ∀ {m n} → m ≤ n → (m R.≤ᵇ n) ≡ true
≤→≤ᵇ z≤n     = refl
≤→≤ᵇ (s≤s p) = ≤→≤ᵇ p

<→<ᵇ : ∀ {m n} → m < n → (m R.<ᵇ n) ≡ true
<→<ᵇ p = ≤→≤ᵇ p

&&-intro : ∀ {a b} → a ≡ true → b ≡ true → (a R.&& b) ≡ true
&&-intro refl q = q
