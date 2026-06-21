-- Reflection of the `Reference` checker's decidable Bool operators into propositional truth: each
-- lemma turns a propositional equality/inequality into the corresponding Bool check (`_==ᵇ_`/`_≤ᵇ_`/
-- `_<ᵇ_`/`_&&_`, or the builtin `_==_`/`_<_`) returning `true`. This is the boilerplate the
-- `ReferenceBridge` correspondence proofs reason with; it is factored out here so the bridge module
-- reads as the `*Valid → ref` correspondence and nothing else. Typecheck-only (not extracted, not
-- rendered).
module Hydra.Protocol.RefReflection where

open import Hydra.Protocol.Prelude
open import Data.Nat using (z≤n; s≤s)
open import Agda.Builtin.Nat using (_==_) renaming (_<_ to _<ᴮ_)
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

-- The structural Bool checks reflect their propositional relations:
==ᵇ-refl : ∀ n → (n R.==ᵇ n) ≡ true
==ᵇ-refl zero    = refl
==ᵇ-refl (suc n) = ==ᵇ-refl n

≡→==ᵇ : ∀ {m n} → m ≡ n → (m R.==ᵇ n) ≡ true
≡→==ᵇ {m} refl = ==ᵇ-refl m

≤→≤ᵇ : ∀ {m n} → m ≤ n → (m R.≤ᵇ n) ≡ true
≤→≤ᵇ z≤n     = refl
≤→≤ᵇ (s≤s p) = ≤→≤ᵇ p

<→<ᵇ : ∀ {m n} → m < n → (m R.<ᵇ n) ≡ true
<→<ᵇ p = ≤→≤ᵇ p

&&-intro : ∀ {a b} → a ≡ true → b ≡ true → (a R.&& b) ≡ true
&&-intro refl q = q
