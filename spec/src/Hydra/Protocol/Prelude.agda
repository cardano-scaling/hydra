-- Project-wide Agda foundation for the Hydra specification.
--
-- This module is pure plumbing: it is type-checked by Agda (imported by
-- Hydra.Protocol.Main) but is NOT part of the rendered Typst document (build.sh
-- only stages .lagda.typ files). It re-exports the libraries the spec builds on
-- and fixes the opaque base types. Crypto primitives are postulated here for now
-- and refined into proper interfaces in Step 2 of the Agda adoption plan.

module Hydra.Protocol.Prelude where

-- Finite sets (ℙ_), finite maps (_⇀_), singletons, ∅ˢ, mapˢ, … on a concrete
-- list-based model. This backs the spec's sets of inputs/outputs and UTxO maps.
open import abstract-set-theory.FiniteSetTheory public

-- Standard-library essentials used throughout the spec's notation.
open import Data.Nat using (ℕ; zero; suc; _≤_; _<_; _+_; _∸_; _≟_) public
open import Data.Integer using (ℤ) public
open import Data.Bool using (Bool; true; false; if_then_else_; _∧_) public
open import Data.Empty using (⊥) public
open import Data.Unit using (⊤) public
open import Relation.Nullary using (¬_; Dec) public
open import Relation.Nullary.Decidable using (⌊_⌋) public
open import Relation.Binary.PropositionalEquality using (_≡_; refl) public
open import Data.Maybe using (Maybe; just; nothing) public
open import Data.List using (List; []; _∷_; length) public
-- List membership, renamed to avoid clashing with the set-theory `_∈_`.
open import Data.List.Membership.Propositional using () renaming (_∈_ to _∈ˡ_) public
open import Data.Product using (_×_; _,_; proj₁; proj₂) public
open import Data.Sum using (_⊎_; inj₁; inj₂) public

-- Spec notation: 𝔹 = booleans, ℍ = byte strings (hashes, keys), Data = Plutus Data.
𝔹 : Set
𝔹 = Bool

postulate
  ℍ      : Set         -- byte strings (spec's tyBytes); hashes and keys live here
  Data   : Set         -- Plutus Data (spec's tyData)
  Script : Set         -- validator/minting scripts 𝒱; outputs store their hash
  VKey   : Set         -- verification keys (elements of 𝒦)

-- On-chain payload types carried in datums/redeemers. Kept abstract; the
-- accumulator commitment η is deliberately distinct from its hash η# = hash η.
postulate
  AccCommitment : Set  -- accumulator commitment η (NOT its hash)
  PartySig      : Set  -- individual party signature σⱼ (NOT the aggregate)
  AggSig        : Set  -- aggregate multi-signature ξ (= msComb of the σⱼ)
  AccWitness    : Set  -- accumulator membership/exclusion witness π

-- Serialisation / hashing (spec §3.1). Kept abstract.
postulate
  concat : List ℍ → ℍ              -- ℍ* → ℍ
  bytes  : ∀ {A : Set} → A → ℍ     -- invertible serialisation to bytes
  hash   : ∀ {A : Set} → A → ℍ     -- collision-resistant hash; x# = hash x
  _≟ℍ_   : (x y : ℍ) → Dec (x ≡ y) -- hashes are byte strings: decidable equality

-- Aggregate multisignature verification (the §3.2 scheme's MS-Verify, instantiated
-- at the protocol's key/message/signature types). Kept abstract (EdDSA-based).
postulate
  msVfy : VKey → ℍ → AggSig → Bool  -- aggregate key, message, aggregate signature

-- Multi-asset values (spec's 𝖵𝖺𝗅): a finite map from (policy, token) to a
-- signed quantity (negative quantities express burning when minting).
CId Token : Set
CId   = ℍ
Token = ℍ

Quantity : Set
Quantity = ℤ

Value : Set
Value = (CId × Token) ⇀ Quantity

-- Multi-asset value arithmetic. Kept abstract (the pointwise definitions over the
-- map are not needed to state the validator's value conditions).
postulate
  εᵛ   : Value                  -- the empty/zero value
  _+ᵛ_ : Value → Value → Value  -- value addition (multiset union of assets)
  _≤ᵛ_ : Value → Value → Set    -- "contained in" (the head value is preserved/grows)
  -- Lovelace (ada) projection: the ada quantity of a value. Additive, so it commutes with `_+ᵛ_`.
  -- This is the homomorphism the differential test exploits to check value conservation on the
  -- (extractable) lovelace component.
  adaOf    : Value → ℕ
  adaOf-+ᵛ : ∀ a b → adaOf (a +ᵛ b) ≡ adaOf a + adaOf b
  -- Non-ada projection: the TOTAL quantity of all non-ada tokens in a value. Also an additive
  -- homomorphism, so it commutes with `_+ᵛ_` exactly like `adaOf`. Checking conservation on BOTH
  -- projections (`adaOf` and `nonAdaOf`) catches value movement in ada AND in native tokens, closing
  -- the "non-ada token siphon is invisible" gap of an ada-only differential. (Per-token granularity
  -- would need the full asset map; this total-quantity projection catches any siphon that changes the
  -- non-ada total, which a real token theft does.)
  nonAdaOf    : Value → ℕ
  nonAdaOf-+ᵛ : ∀ a b → nonAdaOf (a +ᵛ b) ≡ nonAdaOf a + nonAdaOf b
  -- The algebra the value-conservation predicates reason over: (Value, _+ᵛ_, εᵛ) is a commutative
  -- monoid and _≤ᵛ_ a partial order compatible with addition. All hold of the pointwise signed
  -- multi-asset map. NB quantities are ℤ (negative = burning), so `a ≤ᵛ a +ᵛ b` does NOT hold in
  -- general (only when `b` is non-negative); that monotone-growth fact is therefore NOT a law here.
  +ᵛ-assoc     : ∀ a b c → ((a +ᵛ b) +ᵛ c) ≡ (a +ᵛ (b +ᵛ c))
  +ᵛ-comm      : ∀ a b → (a +ᵛ b) ≡ (b +ᵛ a)
  +ᵛ-identityˡ : ∀ a → (εᵛ +ᵛ a) ≡ a
  +ᵛ-identityʳ : ∀ a → (a +ᵛ εᵛ) ≡ a
  ≤ᵛ-refl      : ∀ {a} → a ≤ᵛ a
  ≤ᵛ-trans     : ∀ {a b c} → a ≤ᵛ b → b ≤ᵛ c → a ≤ᵛ c
  ≤ᵛ-antisym   : ∀ {a b} → a ≤ᵛ b → b ≤ᵛ a → a ≡ b
  +ᵛ-monoˡ     : ∀ {a b} (c : Value) → a ≤ᵛ b → (a +ᵛ c) ≤ᵛ (b +ᵛ c)
