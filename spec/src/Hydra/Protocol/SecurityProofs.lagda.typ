```
-- The machine-checked PROOFS of the §7 security properties. Factored out of `Security.lagda.typ` so
-- that the rendered §7 shows the model + the property STATEMENTS first, and this literate module
-- renders the machine-checked RESULTS (the statements below appear in the document and in the
-- appendix; the proof terms are typechecked as part of the build but not rendered): the invariant
-- and its corollaries (L1/L2/L3), `consistency`/`soundness`/`completeness`, the
-- once-honest-then-corrupt extension, the seen-set invariant, and the off-chain⇒on-chain `reflects`
-- bridge. Every name here is imported by the spec build, so the properties remain machine-checked.
module Hydra.Protocol.SecurityProofs where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.OffChain
open import Hydra.Protocol.Preliminaries using (Output; ValidityInterval; Context)
open import Data.Fin using (Fin)
open import Data.Nat using (z≤n; s≤s)
open import Data.Nat.Properties using (≤-total; ≤-antisym; ≤-refl; ≤-trans; m≤n⇒m≤1+n; 1+n≰n; +-identityʳ; +-suc; suc-injective; m+[n∸m]≡n; m+n≡0⇒m≡0)
open import Data.Sum using (map₁; map₂; [_,_]′)
open import Data.List using (_++_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.List.Membership.Propositional.Properties using (∈-++⁺ʳ; ∈-++⁺ˡ; ∈-++⁻)
open import Data.List.Relation.Binary.Subset.Propositional.Properties using () renaming (⊆-refl to ⊆ˡ-refl; ⊆-trans to ⊆ˡ-trans)
open import Data.Vec using (Vec; lookup; _[_]≔_)
open import Data.Vec.Properties using (lookup∘update; lookup∘update′)
import Data.Fin.Properties as FinP
open import Data.Product using (Σ-syntax)
open import Data.List.Relation.Binary.Subset.Propositional using () renaming (_⊆_ to _⊆ˡ_)
open import Relation.Nullary using (yes; no)
open import Relation.Binary.PropositionalEquality using (trans; sym; cong; subst)
open import Data.Empty using (⊥-elim)
import Hydra.Protocol.OnChain as OC
open import Hydra.Protocol.Security
open System  -- bring the System field projections (parties/honest/localOf/U₀/seen/onChain) into scope
```

#import "/template.typ": *
#import "/macros.typ": *

== Machine-checked results <sec:security-theorems>

The properties of the preceding section are not only stated over the execution
model; they are proved. This section renders the machine-checked results: each
statement below is the actual Agda type of a theorem in the module
`SecurityProofs`, shown here and in @agda-appendix, while the proof terms — and
the supporting lemmas and corollaries the prose cites by name — are typechecked
as part of this document's build but not rendered. All results are
inductions over the `Reachable` closure of the step relation `_⟶ˢ_`
(@sec:security); none of them is temporal (liveness remains out of scope).
The trust base is exactly the one enumerated in the Proofs preamble: the ledger
postulates (`applyTxs` and its laws), per-signature unforgeability plus the
aggregation scheme's decomposition (from which `ms-unforgeable` is derived),
the honest-behaviour premises of `signHonest`, and, where a result consumes an
on-chain validity bundle, the per-instance signature-trust hypotheses noted
with that result.

```
-- The empty tx list is always applicable (from the nil law).
[]-applicable : ∀ U → Applicable U []
[]-applicable U eq = bot (trans (sym (applyTxs-nil U)) eq)
  where bot : just U ≡ nothing → ⊥
        bot ()

-- No element is a member of the empty list.
∉[] : ∀ {A : Set} {x : A} → ¬ (x ∈ˡ [])
∉[] ()

-- `true` and `false` are distinct (one shared absurdity lemma, used wherever an honest flag clashes
-- with a `false`).
trueNotFalse : true ≡ false → ⊥
trueNotFalse ()

-- Vec/Fin helper: corruption only ever removes honest parties, so an honest party in the
-- post-state was honest in the pre-state.
honest-mono : ∀ {n} (v : Vec Bool n) (i k : Fin n)
  → lookup (v [ i ]≔ false) k ≡ true → lookup v k ≡ true
honest-mono v i k h with i FinP.≟ k
... | no  i≢k  = trans (sym (lookup∘update′ (λ e → i≢k (sym e)) v false)) h
... | yes refl = ⊥-elim (trueNotFalse (trans (sym h) (lookup∘update i v false)))

-- A certified snapshot stays certified when a signature is added (`sigs` only grows): used to carry
-- "predecessor is certified" facts forward as the signature set grows.
Certified-mono : ∀ (sys : System) {snap : Snapshot} {x : Fin (parties sys) × Snapshot}
  → Certified sys snap → Certified (record sys { sigs = x ∷ sigs sys }) snap
Certified-mono _ cert i = there (cert i)

-- A non-⊥ Maybe is some `just`. (Used by `invStep`'s signHonest applicability derivation and by
-- `soundness`; defined here so it is in scope for both.)
≢nothing→just : ∀ {A : Set} (m : Maybe A) → ¬ (m ≡ nothing) → Σ[ x ∈ A ] (m ≡ just x)
≢nothing→just (just x) _  = x , refl
≢nothing→just nothing  ¬n = ⊥-elim (¬n refl)
```

The workhorse is a single induction: every reachable system satisfies the
eight-field invariant `Inv` of @sec:security (@agda-appendix: `invariant`).
The safety content is derived from the structure of the steps rather than
assumed: `confApp` (L3) is discharged at `confirm` from `sigApp` (a certified
snapshot carries the honest confirmer's own signature, and honest signatures
are only on applicable snapshots), and `sigChain` records, for every honest
signature, an extending certified-or-genesis predecessor (from the
`signHonest` premises together with `confCert`), which is what the nesting
lemma below consumes. Corruption only ever shrinks the honest set, and `sigs`
only grows, so certification facts are carried forward across steps. All of
the L1/L2/L3 consequences used by the theorems of this section are projections
of this one invariant.

```agda
invariant : ∀ sys → Reachable sys → Inv sys
```

```
invariant sys (base (noSigs , allConfNumZero , allConfTxsEmpty , _ , _)) = record
  { sigApp   = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; sigDedup = λ {k} {s1} _ m1 _ _ → ⊥-elim (∉[] (subst (λ z → (k , s1) ∈ˡ z) noSigs m1))
  ; confApp  = λ {i} _ → subst (Applicable (U₀ sys)) (sym (allConfTxsEmpty i)) ([]-applicable (U₀ sys))
  ; sigPos   = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; confCert = λ {i} _ → inj₁ (allConfNumZero i , allConfTxsEmpty i)
  ; sigChain = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; signNumBound = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; sigSeen      = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  }
invariant sys (step {s} r tr) = invStep tr (invariant s r)
  where
    invStep : ∀ {a b} → a ⟶ˢ b → Inv a → Inv b
    -- signHonest fires the reqSn-sign handler (pattern `reqSn-sign vEq sEq`): the four guards are
    -- derived here from the handler's `s ≡ s̄+1` (sEq), the snapshot/Δ shape (txsEq₀), the
    -- U₀-applicability premise (appl₀), the no-in-flight precondition (nf₀) + `signNumBound`, and the
    -- Δ-seen premise (Δseen₀) + `sigSeen`/`confCert`. The signer's `seenNumber` is bumped, so the
    -- localOf-reading fields carry `lookup∘update` bookkeeping (the `.confirmed` they read is
    -- unchanged; only `seenNumber` moves).
    invStep {a} (signHonest {i = i} {snap = snap₀} {Δ = Δ} hi₀ nf₀ (reqSn-sign vEq sEq) txsEq₀ appl₀ Δseen₀ _ _)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain
                   ; signNumBound = signNumBound ; sigSeen = sigSeen } = record
      { sigApp = newApp ; sigDedup = newDed ; confApp = newConfApp
      ; sigPos = newPos ; confCert = newCert ; sigChain = newChain
      ; signNumBound = newSNB ; sigSeen = newSigSeen }
      where
        stᵢ : LocalState
        stᵢ = lookup (localOf a) i
        st' : LocalState
        st' = record stᵢ { seenNumber = Snapshot.number snap₀ }
        -- G3a: the handler's `s ≡ s̄+1` guard (confirmedNo stᵢ = Snapshot.number (confirmed stᵢ)).
        numEq₀ : Snapshot.number snap₀ ≡ suc (confirmedNo stᵢ)
        numEq₀ = sEq
        -- G3b: from snap.txs = confirmedTxs ++ Δ.
        ext⊆₀ : confirmedTxs stᵢ ⊆ˡ Snapshot.txs snap₀
        ext⊆₀ {x} x∈ = subst (x ∈ˡ_) (sym txsEq₀) (∈-++⁺ˡ x∈)
        -- G1: confApp ⇒ applyTxs U₀ confirmedTxs ≡ just U′; Δ applies to U′ (appl₀); compose lifts to U₀.
        g1 : Applicable (U₀ a) (Snapshot.txs snap₀)
        g1 = subst (Applicable (U₀ a)) (sym txsEq₀) applU₀
          where
            U′just : Σ[ U′ ∈ UTxO ] applyTxs (U₀ a) (confirmedTxs stᵢ) ≡ just U′
            U′just = ≢nothing→just (applyTxs (U₀ a) (confirmedTxs stᵢ)) (confApp hi₀)
            applU₀ : Applicable (U₀ a) (confirmedTxs stᵢ ++ Δ)
            applU₀ e≡n = appl₀ (proj₂ U′just)
                              (trans (sym (applyTxs-compose (confirmedTxs stᵢ) Δ (proj₂ U′just))) e≡n)
        -- the signer's own confirmed txs are seen (confCert: genesis [] or certified ⇒ it signed them).
        confSeen-i : confirmedTxs stᵢ ⊆ˡ lookup (seen a) i
        confSeen-i with confCert hi₀
        ... | inj₁ (_ , ct≡[]) = subst (_⊆ˡ lookup (seen a) i) (sym ct≡[]) (λ ())
        ... | inj₂ cert        = sigSeen hi₀ (cert i)
        -- G4: snap.txs = confirmedTxs ++ Δ, both ⊆ seen (confSeen-i and the Δseen₀ premise).
        here-seen : Snapshot.txs snap₀ ⊆ˡ lookup (seen a) i
        here-seen {x} x∈ = [ (λ e → confSeen-i e) , (λ e → Δseen₀ e) ]′
                             (∈-++⁻ (confirmedTxs stᵢ) (subst (x ∈ˡ_) txsEq₀ x∈))
        -- map a pre-state certified-or-genesis fact to the post-state (sigs grew).
        certMono : ∀ {k}
                 → (confirmedNo (lookup (localOf a) k) ≡ 0 × confirmedTxs (lookup (localOf a) k) ≡ [])
                     ⊎ Certified a (LocalState.confirmed (lookup (localOf a) k))
                 → (confirmedNo (lookup (localOf a) k) ≡ 0 × confirmedTxs (lookup (localOf a) k) ≡ [])
                     ⊎ Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a })
                                 (LocalState.confirmed (lookup (localOf a) k))
        certMono (inj₁ p) = inj₁ p
        certMono (inj₂ c) = inj₂ (Certified-mono a {x = (i , snap₀)} c)
        newApp : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Applicable (U₀ a) (Snapshot.txs snap)
        newApp _  (here e)  = subst (λ z → Applicable (U₀ a) (Snapshot.txs z)) (sym (cong proj₂ e)) g1
        newApp hk (there m) = sigApp hk m
        newDed : ∀ {k s1 s2} → lookup (honest a) k ≡ true
               → (k , s1) ∈ˡ ((i , snap₀) ∷ sigs a) → (k , s2) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
        newDed _  (here e1)  (here e2)  _  = trans (cong proj₂ e1) (sym (cong proj₂ e2))
        newDed {s2 = s2} _ (here e1) (there m2) n≡ =
          ⊥-elim (1+n≰n (subst (_≤ confirmedNo stᵢ)
                               (trans (sym n≡) (trans (cong Snapshot.number (cong proj₂ e1)) numEq₀))
                               (subst (Snapshot.number s2 ≤_) nf₀
                                      (signNumBound hi₀ (subst (λ p → (p , s2) ∈ˡ sigs a) (cong proj₁ e1) m2)))))
        newDed {s1 = s1} _ (there m1) (here e2) n≡ =
          ⊥-elim (1+n≰n (subst (_≤ confirmedNo stᵢ)
                               (trans n≡ (trans (cong Snapshot.number (cong proj₂ e2)) numEq₀))
                               (subst (Snapshot.number s1 ≤_) nf₀
                                      (signNumBound hi₀ (subst (λ p → (p , s1) ∈ˡ sigs a) (cong proj₁ e2) m1)))))
        newDed hk (there m1) (there m2) n≡ = sigDedup hk m1 m2 n≡
        newPos : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → 0 < Snapshot.number snap
        newPos _  (here e)  = subst (0 <_) (sym (trans (cong Snapshot.number (cong proj₂ e)) numEq₀)) (s≤s z≤n)
        newPos hk (there m) = sigPos hk m
        newConfApp : ∀ {i'} → lookup (honest a) i' ≡ true
                   → Applicable (U₀ a) (confirmedTxs (lookup (localOf a [ i ]≔ st') i'))
        newConfApp {i'} hi' with i FinP.≟ i'
        ... | no  i≢i' = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update′ (λ e → i≢i' (sym e)) (localOf a) st')) (confApp hi')
        ... | yes refl = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update i (localOf a) st')) (confApp hi')
        newCert : ∀ {k} → lookup (honest a) k ≡ true
                → (confirmedNo (lookup (localOf a [ i ]≔ st') k) ≡ 0 × confirmedTxs (lookup (localOf a [ i ]≔ st') k) ≡ [])
                  ⊎ Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a })
                              (LocalState.confirmed (lookup (localOf a [ i ]≔ st') k))
        newCert {k} hk with i FinP.≟ k
        ... | no  i≢k  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ [])
                                       ⊎ Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a })
                                                   (LocalState.confirmed w))
                               (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (certMono (confCert hk))
        ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ [])
                                       ⊎ Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a })
                                                   (LocalState.confirmed w))
                               (sym (lookup∘update i (localOf a) st')) (certMono (confCert hk))
        newChain : ∀ {k snap} → lookup (honest a) k ≡ true
                 → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a)
                 → PredecessorWitness (Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a })) snap
        newChain _ (here e) = mkPredecessor (LocalState.confirmed stᵢ)
            (trans (cong Snapshot.number (cong proj₂ e)) numEq₀)
            (subst (λ z → Snapshot.txs (LocalState.confirmed stᵢ) ⊆ˡ Snapshot.txs z) (sym (cong proj₂ e)) ext⊆₀)
            preGenesisOrCert
          where
            preGenesisOrCert : (Snapshot.number (LocalState.confirmed stᵢ) ≡ 0)
               ⊎ Certified (record a { localOf = localOf a [ i ]≔ st' ; sigs = (i , snap₀) ∷ sigs a }) (LocalState.confirmed stᵢ)
            preGenesisOrCert with confCert hi₀
            ... | inj₁ (n , _) = inj₁ n
            ... | inj₂ c       = inj₂ (Certified-mono a {x = (i , snap₀)} c)
        newChain hk (there m) with sigChain hk m
        ... | mkPredecessor pre numberSuc txsExtend (inj₁ z) = mkPredecessor pre numberSuc txsExtend (inj₁ z)
        ... | mkPredecessor pre numberSuc txsExtend (inj₂ c) =
              mkPredecessor pre numberSuc txsExtend (inj₂ (Certified-mono a {x = (i , snap₀)} c))
        newSNB : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number snap ≤ LocalState.seenNumber (lookup (localOf a [ i ]≔ st') k)
        newSNB _ (here e) =
          subst (λ p → Snapshot.number (proj₂ p) ≤ LocalState.seenNumber (lookup (localOf a [ i ]≔ st') (proj₁ p)))
                (sym e)
                (subst (Snapshot.number snap₀ ≤_)
                       (sym (cong LocalState.seenNumber (lookup∘update i (localOf a) st'))) ≤-refl)
        newSNB {k} {snap} hk (there m) with i FinP.≟ k
        ... | no  i≢k  = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (signNumBound hk m)
        ... | yes refl = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update i (localOf a) st'))
                               (subst (Snapshot.number snap ≤_) (sym numEq₀)
                                      (m≤n⇒m≤1+n (subst (Snapshot.number snap ≤_) nf₀ (signNumBound hk m))))
        newSigSeen : ∀ {k snap} → lookup (honest a) k ≡ true
                   → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Snapshot.txs snap ⊆ˡ lookup (seen a) k
        newSigSeen _ (here e) =
          subst (λ p → Snapshot.txs (proj₂ p) ⊆ˡ lookup (seen a) (proj₁ p)) (sym e) here-seen
        newSigSeen hk (there m) = sigSeen hk m
    invStep {a} (signCorrupt {i = i} {snap = snap₀} ci)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain
                   ; signNumBound = signNumBound ; sigSeen = sigSeen } = record
      { sigApp = newApp ; sigDedup = newDed ; confApp = confApp
      ; sigPos = newPos ; confCert = newCert ; sigChain = newChain
      ; signNumBound = newSNB ; sigSeen = newSigSeen }
      where
        clash : ∀ {k snap} → lookup (honest a) k ≡ true → (k , snap) ≡ (i , snap₀) → ⊥
        clash hk e = trueNotFalse (trans (sym (subst (λ p → lookup (honest a) p ≡ true) (cong proj₁ e) hk)) ci)
        newApp : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Applicable (U₀ a) (Snapshot.txs snap)
        newApp hk (here e)  = ⊥-elim (clash hk e)
        newApp hk (there m) = sigApp hk m
        newDed : ∀ {k s1 s2} → lookup (honest a) k ≡ true
               → (k , s1) ∈ˡ ((i , snap₀) ∷ sigs a) → (k , s2) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
        newDed hk (here e1)  _          _  = ⊥-elim (clash hk e1)
        newDed hk (there m1) (here e2)  _  = ⊥-elim (clash hk e2)
        newDed hk (there m1) (there m2) n≡ = sigDedup hk m1 m2 n≡
        newPos : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → 0 < Snapshot.number snap
        newPos hk (here e)  = ⊥-elim (clash hk e)
        newPos hk (there m) = sigPos hk m
        newCert : ∀ {k} → lookup (honest a) k ≡ true
                → (confirmedNo (lookup (localOf a) k) ≡ 0 × confirmedTxs (lookup (localOf a) k) ≡ [])
                  ⊎ Certified (record a { sigs = (i , snap₀) ∷ sigs a }) (LocalState.confirmed (lookup (localOf a) k))
        newCert hk with confCert hk
        ... | inj₁ p = inj₁ p
        ... | inj₂ c = inj₂ (Certified-mono a {x = (i , snap₀)} c)
        newChain : ∀ {k snap} → lookup (honest a) k ≡ true
                 → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a)
                 → PredecessorWitness (Certified (record a { sigs = (i , snap₀) ∷ sigs a })) snap
        newChain hk (here e)  = ⊥-elim (clash hk e)
        newChain hk (there m) with sigChain hk m
        ... | mkPredecessor pre numberSuc txsExtend (inj₁ z) = mkPredecessor pre numberSuc txsExtend (inj₁ z)
        ... | mkPredecessor pre numberSuc txsExtend (inj₂ c) =
              mkPredecessor pre numberSuc txsExtend (inj₂ (Certified-mono a {x = (i , snap₀)} c))
        -- corrupt sign: localOf / seen unchanged, so the two new invariants pass through (the new sig
        -- is the corrupt party's, on which the honest-only invariants impose nothing - `clash`).
        newSNB : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number snap ≤ LocalState.seenNumber (lookup (localOf a) k)
        newSNB hk (here e)  = ⊥-elim (clash hk e)
        newSNB hk (there m) = signNumBound hk m
        newSigSeen : ∀ {k snap} → lookup (honest a) k ≡ true
                   → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Snapshot.txs snap ⊆ˡ lookup (seen a) k
        newSigSeen hk (here e)  = ⊥-elim (clash hk e)
        newSigSeen hk (there m) = sigSeen hk m
    invStep {a} (confirm {i = c} {snap = snap₀} aggOK _)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain
                   ; signNumBound = signNumBound ; sigSeen = sigSeen } = record
      { sigApp = sigApp ; sigDedup = sigDedup ; confApp = newConfApp
      ; sigPos = sigPos ; confCert = newCert ; sigChain = sigChain
      ; signNumBound = newSNB ; sigSeen = sigSeen }
      where
        st' : LocalState
        st' = record (lookup (localOf a) c) { confirmed = snap₀ }
        cert : Certified a snap₀                     -- unforgeability: the verified agg sig ⇒ all signed
        cert = ms-unforgeable a snap₀ aggOK
        newConfApp : ∀ {i} → lookup (honest a) i ≡ true
                   → Applicable (U₀ a) (confirmedTxs (lookup (localOf a [ c ]≔ st') i))
        newConfApp {i} hi with c FinP.≟ i
        ... | no  c≢i  = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update′ (λ e → c≢i (sym e)) (localOf a) st')) (confApp hi)
        ... | yes refl = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update c (localOf a) st')) (sigApp hi (cert c))
        newCert : ∀ {k} → lookup (honest a) k ≡ true
                → (confirmedNo (lookup (localOf a [ c ]≔ st') k) ≡ 0 × confirmedTxs (lookup (localOf a [ c ]≔ st') k) ≡ [])
                  ⊎ Certified a (LocalState.confirmed (lookup (localOf a [ c ]≔ st') k))
        newCert {k} hk with c FinP.≟ k
        ... | no  c≢k  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                               (sym (lookup∘update′ (λ e → c≢k (sym e)) (localOf a) st')) (confCert hk)
        ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                               (sym (lookup∘update c (localOf a) st')) (inj₂ cert)
        -- confirm changes only `.confirmed`; `seenNumber`/`sigs` are untouched, so signNumBound just
        -- needs the lookup∘update bookkeeping (seenNumber of the updated entry is unchanged).
        newSNB : ∀ {k snap} → lookup (honest a) k ≡ true → (k , snap) ∈ˡ sigs a
               → Snapshot.number snap ≤ LocalState.seenNumber (lookup (localOf a [ c ]≔ st') k)
        newSNB {k} {snap} hk m with c FinP.≟ k
        ... | no  c≢k  = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update′ (λ e → c≢k (sym e)) (localOf a) st')) (signNumBound hk m)
        ... | yes refl = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update c (localOf a) st')) (signNumBound hk m)
    invStep {a} (corrupt i₀)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain
                   ; signNumBound = signNumBound ; sigSeen = sigSeen } = record
      { sigApp   = λ {k} {snap} hk mem → sigApp (honest-mono (honest a) i₀ k hk) mem
      ; sigDedup = λ {k} {s1} {s2} hk m1 m2 n≡ → sigDedup (honest-mono (honest a) i₀ k hk) m1 m2 n≡
      ; confApp  = λ {i} hi → confApp (honest-mono (honest a) i₀ i hi)
      ; sigPos   = λ {k} {snap} hk mem → sigPos (honest-mono (honest a) i₀ k hk) mem
      ; confCert = λ {i} hi → confCert (honest-mono (honest a) i₀ i hi)
      ; sigChain = λ {k} {snap} hk mem → sigChain (honest-mono (honest a) i₀ k hk) mem
      ; signNumBound = λ {k} {snap} hk m → signNumBound (honest-mono (honest a) i₀ k hk) m
      ; sigSeen      = λ {k} {snap} hk m → sigSeen (honest-mono (honest a) i₀ k hk) m }
    -- `finalize` changes only `onChain`; no `Inv` field mentions it, so re-pack the same proofs (the
    -- record is nominal in `sys`, so we cannot return `inv` directly even though the fields coincide).
    invStep {a} (finalize _ _) inv = record
      { sigApp = Inv.sigApp inv ; sigDedup = Inv.sigDedup inv ; confApp = Inv.confApp inv
      ; sigPos = Inv.sigPos inv ; confCert = Inv.confCert inv ; sigChain = Inv.sigChain inv
      ; signNumBound = Inv.signNumBound inv ; sigSeen = Inv.sigSeen inv }
    -- `see` grows party i₀'s seen set; only `sigSeen` mentions `seen`, so it is the only field that
    -- needs work (membership is preserved by the `++`); the rest re-pack unchanged.
    invStep {a} (see {i = i₀} {txs = txs}) inv = record
      { sigApp = Inv.sigApp inv ; sigDedup = Inv.sigDedup inv ; confApp = Inv.confApp inv
      ; sigPos = Inv.sigPos inv ; confCert = Inv.confCert inv ; sigChain = Inv.sigChain inv
      ; signNumBound = Inv.signNumBound inv ; sigSeen = newSigSeen }
      where
        newSigSeen : ∀ {k snap} → lookup (honest a) k ≡ true → (k , snap) ∈ˡ sigs a
                   → Snapshot.txs snap ⊆ˡ lookup (seen a [ i₀ ]≔ (txs ++ lookup (seen a) i₀)) k
        newSigSeen {k} {snap} hk m with i₀ FinP.≟ k
        ... | no  i≢k  = subst (λ w → Snapshot.txs snap ⊆ˡ w)
                               (sym (lookup∘update′ (λ e → i≢k (sym e)) (seen a) (txs ++ lookup (seen a) i₀)))
                               (Inv.sigSeen inv hk m)
        ... | yes refl = subst (λ w → Snapshot.txs snap ⊆ˡ w)
                               (sym (lookup∘update i₀ (seen a) (txs ++ lookup (seen a) i₀)))
                               (λ x∈ → ∈-++⁺ʳ txs (Inv.sigSeen inv hk m x∈))
    -- offChain (deposit/recover/tick/increment/decrement or reqDec): `sigs`/`seen`/`U₀` unchanged, and
    -- the updated party's confirmed snapshot + seen number are preserved (confPres/snPres). So the 5
    -- signature-only fields re-pack verbatim; only the 3 localOf-reading fields need `lookup∘update`
    -- bookkeeping plus the two preservation equalities.
    invStep {a} (offChain {i = i} {st' = st'} _ confPres snPres)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain
                   ; signNumBound = signNumBound ; sigSeen = sigSeen } = record
      { sigApp = sigApp ; sigDedup = sigDedup ; confApp = newConfApp
      ; sigPos = sigPos ; confCert = newConfCert ; sigChain = sigChain
      ; signNumBound = newSNB ; sigSeen = sigSeen }
      where
        newConfApp : ∀ {i'} → lookup (honest a) i' ≡ true
                   → Applicable (U₀ a) (confirmedTxs (lookup (localOf a [ i ]≔ st') i'))
        newConfApp {i'} hi' with i FinP.≟ i'
        ... | no  i≢i' = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update′ (λ e → i≢i' (sym e)) (localOf a) st')) (confApp hi')
        ... | yes refl = subst (λ w → Applicable (U₀ a) (confirmedTxs w))
                               (sym (lookup∘update i (localOf a) st'))
                               (subst (λ c → Applicable (U₀ a) (Snapshot.txs c)) (sym confPres) (confApp hi'))
        newConfCert : ∀ {k} → lookup (honest a) k ≡ true
                    → (confirmedNo (lookup (localOf a [ i ]≔ st') k) ≡ 0 × confirmedTxs (lookup (localOf a [ i ]≔ st') k) ≡ [])
                      ⊎ Certified a (LocalState.confirmed (lookup (localOf a [ i ]≔ st') k))
        newConfCert {k} hk with i FinP.≟ k
        ... | no  i≢k  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                               (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (confCert hk)
        ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                               (sym (lookup∘update i (localOf a) st'))
                               (subst (λ c → (Snapshot.number c ≡ 0 × Snapshot.txs c ≡ []) ⊎ Certified a c)
                                      (sym confPres) (confCert hk))
        newSNB : ∀ {k snap} → lookup (honest a) k ≡ true → (k , snap) ∈ˡ sigs a
               → Snapshot.number snap ≤ LocalState.seenNumber (lookup (localOf a [ i ]≔ st') k)
        newSNB {k} {snap} hk m with i FinP.≟ k
        ... | no  i≢k  = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (signNumBound hk m)
        ... | yes refl = subst (λ w → Snapshot.number snap ≤ LocalState.seenNumber w)
                               (sym (lookup∘update i (localOf a) st'))
                               (subst (Snapshot.number snap ≤_) (sym snPres) (signNumBound hk m))
```

```
-- ── Derived corollaries of the invariant ───────────────────────────────────────────────────────
-- L3 (applicability), exposed: every honest party's confirmed snapshot is applicable to U₀.
conf-applicable : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
```

```
conf-applicable sys reach = Inv.confApp (invariant sys reach)
```

```
-- L3 for certified snapshots: a CERTIFIED snapshot is applicable to U₀, witnessed by any honest
-- party (who, by `Certified`, signed it, and whose signatures are only on applicable snapshots).
cert-applicable : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → Applicable (U₀ sys) (Snapshot.txs snap)
```

```
cert-applicable sys reach {h} hh cert = Inv.sigApp (invariant sys reach) hh (cert h)
```

Agreement (L1) is the projection of the invariant's one-signature-per-number
field: two certified snapshots of the same number are equal, witnessed by any
honest party, who by `Certified` signed both (@agda-appendix: `agree`).

```agda
agree : ∀ sys → Reachable sys → ∀ {h s1 s2} → lookup (honest sys) h ≡ true
  → Certified sys s1 → Certified sys s2 → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
```

```
agree sys reach {h} hh c1 c2 = Inv.sigDedup (invariant sys reach) hh (c1 h) (c2 h)
```

```
-- A certified snapshot has number > 0 (an honest party signed it, and honest signing is for the
-- snapshot one above its confirmed number, hence ≥ 1).
cert-pos : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → 0 < Snapshot.number snap
```

```
cert-pos sys reach {h} hh cert = Inv.sigPos (invariant sys reach) hh (cert h)
```

```
-- An honest party's confirmed snapshot is the genesis (number 0, txs []) or is certified.
confCert-of : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
    ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
```

```
confCert-of sys reach = Inv.confCert (invariant sys reach)
```

```
-- Every honest signature on `snap` has an extending certified-or-genesis predecessor `pre`.
sigChain-of : ∀ sys → Reachable sys → ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
  → PredecessorWitness (Certified sys) snap
```

```
sigChain-of sys reach = Inv.sigChain (invariant sys reach)
```

```
-- ── L2: certified snapshots nest ────────────────────────────────────────────────────────────────
-- Certified snapshots nest by number. Proof by induction on the gap d = number s2 ∸ number s1: at
-- d=0 the numbers are equal, so by agreement (L1) the snapshots are equal; at d=suc, the higher
-- snapshot s2 has (by `sigChain-of`) an extending certified-or-genesis predecessor `pre` one number
-- below it, so we recurse on the smaller gap to `pre` and compose with `txs pre ⊆ txs s2`. The
-- genesis case is impossible: `cert-pos` makes a certified snapshot's number positive, but `pre`
-- would sit at number 0.
cert-nest-aux : ∀ sys → Reachable sys → ∀ d {h s1 s2}
  → lookup (honest sys) h ≡ true → Certified sys s1 → Certified sys s2
  → Snapshot.number s1 + d ≡ Snapshot.number s2
  → Snapshot.txs s1 ⊆ˡ Snapshot.txs s2
cert-nest-aux sys reach zero {h} {s1} {s2} hh c1 c2 eq =
  subst (λ z → Snapshot.txs s1 ⊆ˡ Snapshot.txs z)
        (agree sys reach hh c1 c2 (trans (sym (+-identityʳ (Snapshot.number s1))) eq))
        ⊆ˡ-refl
cert-nest-aux sys reach (suc d') {h} {s1} {s2} hh c1 c2 eq
  with sigChain-of sys reach hh (c2 h)
... | mkPredecessor pre numberSuc txsExtend (inj₂ certPre) =
      ⊆ˡ-trans (cert-nest-aux sys reach d' hh c1 certPre eq') txsExtend
  where
    eq' : Snapshot.number s1 + d' ≡ Snapshot.number pre
    eq' = suc-injective (trans (sym (+-suc (Snapshot.number s1) d')) (trans eq numberSuc))
... | mkPredecessor pre numberSuc txsExtend (inj₁ preNumZero) =
      ⊥-elim (1≤0 (subst (1 ≤_) ns1≡0 (cert-pos sys reach hh c1)))
  where
    eq' : Snapshot.number s1 + d' ≡ Snapshot.number pre
    eq' = suc-injective (trans (sym (+-suc (Snapshot.number s1) d')) (trans eq numberSuc))
    ns1≡0 : Snapshot.number s1 ≡ 0
    ns1≡0 = m+n≡0⇒m≡0 (Snapshot.number s1) (trans eq' preNumZero)
    1≤0 : 1 ≤ 0 → ⊥
    1≤0 ()
```

Certified snapshots nest by number (L2). The proof is an induction on the gap
between the two numbers: at gap zero the snapshots are equal by `agree`;
otherwise the higher snapshot has, by the invariant's `sigChain`, an extending
certified-or-genesis predecessor one number below it, so the induction
recurses to the predecessor and composes with the extension, the genesis case
being impossible for a certified (hence positive-numbered) snapshot
(@agda-appendix: `cert-nest`).

```agda
cert-nest : ∀ sys → Reachable sys → ∀ {h s1 s2}
  → lookup (honest sys) h ≡ true → Certified sys s1 → Certified sys s2
  → Snapshot.number s1 ≤ Snapshot.number s2 → Snapshot.txs s1 ⊆ˡ Snapshot.txs s2
```

```
cert-nest sys reach {h} {s1} {s2} hh c1 c2 le =
  cert-nest-aux sys reach (Snapshot.number s2 ∸ Snapshot.number s1) hh c1 c2 (m+[n∸m]≡n le)
```

The @sec:security nesting obligation follows: two honest parties' confirmed snapshots
nest by number, since an honest party's confirmed snapshot is the genesis
(whose transaction list is empty, hence trivially contained) or is certified,
in which case `cert-nest` applies (@agda-appendix: `confirmed-nest`).

```agda
confirmed-nest : ∀ sys → Reachable sys → ∀ i j
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → confirmedNo (lookup (localOf sys) i) ≤ confirmedNo (lookup (localOf sys) j)
  → confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
```

```
confirmed-nest sys reach i j hi hj le with confCert-of sys reach hi
... | inj₁ (_ , ti≡[]) = subst (_⊆ˡ confirmedTxs (lookup (localOf sys) j)) (sym ti≡[]) []⊆
  where []⊆ : [] ⊆ˡ confirmedTxs (lookup (localOf sys) j)
        []⊆ ()
... | inj₂ ci with confCert-of sys reach hj
... | inj₁ (nj≡0 , _) =
      ⊥-elim (1≤0 (subst (1 ≤_)
        (≤-antisym (subst (confirmedNo (lookup (localOf sys) i) ≤_) nj≡0 le) z≤n)
        (cert-pos sys reach hi ci)))
  where 1≤0 : 1 ≤ 0 → ⊥
        1≤0 ()
... | inj₂ cj = cert-nest sys reach hi ci cj le
```

=== Consistency

#theorem(name: "Consistency")[
  In any reachable system, the confirmed transaction sets of two honest
  parties nest (one contains the other) and each is applicable to $Uinit$, so
  their union never fails to apply; the union form is machine-checked as a
  single transaction set containing both that is applicable to $Uinit$
  (`consistency-union`), making the paper's
  $Uinit compose (Tbar_i union Tbar_j) != bot$ literal. The property moreover
  extends to once-honest-then-corrupt parties: _every_ party's confirmed
  snapshot is genesis-or-certified unconditionally (`confCert-all`), because
  `confirm` demands a verifying aggregate multisignature regardless of the
  confirmer's honesty, so _any_ two parties' confirmed sets, including one a
  party confirmed before (or even adopted after) being corrupted, nest and are
  applicable, given at least one honest witness (`consistency-uncorrupted`)
  (@agda-appendix).
] <thm:consistency>

```agda
consistency : Consistency
```

```
consistency sys reach i j hi hj =
  nested , conf-applicable sys reach hi , conf-applicable sys reach hj
  where
    nested : (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j))
           ⊎ (confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
    nested with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
    ... | inj₁ le = inj₁ (confirmed-nest sys reach i j hi hj le)
    ... | inj₂ ge = inj₂ (confirmed-nest sys reach j i hj hi ge)
```

The union form: there is a single transaction set `T` containing both honest
confirmed sets (their union, i.e. the inclusion-larger of the two, since they
nest) that is applicable to $Uinit$.

```
consistency-union : ∀ sys → Reachable sys → ∀ i j
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → Σ[ T ∈ List Data ] (confirmedTxs (lookup (localOf sys) i) ⊆ˡ T
                      × confirmedTxs (lookup (localOf sys) j) ⊆ˡ T
                      × Applicable (U₀ sys) T)
```

```
consistency-union sys reach i j hi hj
  with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
... | inj₁ le = confirmedTxs (lookup (localOf sys) j)
              , confirmed-nest sys reach i j hi hj le , ⊆ˡ-refl , conf-applicable sys reach hj
... | inj₂ ge = confirmedTxs (lookup (localOf sys) i)
              , ⊆ˡ-refl , confirmed-nest sys reach j i hj hi ge , conf-applicable sys reach hi
```

The @sec:security random variables $Tbar_i$ scope a party's confirmed set to a party
_while uncorrupted_, and an on-chain close could be built against the
confirmed snapshot of a party corrupted after confirming. The extension rests
on the fact that certification is unconditional: the only step that changes a
party's confirmed snapshot is `confirm`, which requires a verifying aggregate
multisignature (hence, by unforgeability, a certificate) whatever the
confirmer's honesty flag. This is in fact stronger than the literal @sec:security
scoping, since it also covers any snapshot a corrupt party adopts after
corruption.

```
confCert-all : ∀ sys → Reachable sys → ∀ i
  → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
    ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
```

```
confCert-all sys (base (_ , cn≡0 , ct≡[] , _ , _)) i = inj₁ (cn≡0 i , ct≡[] i)
confCert-all sys (step {s} r tr) = cc tr (confCert-all s r)
  where
    cc : ∀ {a b} → a ⟶ˢ b
       → (∀ i → (confirmedNo (lookup (localOf a) i) ≡ 0 × confirmedTxs (lookup (localOf a) i) ≡ [])
                ⊎ Certified a (LocalState.confirmed (lookup (localOf a) i)))
       → (∀ i → (confirmedNo (lookup (localOf b) i) ≡ 0 × confirmedTxs (lookup (localOf b) i) ≡ [])
                ⊎ Certified b (LocalState.confirmed (lookup (localOf b) i)))
    cc {a} (signHonest {i = signer} {snap = snap₀} _ _ _ _ _ _ _ _) ih i with signer FinP.≟ i
    ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ [])
                                   ⊎ Certified (record a { localOf = localOf a [ signer ]≔ st'ₕ ; sigs = (signer , snap₀) ∷ sigs a }) (LocalState.confirmed w))
                           (sym (lookup∘update signer (localOf a) st'ₕ)) (mono (ih signer))
      where
        st'ₕ = record (lookup (localOf a) signer) { seenNumber = Snapshot.number snap₀ }
        mono : (confirmedNo (lookup (localOf a) signer) ≡ 0 × confirmedTxs (lookup (localOf a) signer) ≡ []) ⊎ Certified a (LocalState.confirmed (lookup (localOf a) signer))
             → (confirmedNo (lookup (localOf a) signer) ≡ 0 × confirmedTxs (lookup (localOf a) signer) ≡ []) ⊎ Certified (record a { localOf = localOf a [ signer ]≔ st'ₕ ; sigs = (signer , snap₀) ∷ sigs a }) (LocalState.confirmed (lookup (localOf a) signer))
        mono (inj₁ p) = inj₁ p
        mono (inj₂ c) = inj₂ (Certified-mono a {x = signer , snap₀} c)
    ... | no  s≢i  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ [])
                                   ⊎ Certified (record a { localOf = localOf a [ signer ]≔ st'ₕ ; sigs = (signer , snap₀) ∷ sigs a }) (LocalState.confirmed w))
                           (sym (lookup∘update′ (λ e → s≢i (sym e)) (localOf a) st'ₕ)) (mono (ih i))
      where
        st'ₕ = record (lookup (localOf a) signer) { seenNumber = Snapshot.number snap₀ }
        mono : (confirmedNo (lookup (localOf a) i) ≡ 0 × confirmedTxs (lookup (localOf a) i) ≡ []) ⊎ Certified a (LocalState.confirmed (lookup (localOf a) i))
             → (confirmedNo (lookup (localOf a) i) ≡ 0 × confirmedTxs (lookup (localOf a) i) ≡ []) ⊎ Certified (record a { localOf = localOf a [ signer ]≔ st'ₕ ; sigs = (signer , snap₀) ∷ sigs a }) (LocalState.confirmed (lookup (localOf a) i))
        mono (inj₁ p) = inj₁ p
        mono (inj₂ c) = inj₂ (Certified-mono a {x = signer , snap₀} c)
    cc {a} (signCorrupt {i = signer} {snap = snap₀} _) ih i with ih i
    ... | inj₁ p = inj₁ p
    ... | inj₂ c = inj₂ (Certified-mono a {x = signer , snap₀} c)
    cc {a} (confirm {i = c} {snap = snap₀} aggOK _) ih i with c FinP.≟ i
    ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update c (localOf a) (record (lookup (localOf a) c) { confirmed = snap₀ })))
                           (inj₂ (ms-unforgeable a snap₀ aggOK))
    ... | no  c≢i  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update′ (λ e → c≢i (sym e)) (localOf a) (record (lookup (localOf a) c) { confirmed = snap₀ })))
                           (ih i)
    cc {a} (corrupt _)    ih i = ih i
    cc {a} (finalize _ _) ih i = ih i
    cc {a} (see)          ih i = ih i
    -- offChain leaves `sigs` (hence `Certified`) untouched and preserves the updated party's confirmed
    -- snapshot (confPres), so the genesis-or-certified status carries over (lookup∘update bookkeeping).
    cc {a} (offChain {i = j} {st' = st'} _ confPres _) ih i with j FinP.≟ i
    ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update j (localOf a) st'))
                           (subst (λ c → (Snapshot.number c ≡ 0 × Snapshot.txs c ≡ []) ⊎ Certified a c)
                                  (sym confPres) (ih i))
    ... | no  j≢i  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update′ (λ e → j≢i (sym e)) (localOf a) st')) (ih i)
```

```
-- Nesting for ANY two parties (via the honest witness `h`), from `confCert-all` + `cert-nest`.
nestU : ∀ sys → Reachable sys → ∀ {h} → lookup (honest sys) h ≡ true → ∀ i j
  → confirmedNo (lookup (localOf sys) i) ≤ confirmedNo (lookup (localOf sys) j)
  → confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
nestU sys reach hh i j le with confCert-all sys reach i
... | inj₁ (_ , ti≡[]) = subst (_⊆ˡ confirmedTxs (lookup (localOf sys) j)) (sym ti≡[]) []⊆
  where []⊆ : [] ⊆ˡ confirmedTxs (lookup (localOf sys) j)
        []⊆ ()
... | inj₂ ci with confCert-all sys reach j
... | inj₁ (nj≡0 , _) =
      ⊥-elim (1≤0 (subst (1 ≤_)
        (≤-antisym (subst (confirmedNo (lookup (localOf sys) i) ≤_) nj≡0 le) z≤n)
        (cert-pos sys reach hh ci)))
  where 1≤0 : 1 ≤ 0 → ⊥
        1≤0 ()
... | inj₂ cj = cert-nest sys reach hh ci cj le

-- Applicability for ANY party's confirmed snapshot (via the honest witness), from `confCert-all`.
appU : ∀ sys → Reachable sys → ∀ {h} → lookup (honest sys) h ≡ true → ∀ i
  → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
appU sys reach hh i with confCert-all sys reach i
... | inj₁ (_ , ti≡[]) = subst (Applicable (U₀ sys)) (sym ti≡[]) ([]-applicable (U₀ sys))
... | inj₂ ci = cert-applicable sys reach hh ci
```

```agda
consistency-uncorrupted : ∀ sys → Reachable sys → ∀ {h} → lookup (honest sys) h ≡ true → ∀ i j
  → (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
       ⊎ confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
   × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
   × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) j))
```

```
consistency-uncorrupted sys reach hh i j =
  nested , appU sys reach hh i , appU sys reach hh j
  where
    nested : (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j))
           ⊎ (confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
    nested with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
    ... | inj₁ le = inj₁ (nestU sys reach hh i j le)
    ... | inj₂ ge = inj₂ (nestU sys reach hh j i ge)
```

```
-- ── Seen-set invariant: every honest signature is on txs that party has SEEN ───────────────────
-- `Snapshot.txs snap ⊆ lookup seen k` for any honest `k` that signed `snap`. This is the `sigSeen`
-- component of the main invariant (derived at `signHonest` from the handler's Δ ⊆ seen premise + the
-- confirmed-txs-seen fact); exposed here as a thin corollary for `soundness`'s call site.
sigSeen-inv : ∀ sys → Reachable sys → ∀ {k snap}
  → lookup (honest sys) k ≡ true → Signed sys k snap
  → Snapshot.txs snap ⊆ˡ lookup (seen sys) k
```

```
sigSeen-inv sys reach = Inv.sigSeen (invariant sys reach)
```

=== Soundness and completeness

#theorem(name: "Soundness")[
  In any reachable system with at least one honest party, if a snapshot's
  aggregate multisignature verifies, then applying its transactions to
  $Uinit$ succeeds (the final UTxO set exists and is conflict-free) and those
  transactions have been seen by _every_ honest party
  ($tilde(T) subset.eq inter.big_(j in honest) That_j$) (@agda-appendix:
  `soundness`).
] <thm:soundness>

Both conjuncts are derived: `ms-unforgeable` makes the verified snapshot
certified, an honest signer signs only applicable snapshots
(`cert-applicable`, giving conflict-freedom), and only transactions it has
seen (the `sigSeen` component of the invariant, giving the intersection-seen
subset).

```agda
soundness : Soundness
```

```
soundness sys reach {snap = snap} hh aggOK =
  let cert      = ms-unforgeable sys snap aggOK
      finalJust = ≢nothing→just (Ufinal sys snap) (cert-applicable sys reach hh cert)
  in proj₁ finalJust , proj₂ finalJust , (λ {j} hj → sigSeen-inv sys reach hj (cert j))
```

#theorem(name: "Completeness")[
  Every honest party's confirmed transactions are contained in the finalized
  snapshot (the snapshot whose aggregate multisignature verifies), whenever
  that party's confirmed number is at most the finalized snapshot's number
  (@agda-appendix: `completeness`).
] <thm:completeness>

The finalized snapshot is certified (`ms-unforgeable`); the party's confirmed
snapshot is genesis (trivially contained) or certified (`confCert-of`), and
two certified snapshots nest by number (`cert-nest`), using the party itself
as the honest witness. The `confirmedNo ≤ number` premise is the "latest
multi-signed snapshot wins the close/contest game" fact of @sec:close-tx/@sec:contest-tx: the real
close/contest process always settles on the latest multi-signed snapshot,
whereas the model's `finalize` admits _any_ certified snapshot, so the fact
enters as a per-party premise rather than being derived.

```agda
completeness : Completeness
```

```
completeness sys reach {snap} aggOK i hi le with confCert-of sys reach hi
... | inj₁ (_ , ti≡[]) = subst (_⊆ˡ Snapshot.txs snap) (sym ti≡[]) []⊆snap
  where []⊆snap : [] ⊆ˡ Snapshot.txs snap
        []⊆snap ()
... | inj₂ ci = cert-nest sys reach hi ci (ms-unforgeable sys snap aggOK) le
```

=== The on-chain reflection bridge

#theorem(name: "On-chain settlement reflects the off-chain state")[
  A finalization against a snapshot whose aggregate multisignature verifies
  yields `Reflects`: the off-chain final UTxO exists (from
  @thm:soundness), the on-chain snapshot number matches (the `finalize`
  witness), and the stored accumulator commits to that UTxO. Conversely, when
  the datum reflects a finalized snapshot, the outputs a valid on-chain
  fan-out actually distributes are a subset of that off-chain final UTxO's
  outputs (`reflect-sound`, `reflect-fanout-⊆`) (@agda-appendix).
] <thm:reflection>

Two of the three `Reflects` conjuncts are derived; the accumulator commitment
is supplied as the explicit per-finalization hypothesis `ηEq`, the irreducible
signature-trust assumption: $nuHead$ authenticates $eta$ via the
multisignature over $cid || v || s || eta^(\#) || delta^(\#) || kappa^(\#)$, not by recomputing
$accUTxO(U)$. It is a hypothesis rather than a global postulate on purpose,
since `finalize` admits any datum with a matching snapshot number, a global
axiom would have no model, and the finalizer discharges `ηEq` from the $eta$
it actually committed.

```agda
reflects : ∀ sys → Reachable sys → ∀ {h snap}
  → lookup (honest sys) h ≡ true
  → AggVerified sys snap
  → OC.snapNum (onChain sys) ≡ Snapshot.number snap
  → (∀ U → Ufinal sys snap ≡ just U → OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))
  → Reflects sys snap
```

```
reflects sys reach {snap = snap} hh aggOK numEq ηEq =
  let s        = soundness sys reach hh aggOK
      finalEq  = proj₁ (proj₂ s)
  in mkReflects (proj₁ s) finalEq numEq (ηEq (proj₁ s) finalEq)
```

```agda
reflect-sound : ∀ sys → Reachable sys → ∀ {snap} → Reflects sys snap
  → Σ[ U ∈ UTxO ] (Ufinal sys snap ≡ just U)
                × (OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))
```

```
reflect-sound sys reach (mkReflects U conflictFree _ accCommits) = U , conflictFree , accCommits
```

The fan-out half consumes the bundle's membership conjunct through the
accumulator soundness law: the outputs the transaction pays (the anchored
`distributedOuts`) are a subset of the outputs of the off-chain final UTxO,
tying the transaction's own outputs, not a free set parameter, to the
Soundness UTxO.

```agda
reflect-fanout-⊆ : ∀ sys → ∀ {ctx U m π crs}
  → OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U)
  → OC.FanoutValid ctx (onChain sys) m π crs
  → OC.distributedOuts ctx m ⊆ outsOf U
```

```
reflect-fanout-⊆ sys {ctx} {U} {m} {π} η≡ b =
  OC.accVerify-sound
    (subst (λ z → OC.accVerify z (OC.distributedOuts ctx m) π ≡ true) η≡ (OC.FanoutValid.membersOK b))
```

=== No settlement without unanimity

#theorem(name: "No settlement without unanimity")[
  Every @sec:increment-tx–@sec:contest-tx validity bundle carries a `sigOK` conjunct bottoming out in
  `snapshotSigOK` ($msVfy$ over
  $cid || v || s || eta^(\#) || delta^(\#) || kappa^(\#)$), and the model's
  signing message `snapMsg` is _defined_ as that same concatenation. Once the
  snapshot's identifying fields match the signed ones and the redeemer's
  aggregate signature is the system-recorded one (the per-instance
  signature-trust hypothesis `ξEq`, the same pattern as the reflection
  bridge's `ηEq`), the on-chain conjunct is exactly `AggVerified`, and
  unforgeability certifies: every party signed. Hence no increment, decrement,
  close or contest settles without a unanimous certificate (`sig-certifies`
  and the per-transaction `*-certified` corollaries); the certified snapshot's
  accumulator hash is the hash of the accumulator actually stored in the
  produced datum (`close-η-reflected`, `contest-η-reflected`); and a valid
  deposit-claim transaction is both unanimously certified and posted before
  the deposit's recover deadline (`claimTx-certified`) (@agda-appendix).
] <thm:unanimity>

These corollaries consume the `sigOK` fields of the on-chain bundles, so the
off-chain certificate layer and the on-chain validity bundles meet at the
signature conjuncts, not only at datum accessors. The field-matching
equalities are dischargeable by `refl` for the snapshot actually signed;
`ξEq` is per-instance for the same reason `ηEq` is.

```agda
sig-certifies : ∀ sys (snap : Snapshot) {hk cid v s η# δ# κ# ξ}
  → OC.snapshotSigOK hk cid v s η# δ# κ# ξ
  → hk ≡ aggKey
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ s
  → Snapshot.etaHash snap ≡ η#
  → Snapshot.decHash snap ≡ δ#
  → Snapshot.comHash snap ≡ κ#
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
```

```
sig-certifies sys snap sig refl refl refl refl refl refl refl refl = ms-unforgeable sys snap sig
```

The per-transaction corollaries (`increment-certified`, `decrement-certified`,
`close-certified`, `contest-certified`) each consume their bundle's `sigOK`
field through a one-line application of `sig-certifies` (typechecked, not
rendered); the `closeUsed`/`contestUsed`/`closeAny` redeemer variants follow
identically (their `sigOK` reduces to `snapshotSigOK` at version $v - 1$ or
$v$).

```
increment-certified : ∀ sys {ctx cid v d d' ξ s ref δ#} (snap : Snapshot)
  → OC.IncrementValid ctx aggKey cid v d d' ξ s ref δ#
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ s
  → Snapshot.etaHash snap ≡ hash (OC.ηOf d')
  → Snapshot.decHash snap ≡ δ#
  → Snapshot.comHash snap ≡ OC.depositCommitsHashOf ctx ref
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
```

```
increment-certified sys snap b cidEq vEq sEq ηEq δEq κEq ξEq =
  sig-certifies sys snap (OC.IncrementValid.sigOK b) refl cidEq vEq sEq ηEq δEq κEq ξEq
```

```
decrement-certified : ∀ sys {ctx cid v d d' ξ s m κ#} (snap : Snapshot)
  → OC.DecrementValid ctx aggKey cid v d d' ξ s m κ#
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ s
  → Snapshot.etaHash snap ≡ hash (OC.ηOf d')
  → Snapshot.decHash snap ≡ OC.decommitOutputsHashOf ctx m
  → Snapshot.comHash snap ≡ κ#
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
```

```
decrement-certified sys snap b cidEq vEq sEq ηEq δEq κEq ξEq =
  sig-certifies sys snap (OC.DecrementValid.sigOK b) refl cidEq vEq sEq ηEq δEq κEq ξEq
```

```
close-certified : ∀ sys {ctx cid v cp s' d d' ξ η# δ# κ#} (snap : Snapshot)
  → OC.CloseValid ctx aggKey cid v cp s' d d' (OC.closeUnused ξ η# δ# κ#)
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ s'
  → Snapshot.etaHash snap ≡ η#
  → Snapshot.decHash snap ≡ δ#
  → Snapshot.comHash snap ≡ κ#
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
```

```
close-certified sys snap b cidEq vEq sEq ηEq δEq κEq ξEq =
  sig-certifies sys snap (OC.CloseValid.sigOK b) refl cidEq vEq sEq ηEq δEq κEq ξEq
```

```
contest-certified : ∀ sys {ctx cid v s tfin d d' ξ η# δ# κ# kh} (snap : Snapshot)
  → OC.ContestValid ctx aggKey cid v s tfin d d' (OC.contestUnused ξ η# δ# κ#) kh
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ OC.snapNum d'
  → Snapshot.etaHash snap ≡ η#
  → Snapshot.decHash snap ≡ δ#
  → Snapshot.comHash snap ≡ κ#
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
```

```
contest-certified sys snap b cidEq vEq sEq ηEq δEq κEq ξEq =
  sig-certifies sys snap (OC.ContestValid.sigOK b) refl cidEq vEq sEq ηEq δEq κEq ξEq
```

On top of the certificate, close and contest cannot verify a signature over
one accumulator while storing another: the `etaOK` conjunct binds the
redeemer's $eta^(\#)$ to the hash of the accumulator stored in the produced
datum (the two-line corollaries `close-η-reflected` / `contest-η-reflected`,
typechecked but not rendered).

```
close-η-reflected : ∀ {ctx cid v cp s' d d' ξ η# δ# κ#} (snap : Snapshot)
  → OC.CloseValid ctx aggKey cid v cp s' d d' (OC.closeUnused ξ η# δ# κ#)
  → Snapshot.etaHash snap ≡ η#
  → Snapshot.etaHash snap ≡ hash (OC.ηOf d')
```

```
close-η-reflected snap b ηEq = trans ηEq (OC.CloseValid.etaOK b)
```

```
contest-η-reflected : ∀ {ctx cid v s tfin d d' ξ η# δ# κ# kh} (snap : Snapshot)
  → OC.ContestValid ctx aggKey cid v s tfin d d' (OC.contestUnused ξ η# δ# κ#) kh
  → Snapshot.etaHash snap ≡ η#
  → Snapshot.etaHash snap ≡ hash (OC.ηOf d')
```

```
contest-η-reflected snap b ηEq = trans ηEq (OC.ContestValid.etaOK b)
```

A valid deposit claim (the joint `ClaimTxValid` encoding, $nuHead$ increment
and $nuDeposit$ claim both passing) is unanimously certified _and_ posted
before the deposit's recover deadline: a deposit can be absorbed into the head
neither without every party's signature nor after it has become recoverable.

```agda
claimTx-certified : ∀ sys {ctx dd cid n cp v η ada headOut ξ s ref δ#} (snap : Snapshot)
  → OC.ClaimTxValid ctx dd (OC.Open cid aggKey n cp v η ada) headOut ξ s ref δ#
  → Snapshot.cid snap ≡ cid
  → Snapshot.version snap ≡ v
  → Snapshot.number snap ≡ s
  → Snapshot.etaHash snap ≡ hash (OC.ηOf headOut)
  → Snapshot.decHash snap ≡ δ#
  → Snapshot.comHash snap ≡ OC.depositCommitsHashOf ctx ref
  → ξ ≡ aggSigOf sys snap
  → Certified sys snap
    × (ValidityInterval.hi (Context.validity ctx) ≤ OC.DepositDatum.tRecover dd)
```

```
claimTx-certified sys snap b cidEq vEq sEq ηEq δEq κEq ξEq =
    increment-certified sys snap (OC.ClaimTxValid.headSideOK b) cidEq vEq sEq ηEq δEq κEq ξEq
  , OC.ClaimValid.beforeRecoverDeadline (OC.ClaimTxValid.depositSideOK b)
```

=== Handler invariants across adversarial executions

#invariant(name: "No commit/decommit overlap, version discipline (system level)")[
  Throughout any reachable adversarial execution, for every party: a pending
  commit ($tx_alpha$) and a pending decommit ($tx_omega$) are never both in
  flight (`noBothInFlightˢ`), and the party's seen open-state version is its
  confirmed snapshot's version or exactly one above (`versionDisciplineˢ`).
  Both are seeded by the `Initial` predicate and preserved by every `_⟶ˢ_`
  step, reusing the local handler-model preservation lemmas of
  @sec:offchain-theorems on the `offChain` step's embedded handler witness
  (@agda-appendix).
] <inv:system-handler>

This lifts the `require` disciplines of @fig:off-chain-prot from the single-party handler
model to the multi-party adversarial system: the deposit/decommit exclusivity
and the version discipline are properties of every reachable execution, not
runtime assertions an honest node merely hopes to maintain.

```agda
noBothInFlightˢ : ∀ sys → Reachable sys → ∀ i → NoBothInFlight (lookup (localOf sys) i)
```

```
noBothInFlightˢ sys (base (_ , _ , _ , nbf , _)) i = nbf i
noBothInFlightˢ sys (step {s} r tr) = nbStep tr (noBothInFlightˢ s r)
  where
    nbStep : ∀ {a b} → a ⟶ˢ b
           → (∀ i → NoBothInFlight (lookup (localOf a) i))
           → ∀ i → NoBothInFlight (lookup (localOf b) i)
    nbStep {a} (signHonest {i = j} _ _ _ _ _ _ _ _) ih i with j FinP.≟ i
    ... | yes refl = subst NoBothInFlight (sym (lookup∘update j (localOf a) _)) (ih j)
    ... | no  j≢i  = subst NoBothInFlight (sym (lookup∘update′ (λ e → j≢i (sym e)) (localOf a) _)) (ih i)
    nbStep (signCorrupt _) ih i = ih i
    nbStep {a} (confirm {i = c} _ _) ih i with c FinP.≟ i
    ... | yes refl = subst NoBothInFlight (sym (lookup∘update c (localOf a) _)) (ih c)
    ... | no  c≢i  = subst NoBothInFlight (sym (lookup∘update′ (λ e → c≢i (sym e)) (localOf a) _)) (ih i)
    nbStep (corrupt _)    ih i = ih i
    nbStep (finalize _ _) ih i = ih i
    nbStep see            ih i = ih i
    nbStep {a} (offChain {i = j} {st' = st'} w _ _) ih i with j FinP.≟ i
    ... | yes refl = subst NoBothInFlight (sym (lookup∘update j (localOf a) st')) (noBothInFlight-step w (ih j))
    ... | no  j≢i  = subst NoBothInFlight (sym (lookup∘update′ (λ e → j≢i (sym e)) (localOf a) st')) (ih i)
```

```agda
versionDisciplineˢ : ∀ sys → Reachable sys → ∀ i → VersionDiscipline (lookup (localOf sys) i)
```

```
versionDisciplineˢ sys (base (_ , _ , _ , _ , vd)) i = vd i
versionDisciplineˢ sys (step {s} r tr) = vdStep tr (versionDisciplineˢ s r)
  where
    vdStep : ∀ {a b} → a ⟶ˢ b
           → (∀ i → VersionDiscipline (lookup (localOf a) i))
           → ∀ i → VersionDiscipline (lookup (localOf b) i)
    vdStep {a} (signHonest {i = j} _ _ _ _ _ _ _ _) ih i with j FinP.≟ i
    ... | yes refl = subst VersionDiscipline (sym (lookup∘update j (localOf a) _)) (ih j)
    ... | no  j≢i  = subst VersionDiscipline (sym (lookup∘update′ (λ e → j≢i (sym e)) (localOf a) _)) (ih i)
    vdStep (signCorrupt _) ih i = ih i
    vdStep {a} (confirm {i = c} {snap = snap₀} _ vd) ih i with c FinP.≟ i
    ... | yes refl = subst VersionDiscipline (sym (lookup∘update c (localOf a) (record (lookup (localOf a) c) { confirmed = snap₀ }))) vd
    ... | no  c≢i  = subst VersionDiscipline (sym (lookup∘update′ (λ e → c≢i (sym e)) (localOf a) _)) (ih i)
    vdStep (corrupt _)    ih i = ih i
    vdStep (finalize _ _) ih i = ih i
    vdStep see            ih i = ih i
    vdStep {a} (offChain {i = j} {st' = st'} w _ _) ih i with j FinP.≟ i
    ... | yes refl = subst VersionDiscipline (sym (lookup∘update j (localOf a) st')) (versionDiscipline-step w (ih j))
    ... | no  j≢i  = subst VersionDiscipline (sym (lookup∘update′ (λ e → j≢i (sym e)) (localOf a) st')) (ih i)
```
