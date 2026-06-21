-- The machine-checked PROOFS of the §7 security properties. Factored out of `Security.lagda.typ` so
-- that the rendered §7 shows the model + the property STATEMENTS, while this typecheck-only module
-- (imported by Main, not rendered, not extracted) carries the proof terms: the invariant and its
-- corollaries (L1/L2/L3), `consistency`/`soundness`/`completeness`, the once-honest-then-corrupt
-- extension, the seen-set invariant, and the off-chain⇒on-chain `reflects` bridge. Every name here is
-- imported by the spec build, so the properties remain machine-checked; only their derivations live
-- out of the prose document.
module Hydra.Protocol.SecurityProofs where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.OffChain
open import Hydra.Protocol.Preliminaries using (Output)
open import Data.Fin using (Fin)
open import Data.Nat using (z≤n; s≤s)
open import Data.Nat.Properties using (≤-total; ≤-antisym; +-identityʳ; +-suc; suc-injective; m+[n∸m]≡n; m+n≡0⇒m≡0)
open import Data.Sum using (map₁; map₂)
open import Data.List using (_++_)
open import Data.List.Relation.Unary.Any using (here; there)
open import Data.List.Membership.Propositional.Properties using (∈-++⁺ʳ)
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

-- The invariants hold at every reachable system. The key safety facts are DERIVED, not assumed:
-- `confApp` (L3) discharges applicability at `confirm` from `sigApp` (a certified snapshot carries
-- the honest confirmer's own signature; honest signatures are only on applicable snapshots); and
-- `sigChain` records, for every honest signature, an extending certified-or-genesis predecessor
-- (from `signHonest`'s guards + `confCert`), which gives L2 (`confirmed-nest`). Corruption only
-- shrinks the honest set (`honest-mono`); `sigs` only grows (`Certified-mono` carries facts forward).
invariant : ∀ sys → Reachable sys → Inv sys
invariant sys (base (noSigs , allConfNumZero , allConfTxsEmpty)) = record
  { sigApp   = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; sigDedup = λ {k} {s1} _ m1 _ _ → ⊥-elim (∉[] (subst (λ z → (k , s1) ∈ˡ z) noSigs m1))
  ; confApp  = λ {i} _ → subst (Applicable (U₀ sys)) (sym (allConfTxsEmpty i)) ([]-applicable (U₀ sys))
  ; sigPos   = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  ; confCert = λ {i} _ → inj₁ (allConfNumZero i , allConfTxsEmpty i)
  ; sigChain = λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) noSigs mem))
  }
invariant sys (step {s} r tr) = invStep tr (invariant s r)
  where
    invStep : ∀ {a b} → a ⟶ˢ b → Inv a → Inv b
    invStep {a} (signHonest {i = i} {snap = snap₀} hi₀ appl₀ fresh numEq₀ ext⊆₀ _)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain } = record
      { sigApp = newApp ; sigDedup = newDed ; confApp = confApp
      ; sigPos = newPos ; confCert = newCert ; sigChain = newChain }
      where
        newApp : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Applicable (U₀ a) (Snapshot.txs snap)
        newApp _  (here e)  = subst (λ z → Applicable (U₀ a) (Snapshot.txs z)) (sym (cong proj₂ e)) appl₀
        newApp hk (there m) = sigApp hk m
        newDed : ∀ {k s1 s2} → lookup (honest a) k ≡ true
               → (k , s1) ∈ˡ ((i , snap₀) ∷ sigs a) → (k , s2) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
        newDed _  (here e1)  (here e2)  _  = trans (cong proj₂ e1) (sym (cong proj₂ e2))
        newDed _  (here e1)  (there m2) n≡ =
          ⊥-elim (fresh (trans (sym n≡) (cong Snapshot.number (cong proj₂ e1)))
                        (subst (λ p → (p , _) ∈ˡ sigs a) (cong proj₁ e1) m2))
        newDed _  (there m1) (here e2)  n≡ =
          ⊥-elim (fresh (trans n≡ (cong Snapshot.number (cong proj₂ e2)))
                        (subst (λ p → (p , _) ∈ˡ sigs a) (cong proj₁ e2) m1))
        newDed hk (there m1) (there m2) n≡ = sigDedup hk m1 m2 n≡
        newPos : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → 0 < Snapshot.number snap
        newPos _  (here e)  = subst (0 <_) (sym (trans (cong Snapshot.number (cong proj₂ e)) numEq₀)) (s≤s z≤n)
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
        newChain _ (here e) = mkPredecessor
            (LocalState.confirmed (lookup (localOf a) i))
            (trans (cong Snapshot.number (cong proj₂ e)) numEq₀)
            (subst (λ z → Snapshot.txs (LocalState.confirmed (lookup (localOf a) i)) ⊆ˡ Snapshot.txs z)
                   (sym (cong proj₂ e)) ext⊆₀)
            preGenesisOrCert
          where
            preGenesisOrCert : (Snapshot.number (LocalState.confirmed (lookup (localOf a) i)) ≡ 0)
               ⊎ Certified (record a { sigs = (i , snap₀) ∷ sigs a }) (LocalState.confirmed (lookup (localOf a) i))
            preGenesisOrCert with confCert hi₀
            ... | inj₁ (n , _) = inj₁ n
            ... | inj₂ c       = inj₂ (Certified-mono a {x = (i , snap₀)} c)
        newChain hk (there m) with sigChain hk m
        ... | mkPredecessor pre numberSuc txsExtend (inj₁ z) = mkPredecessor pre numberSuc txsExtend (inj₁ z)
        ... | mkPredecessor pre numberSuc txsExtend (inj₂ c) =
              mkPredecessor pre numberSuc txsExtend (inj₂ (Certified-mono a {x = (i , snap₀)} c))
    invStep {a} (signCorrupt {i = i} {snap = snap₀} ci)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain } = record
      { sigApp = newApp ; sigDedup = newDed ; confApp = confApp
      ; sigPos = newPos ; confCert = newCert ; sigChain = newChain }
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
    invStep {a} (confirm {i = c} {snap = snap₀} aggOK)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain } = record
      { sigApp = sigApp ; sigDedup = sigDedup ; confApp = newConfApp
      ; sigPos = sigPos ; confCert = newCert ; sigChain = sigChain }
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
    invStep {a} (corrupt i₀)
            record { sigApp = sigApp ; sigDedup = sigDedup ; confApp = confApp
                   ; sigPos = sigPos ; confCert = confCert ; sigChain = sigChain } = record
      { sigApp   = λ {k} {snap} hk mem → sigApp (honest-mono (honest a) i₀ k hk) mem
      ; sigDedup = λ {k} {s1} {s2} hk m1 m2 n≡ → sigDedup (honest-mono (honest a) i₀ k hk) m1 m2 n≡
      ; confApp  = λ {i} hi → confApp (honest-mono (honest a) i₀ i hi)
      ; sigPos   = λ {k} {snap} hk mem → sigPos (honest-mono (honest a) i₀ k hk) mem
      ; confCert = λ {i} hi → confCert (honest-mono (honest a) i₀ i hi)
      ; sigChain = λ {k} {snap} hk mem → sigChain (honest-mono (honest a) i₀ k hk) mem }
    -- `finalize` changes only `onChain`, and `see` only `seen`; no `Inv` field mentions either, so each
    -- field's type is unchanged — re-pack the same proofs (the record is nominal in `sys`, so we cannot
    -- return `inv` directly even though the fields coincide).
    invStep {a} (finalize _ _) inv = record
      { sigApp = Inv.sigApp inv ; sigDedup = Inv.sigDedup inv ; confApp = Inv.confApp inv
      ; sigPos = Inv.sigPos inv ; confCert = Inv.confCert inv ; sigChain = Inv.sigChain inv }
    invStep {a} (see) inv = record
      { sigApp = Inv.sigApp inv ; sigDedup = Inv.sigDedup inv ; confApp = Inv.confApp inv
      ; sigPos = Inv.sigPos inv ; confCert = Inv.confCert inv ; sigChain = Inv.sigChain inv }

-- ── Derived corollaries of the invariant ───────────────────────────────────────────────────────
-- L3 (applicability), exposed: every honest party's confirmed snapshot is applicable to U₀.
conf-applicable : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
conf-applicable sys reach = Inv.confApp (invariant sys reach)

-- L3 for certified snapshots: a CERTIFIED snapshot is applicable to U₀, witnessed by any honest
-- party (who, by `Certified`, signed it, and whose signatures are only on applicable snapshots).
cert-applicable : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → Applicable (U₀ sys) (Snapshot.txs snap)
cert-applicable sys reach {h} hh cert = Inv.sigApp (invariant sys reach) hh (cert h)

-- L1 (agreement at a number): two certified snapshots of the same number are equal, witnessed by
-- any honest party (who signed both, by `Certified`, and signs ≤1 per number, by `sigDed`).
agree : ∀ sys → Reachable sys → ∀ {h s1 s2} → lookup (honest sys) h ≡ true
  → Certified sys s1 → Certified sys s2 → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
agree sys reach {h} hh c1 c2 = Inv.sigDedup (invariant sys reach) hh (c1 h) (c2 h)

-- A certified snapshot has number > 0 (an honest party signed it, and honest signing is for the
-- snapshot one above its confirmed number, hence ≥ 1).
cert-pos : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → 0 < Snapshot.number snap
cert-pos sys reach {h} hh cert = Inv.sigPos (invariant sys reach) hh (cert h)

-- An honest party's confirmed snapshot is the genesis (number 0, txs []) or is certified.
confCert-of : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
    ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
confCert-of sys reach = Inv.confCert (invariant sys reach)

-- Every honest signature on `snap` has an extending certified-or-genesis predecessor `pre`.
sigChain-of : ∀ sys → Reachable sys → ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
  → PredecessorWitness (Certified sys) snap
sigChain-of sys reach = Inv.sigChain (invariant sys reach)

-- ── L2: now DERIVED (no longer a postulate) ─────────────────────────────────────────────────────
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

cert-nest : ∀ sys → Reachable sys → ∀ {h s1 s2}
  → lookup (honest sys) h ≡ true → Certified sys s1 → Certified sys s2
  → Snapshot.number s1 ≤ Snapshot.number s2 → Snapshot.txs s1 ⊆ˡ Snapshot.txs s2
cert-nest sys reach {h} {s1} {s2} hh c1 c2 le =
  cert-nest-aux sys reach (Snapshot.number s2 ∸ Snapshot.number s1) hh c1 c2 (m+[n∸m]≡n le)

-- L2, the §7 nesting obligation, now DERIVED: two honest parties' confirmed snapshots nest by number.
-- An honest party's confirmed snapshot is the genesis (txs ⊆ anything) or certified; in the latter
-- case `cert-nest` applies.
confirmed-nest : ∀ sys → Reachable sys → ∀ i j
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → confirmedNo (lookup (localOf sys) i) ≤ confirmedNo (lookup (localOf sys) j)
  → confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
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

consistency : Consistency
consistency sys reach i j hi hj =
  nested , conf-applicable sys reach hi , conf-applicable sys reach hj
  where
    nested : (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j))
           ⊎ (confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
    nested with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
    ... | inj₁ le = inj₁ (confirmed-nest sys reach i j hi hj le)
    ... | inj₂ ge = inj₂ (confirmed-nest sys reach j i hj hi ge)

-- ── A5: Consistency over once-honest-then-corrupt parties ────────────────────────────────────────
-- The §7 random variables T̄ᵢ are the txs party i confirmed _while uncorrupted_; `consistency` above
-- covers only CURRENTLY-honest parties. We extend it to ANY party (including one corrupted after it
-- confirmed -- whose confirmed snapshot an on-chain close could be built against). The key: EVERY
-- confirmed snapshot is certified-or-genesis UNCONDITIONALLY (`confCert-all`), because `confirm`
-- requires an `AggVerified` multisignature regardless of the confirmer's honesty; and any two certified
-- snapshots nest / are applicable via a single honest witness (`cert-nest`/`cert-applicable`). So a
-- once-honest party's confirmed set stays consistent with every other party's, as long as ≥1 honest
-- party exists. (This is in fact STRONGER than the literal §7 "while uncorrupted" scoping: because
-- `confirm` requires certification regardless of honesty, it also covers any snapshot a corrupt party
-- adopts AFTER corruption -- those too must be certified, hence consistent.) Standalone (does not
-- perturb the `invariant`/`consistency` core).

-- Every party's confirmed snapshot is the genesis or is certified -- with NO honesty hypothesis on the
-- party (the honest-only version is the `confCert` invariant component). The only step that changes a
-- party's confirmed snapshot is `confirm`, which requires `AggVerified` ⇒ (`ms-unforgeable`) certified.
confCert-all : ∀ sys → Reachable sys → ∀ i
  → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
    ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
confCert-all sys (base (_ , cn≡0 , ct≡[])) i = inj₁ (cn≡0 i , ct≡[] i)
confCert-all sys (step {s} r tr) = cc tr (confCert-all s r)
  where
    cc : ∀ {a b} → a ⟶ˢ b
       → (∀ i → (confirmedNo (lookup (localOf a) i) ≡ 0 × confirmedTxs (lookup (localOf a) i) ≡ [])
                ⊎ Certified a (LocalState.confirmed (lookup (localOf a) i)))
       → (∀ i → (confirmedNo (lookup (localOf b) i) ≡ 0 × confirmedTxs (lookup (localOf b) i) ≡ [])
                ⊎ Certified b (LocalState.confirmed (lookup (localOf b) i)))
    cc {a} (signHonest {i = signer} {snap = snap₀} _ _ _ _ _ _) ih i with ih i
    ... | inj₁ p = inj₁ p
    ... | inj₂ c = inj₂ (Certified-mono a {x = signer , snap₀} c)
    cc {a} (signCorrupt {i = signer} {snap = snap₀} _) ih i with ih i
    ... | inj₁ p = inj₁ p
    ... | inj₂ c = inj₂ (Certified-mono a {x = signer , snap₀} c)
    cc {a} (confirm {i = c} {snap = snap₀} aggOK) ih i with c FinP.≟ i
    ... | yes refl = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update c (localOf a) (record (lookup (localOf a) c) { confirmed = snap₀ })))
                           (inj₂ (ms-unforgeable a snap₀ aggOK))
    ... | no  c≢i  = subst (λ w → (confirmedNo w ≡ 0 × confirmedTxs w ≡ []) ⊎ Certified a (LocalState.confirmed w))
                           (sym (lookup∘update′ (λ e → c≢i (sym e)) (localOf a) (record (lookup (localOf a) c) { confirmed = snap₀ })))
                           (ih i)
    cc {a} (corrupt _)    ih i = ih i
    cc {a} (finalize _ _) ih i = ih i
    cc {a} (see)          ih i = ih i

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

-- §7 Consistency, extended to once-honest-then-corrupt parties: ANY two parties' confirmed sets nest
-- and are applicable to U₀, given ≥1 honest witness. (`consistency` is the special case i, j honest.)
consistency-uncorrupted : ∀ sys → Reachable sys → ∀ {h} → lookup (honest sys) h ≡ true → ∀ i j
  → (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
       ⊎ confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
   × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
   × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) j))
consistency-uncorrupted sys reach hh i j =
  nested , appU sys reach hh i , appU sys reach hh j
  where
    nested : (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j))
           ⊎ (confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
    nested with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
    ... | inj₁ le = inj₁ (nestU sys reach hh i j le)
    ... | inj₂ ge = inj₂ (nestU sys reach hh j i ge)

-- A non-⊥ Maybe is some `just`.
≢nothing→just : ∀ {A : Set} (m : Maybe A) → ¬ (m ≡ nothing) → Σ[ x ∈ A ] (m ≡ just x)
≢nothing→just (just x) _  = x , refl
≢nothing→just nothing  ¬n = ⊥-elim (¬n refl)

-- ── Seen-set invariant: every honest signature is on txs that party has SEEN ───────────────────
-- `Snapshot.txs snap ⊆ lookup seen k` for any honest `k` that signed `snap`. From `signHonest`'s seen
-- guard; `see` only GROWS a party's seen set (membership is preserved by the `++`); corruption only
-- shrinks the honest set; the other steps leave `sigs`/`seen`/`honest` unchanged. A standalone
-- induction over `Reachable`, kept separate from `invariant` so the safety core stays untouched.
sigSeen-inv : ∀ sys → Reachable sys → ∀ {k snap}
  → lookup (honest sys) k ≡ true → Signed sys k snap
  → Snapshot.txs snap ⊆ˡ lookup (seen sys) k
sigSeen-inv sys (base (sg≡[] , _ , _)) {k} {snap} _ mem =
  ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) sg≡[] mem))
sigSeen-inv sys (step {s} r tr) = ss tr (sigSeen-inv s r)
  where
    ss : ∀ {a b} → a ⟶ˢ b
       → (∀ {k snap} → lookup (honest a) k ≡ true → Signed a k snap → Snapshot.txs snap ⊆ˡ lookup (seen a) k)
       → (∀ {k snap} → lookup (honest b) k ≡ true → Signed b k snap → Snapshot.txs snap ⊆ˡ lookup (seen b) k)
    ss {a} (signHonest {i = i} {snap = snap₀} _ _ _ _ _ seen⊆₀) ih {k} {snap} hk (here e) =
      subst (λ p → Snapshot.txs (proj₂ p) ⊆ˡ lookup (seen a) (proj₁ p)) (sym e) seen⊆₀
    ss {a} (signHonest _ _ _ _ _ _)              ih {k} {snap} hk (there m) = ih hk m
    ss {a} (signCorrupt {i = i} {snap = snap₀} ci) ih {k} {snap} hk (here e) =
      ⊥-elim (trueNotFalse (trans (sym (subst (λ p → lookup (honest a) p ≡ true) (cong proj₁ e) hk)) ci))
    ss {a} (signCorrupt _)  ih {k} {snap} hk (there m) = ih hk m
    ss {a} (confirm _)      ih hk mem = ih hk mem
    ss {a} (corrupt i₀)     ih {k} hk mem = ih (honest-mono (honest a) i₀ k hk) mem
    ss {a} (finalize _ _)   ih hk mem = ih hk mem
    ss {a} (see {i = i} {txs = txs}) ih {k} {snap} hk mem with i FinP.≟ k
    ... | no  i≢k  = subst (λ w → Snapshot.txs snap ⊆ˡ w)
                           (sym (lookup∘update′ (λ e → i≢k (sym e)) (seen a) (txs ++ lookup (seen a) i)))
                           (ih hk mem)
    ... | yes refl = subst (λ w → Snapshot.txs snap ⊆ˡ w)
                           (sym (lookup∘update i (seen a) (txs ++ lookup (seen a) i)))
                           (λ {x} x∈ → ∈-++⁺ʳ txs (ih hk mem x∈))

soundness : Soundness
soundness sys reach {snap = snap} hh aggOK =
  let cert      = ms-unforgeable sys snap aggOK
      finalJust = ≢nothing→just (Ufinal sys snap) (cert-applicable sys reach hh cert)
  in proj₁ finalJust , proj₂ finalJust , (λ {j} hj → sigSeen-inv sys reach hj (cert j))

completeness : Completeness
completeness = confirmed-nest

-- `Reflects` is now CONSTRUCTED, not assumed -- and SOUNDLY so. Given a system that has finalized
-- against a snapshot whose aggregate multisignature verifies (`AggVerified`; the `finalize` step
-- supplies the snapshot-number match `numEq`), the conflict-freedom conjunct is DERIVED from
-- `soundness` (an honest party signed the certified snapshot, and honest signatures are applicable to
-- U₀), and the number match is the finalize witness. The accumulator commitment is supplied as the
-- explicit per-finalization hypothesis `ηEq` -- the irreducible SIGNATURE-TRUST assumption (νHead
-- authenticates η via `msVfy` over cid‖v‖s‖η#, NOT by recomputing `accUTxO(U)`: `closeValid`/
-- `fanoutValid` only check `η# ≡ hash (ηOf d')` plus the multisig). It is given as a hypothesis,
-- NOT a global postulate, on purpose: `finalize` admits any datum with a matching snapshot number, so
-- a global `∀ sys → … → ηOf ≡ accUTxO (outsOf U)` would have no model (two finalizations with the same
-- final U but different stored η). The finalizer discharges `ηEq` from the η it actually committed.
-- The earlier code took the WHOLE `Reflects` as an opaque hypothesis that nothing produced; here two
-- of its three conjuncts are derived and only the signature-trust conjunct is assumed.
reflects : ∀ sys → Reachable sys → ∀ {h snap}
  → lookup (honest sys) h ≡ true
  → AggVerified sys snap
  → OC.snapNum (onChain sys) ≡ Snapshot.number snap
  → (∀ U → Ufinal sys snap ≡ just U → OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))
  → Reflects sys snap
reflects sys reach {snap = snap} hh aggOK numEq ηEq =
  let s        = soundness sys reach hh aggOK
      finalEq  = proj₁ (proj₂ s)
  in mkReflects (proj₁ s) finalEq numEq (ηEq (proj₁ s) finalEq)

-- Reflected Soundness: when the on-chain datum reflects a finalized snapshot, its committed
-- accumulator commits to the conflict-free off-chain final UTxO U₀ ∘ (txs snap).
reflect-sound : ∀ sys → Reachable sys → ∀ {snap} → Reflects sys snap
  → Σ[ U ∈ UTxO ] (Ufinal sys snap ≡ just U)
                × (OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))
reflect-sound sys reach (mkReflects U conflictFree _ accCommits) = U , conflictFree , accCommits

-- The key link: the on-chain fanout distributes only outputs of the off-chain final UTxO. Its
-- membership-verified outputs (`OC.fanoutMembersOK`, i.e. `accVerify η outs π ≡ true`) are, by the
-- accumulator soundness law and the reflection bridge, a subset of outsOf(U). This ties the
-- on-chain `fanoutValid` distribution to the off-chain Soundness UTxO.
reflect-fanout-⊆ : ∀ sys → ∀ {U outs π}
  → OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U)
  → OC.fanoutMembersOK (OC.ηOf (onChain sys)) outs π
  → outs ⊆ outsOf U
reflect-fanout-⊆ sys {U} {outs} {π} η≡ mem =
  OC.accVerify-sound (subst (λ z → OC.accVerify z outs π ≡ true) η≡ mem)

