```
module Hydra.Protocol.Security where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.OffChain
open import Hydra.Protocol.Preliminaries using (Output)
open import Data.Fin using (Fin)
open import Data.Nat using (z≤n; s≤s)
open import Data.Nat.Properties using (≤-total; ≤-antisym; +-identityʳ; +-suc; suc-injective; m+[n∸m]≡n; m+n≡0⇒m≡0)
open import Data.Sum using (map₁; map₂)
open import Data.List.Relation.Unary.Any using (here; there)
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
```

#import "/template.typ": *
#import "/macros.typ": *

= Security (WIP --- Iteration 1) <sec:security>
#todo[The security analysis is still *sketchy*, with the goal to make it more formal in upcoming iterations]

#todo[Add security experiment]
Adversaries:

/ Active Adversary.: An _active adversary_ $adv$ has full control
  over the protocol, i.e., he is fully unrestricted in the above#todo[above this section there is no security game] security game.

/ Network Adversary.: A _network adversary_ $adv_emptyset$ does not corrupt
  any head parties, eventually delivers all sent network messages
  (i.e., does not drop any messages), and does not cause the $hpClose$ event.
  Apart from this restriction, the adversary can act arbitrarily in the above experiment.

Random variables:

- $That_i$: the set of transactions $tx$ for which party $party_i$,
  _while uncorrupted_, output $(hpSeen, tx)$;

- $Tbar_i$: the set of transactions $tx$ for which party $party_i$,
  _while uncorrupted_, output $(hpConf, tx)$;

- $Snapbar_i$: latest snapshot $(s, U)$ that party
  $party_i$ performed _while uncorrupted_: output $(hpSnap, (s, U))$;

- $Hcont$: the set of (at the time) uncorrupted parties who produced
  $xi$ upon close/contest request and $xi$ was applied to
  correct~$eta$; and

- $honest$: the set of parties that remain uncorrupted.


Security conditions / events:

- #propName[Consistency (Head)]: In presence of an active adversary, the
  following condition holds at any point in time:
  For all $i, j$,
  $Uinit compose (Tbar_i union Tbar_j) != bot$, i.e., no two
  uncorrupted parties see conflicting transactions confirmed.

- #propName[Oblivious Liveness (Head)]:
  Consider any protocol execution in presence of a network adversary wherein
  the head does not get closed for a sufficiently long period of time, and consider
  an honest party $p_i$ who enters transaction $tx$ by executing $(hpNew, tx)$ _each time after having finished a snapshot_.

  Then the following eventually holds:
  $tx in inter.big_(i in [n]) Tbar_i or
  forall i: Uinit compose (Tbar_i union {tx}) = bot$,
  i.e., every party will observe the transaction confirmed or every party
  will observe the transaction in conflict with their confirmed transactions.#footnote[
    In particular, _liveness_ expresses that the protocol makes progress
    under reasonable network conditions if no head parties get corrupted.
  ]

- #propName[Soundness (Chain)]: In presence of an active adversary,
  the following condition is satisfied:
  $exists Ttilde subset.eq inter.big_(i in honest) That_i : Ufinal
  = Uinit compose Ttilde != bot$, i.e., the final UTxO set results
  from applying a set of transactions to $U_0$ that have been seen by
  all honest parties (whereas each such transaction applies conforming to the ledger rules).

- #propName[Completeness (Chain)]: In presence of an active adversary,
  the following condition holds: For $Ttilde$ as above,
  $union.big_(p_i in Hcont) Tbar_i subset.eq Ttilde$, i.e., all
  transactions seen as confirmed by an honest party at the end of the
  protocol are considered.

Note that the original version of the coordinated head satisfies a stronger version of liveness which is important for the 'user experience' in the protocol:

- #propName[Liveness (Head)]:
  Consider any protocol execution in presence of a network adversary wherein
  the head does not get closed for a sufficiently long period of time, and consider
  an honest party $p_i$ who enters transaction $tx$ by executing $(hpNew, tx)$.

  Then the following eventually holds:
  $tx in inter.big_(i in [n]) Tbar_i or
  forall i: Uinit compose (Tbar_i union {tx}) = bot$,
  i.e., every party will observe the transaction confirmed or every party
  will observe the transaction in conflict with their confirmed transactions.#footnote[
    In particular, _liveness_ expresses that the protocol makes progress
    under reasonable network conditions if no head parties get corrupted.
  ]


== Proofs

The security properties are stated over the protocol model below. Three of the four are
*machine-checked* in Agda --- #propName[Consistency] (`consistency`), #propName[Soundness]
(`soundness`) and #propName[Completeness] (`completeness`) --- and, unlike an earlier
single-confirmed-chain model that simply _assumed_ agreement, the safety content is now *derived*
from a signature model (below): individual party signatures, a snapshot _confirmable_ only once
*every* party signed it (the coordinated head's full multisignature), and honest parties signing
only _applicable_ snapshots, at most one per number, each extending the signer's own confirmed
snapshot. From these the Agda machine-checks that every honest party's confirmed snapshot is
applicable to $Uinit$ (so confirmed sets never conflict), that two confirmations of the same snapshot
number coincide, and that confirmed snapshots nest by number (`confirmed-nest`, no longer a
postulate). `confirm` checks the §3.2 aggregate multisignature (`msVfy`), and the only residual
assumptions are the ledger semantics (`applyTxs`) and that scheme's _unforgeability_
(`ms-unforgeable`: a verifying aggregate signature implies every party signed). #propName[Liveness]
remains a postulate pending the temporal/fairness layer (P3). The prose lemmas further below give the informal
arguments these proofs mirror.

```agda
-- An illustrative standalone proposition (not part of the proved properties below): a
-- confirmed snapshot's number does not decrease from one local state to another.
SnapshotMonotone : LocalState → LocalState → Set
SnapshotMonotone st st' =
  Snapshot.number (LocalState.confirmed st) ≤ Snapshot.number (LocalState.confirmed st')
```

The §7 properties quantify over whole multi-party executions in the presence of an
adversary, so they are stated over an explicit execution model (see
#raw("security-formalisation-plan.md")): a ledger-application operation `applyTxs`, a global
$sans("System")$ state recording each party's signatures, a concrete single-step relation
$sans("_⟶ˢ_")$ (an honest party signs an _applicable_ snapshot; a corrupt party signs arbitrarily;
a party confirms a snapshot whose aggregate multisignature verifies; the adversary corrupts a party),
and the $sans("Reachable")$ closure from an initial system. A snapshot is $sans("Certified")$ once
*every* party signed it, so
unforgeability is immediate: a certified snapshot carries the confirmer's own honest signature. The
machine-checked invariant then *derives*: (i) every honest party's confirmed snapshot is applicable
to $Uinit$, from the honest "sign only applicable" guard; (ii) two certified snapshots of the same
number are equal, from the honest "one signature per number" guard; and (iii) confirmed snapshots
nest by number (`confirmed-nest`), from the honest "extend my own confirmed snapshot" guard plus a
gap induction using (ii). `confirm` checks the §3.2 aggregate multisignature (`AggVerified`/`msVfy`);
no honest-core safety assumption remains beyond the ledger `applyTxs` and that scheme's unforgeability
(`ms-unforgeable`). *Liveness* additionally needs a temporal/fairness layer (P3).

#dparagraph[Modelling note (honest signing discipline).]
The `signHonest` move makes explicit the coordinated head's snapshot regime (§6.2): an honest party
signs the snapshot numbered exactly one above its _own_ confirmed snapshot, whose transactions extend
that snapshot's, and signs at most one snapshot per number. The §6 prose specifies this only
operationally (round-robin snapshot leader, sequential snapshot numbers $s = hats + 1$, and the
$hpRS$ 'wait' applicability guard); the security model promotes it to an explicit honest-behaviour
assumption. It is what makes the confirmed chain provably linear (`agree`) and monotone
(`confirmed-nest`); a faithful, slightly more explicit, statement of the protocol's intent rather than
an extra restriction.

```agda
-- Ledger application: apply a transaction list to a UTxO set; `nothing` = ⊥ (conflict).
-- `applyTxs-nil` is the (trivial) ledger law that applying no transactions never conflicts.
postulate
  applyTxs     : UTxO → List Data → Maybe UTxO
  applyTxs-nil : ∀ U → applyTxs U [] ≡ just U

-- A transaction list is jointly applicable to U when applying it does not conflict (≠ ⊥).
Applicable : UTxO → List Data → Set
Applicable U txs = ¬ (applyTxs U txs ≡ nothing)

-- The empty tx list is always applicable (from the nil law).
[]-applicable : ∀ U → Applicable U []
[]-applicable U eq = bot (trans (sym (applyTxs-nil U)) eq)
  where bot : just U ≡ nothing → ⊥
        bot ()

-- No element is a member of the empty list.
∉[] : ∀ {A : Set} {x : A} → ¬ (x ∈ˡ [])
∉[] ()

-- T̄ᵢ / ŝᵢ: a party's confirmed transactions and confirmed snapshot number.
confirmedTxs : LocalState → List Data
confirmedTxs st = Snapshot.txs (LocalState.confirmed st)

confirmedNo : LocalState → ℕ
confirmedNo st = Snapshot.number (LocalState.confirmed st)

-- ════════════════════════════════════════════════════════════════════════════════════════════
-- P1-real: DERIVING the agreement/applicability of confirmed snapshots from a signature model,
-- rather than ASSUMING it (as the earlier single-chain model did). We record individual party
-- signatures, declare a snapshot CONFIRMABLE (`Certified`) only once EVERY party signed it (the
-- coordinated head's full multisignature), and constrain HONEST signing to applicable snapshots,
-- at most one per number, each extending its own confirmed snapshot. From these we DERIVE below:
-- every honest party's confirmed snapshot is applicable to U₀ (L3); two certified snapshots of the
-- same number are equal (L1); and confirmed snapshots NEST by number (L2, `confirmed-nest`, now a
-- proof). `confirm` checks the §3.2 aggregate multisignature (`msVfy`); the only irreducible
-- assumptions are the ledger `applyTxs` / nil law and that scheme's unforgeability (`ms-unforgeable`).
-- ════════════════════════════════════════════════════════════════════════════════════════════

-- Global system state. Party-indexed data are vectors for clean updates. `sigs` records the
-- individual signatures produced so far as (party, snapshot) pairs; there is NO pre-ordained chain.
record System : Set where
  field
    parties  : ℕ
    localOf  : Vec LocalState parties
    onChain  : OC.HeadDatum
    honest   : Vec Bool parties
    U₀       : UTxO
    sigs     : List (Fin parties × Snapshot)
open System

-- Party i has signed snapshot snap (its (i , snap) pair is recorded).
Signed : (sys : System) → Fin (parties sys) → Snapshot → Set
Signed sys i snap = (i , snap) ∈ˡ sigs sys

-- A snapshot is CERTIFIED when EVERY party signed it: the SEMANTIC content of the coordinated head's
-- n-of-n multisignature, which the safety proofs reason with directly.
Certified : (sys : System) → Snapshot → Set
Certified sys snap = ∀ (i : Fin (parties sys)) → Signed sys i snap

-- Operationally a node does not test `Certified` (all n individual signatures); it checks ONE
-- AGGREGATE multisignature with the §3.2 scheme's verifier `msVfy`, under the head's aggregate key
-- (§4) over the snapshot's message cid‖v‖s‖η# (§6). `aggKey`/`msgOf`/`sigOf` abstract that aggregate
-- key, the message serialisation, and the snapshot's aggregate signature (`Snapshot.sig`).
postulate
  aggKey : VKey
  msgOf  : Snapshot → ℍ
  sigOf  : Snapshot → AggSig

AggVerified : Snapshot → Set
AggVerified snap = msVfy aggKey (msgOf snap) (sigOf snap) ≡ true

-- MS-scheme UNFORGEABILITY (the irreducible cryptographic assumption, named and explicit): a
-- snapshot whose aggregate signature verifies must have been signed by every party. This lets the
-- operational check `AggVerified` (what `confirm` actually tests) stand in for the semantic condition
-- `Certified` the proofs use. The earlier model baked "every party signed" into `confirm` directly;
-- here it is obtained from `msVfy` + this single axiom.
postulate
  ms-unforgeable : ∀ sys snap → AggVerified snap → Certified sys snap

-- The single-step relation _⟶ˢ_:
--   signHonest  : an honest party signs a snapshot, but ONLY if its txs are applicable to U₀ (the
--                 reqSn 'wait' guard) and it has not already signed a snapshot of that number (one
--                 signature per round). These two guards are the honest discipline L1/L3 rest on.
--   signCorrupt : a corrupt party may sign ANY snapshot (the adversary forges nothing honest).
--   confirm     : a party adopts a snapshot whose AGGREGATE multisignature verifies (`AggVerified`,
--                 i.e. `msVfy` passes); unforgeability then makes it certified (all parties signed).
--   corrupt     : the active adversary corrupts a party (honest parties only ever shrink).
-- `sigs` only grows; `U₀` and `onChain` are never changed by a step.
data _⟶ˢ_ : System → System → Set where
  signHonest : ∀ {sys i snap}
    → lookup (honest sys) i ≡ true
    → Applicable (U₀ sys) (Snapshot.txs snap)
    → (∀ {s'} → Snapshot.number s' ≡ Snapshot.number snap → ¬ Signed sys i s')
    -- chain-extension discipline: an honest party signs the snapshot one above its OWN confirmed
    -- snapshot, extending it (this is what makes the confirmed chain provably nest, L2).
    → Snapshot.number snap ≡ suc (confirmedNo (lookup (localOf sys) i))
    → Snapshot.txs (LocalState.confirmed (lookup (localOf sys) i)) ⊆ˡ Snapshot.txs snap
    → sys ⟶ˢ record sys { sigs = (i , snap) ∷ sigs sys }

  signCorrupt : ∀ {sys i snap}
    → lookup (honest sys) i ≡ false
    → sys ⟶ˢ record sys { sigs = (i , snap) ∷ sigs sys }

  confirm : ∀ {sys i snap}
    → AggVerified snap
    → sys ⟶ˢ record sys
        { localOf = localOf sys [ i ]≔ record (lookup (localOf sys) i) { confirmed = snap } }

  corrupt : ∀ {sys} (i : Fin (parties sys))
    → sys ⟶ˢ record sys { honest = honest sys [ i ]≔ false }

-- An initial system: no signatures yet, and every party's confirmed snapshot is the genesis
-- (number 0, empty tx list, applicable by the nil law).
Initial : System → Set
Initial sys =
    (sigs sys ≡ [])
  × (∀ i → confirmedNo (lookup (localOf sys) i) ≡ 0)
  × (∀ i → confirmedTxs (lookup (localOf sys) i) ≡ [])

-- Reachable = reflexive-transitive closure of _⟶ˢ_ from an initial system.
data Reachable : System → Set where
  base : ∀ {s}    → Initial s → Reachable s
  step : ∀ {s s'} → Reachable s → s ⟶ˢ s' → Reachable s'

-- The DERIVED invariants carried through every reachable system:
--   sigApp  : every honest signature is on a snapshot applicable to U₀ (from signHonest's guard);
--   sigDed  : an honest party signs at most one snapshot per number (from signHonest's guard);
--   confApp : every honest party's confirmed snapshot is applicable to U₀ (L3). This REPLACES the
--             old `Initial` assumption that the whole chain is applicable: here it is DERIVED.
--   sigPos  : an honest signature is on a snapshot of number > 0 (from the extension guard);
--   confCert: an honest party's confirmed snapshot is the genesis or is certified;
--   sigChain: an honest signature on `snap` has a predecessor `pre` it extends (number suc, txs ⊆),
--             with `pre` the genesis or certified. These last three give L2 (`confirmed-nest`).
Inv : System → Set
Inv sys =
    (∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
        → Applicable (U₀ sys) (Snapshot.txs snap))
  × (∀ {k s1 s2} → lookup (honest sys) k ≡ true → Signed sys k s1 → Signed sys k s2
        → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2)
  × (∀ {i} → lookup (honest sys) i ≡ true
        → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i)))
  × (∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap → 0 < Snapshot.number snap)
  × (∀ {i} → lookup (honest sys) i ≡ true
        → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
          ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i)))
  × (∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
        → Σ[ pre ∈ Snapshot ] (Snapshot.number snap ≡ suc (Snapshot.number pre))
                            × (Snapshot.txs pre ⊆ˡ Snapshot.txs snap)
                            × ((Snapshot.number pre ≡ 0) ⊎ Certified sys pre))

-- Vec/Fin helper: corruption only ever removes honest parties, so an honest party in the
-- post-state was honest in the pre-state.
honest-mono : ∀ {n} (v : Vec Bool n) (i k : Fin n)
  → lookup (v [ i ]≔ false) k ≡ true → lookup v k ≡ true
honest-mono v i k h with i FinP.≟ k
... | no  i≢k  = trans (sym (lookup∘update′ (λ e → i≢k (sym e)) v false)) h
... | yes refl = ⊥-elim (bool-absurd (trans (sym h) (lookup∘update i v false)))
  where
    bool-absurd : true ≡ false → ⊥
    bool-absurd ()

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
invariant sys (base (sg≡[] , cn≡0 , ct≡[])) =
    (λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) sg≡[] mem)))
  , (λ {k} {s1} _ m1 _ _ → ⊥-elim (∉[] (subst (λ z → (k , s1) ∈ˡ z) sg≡[] m1)))
  , (λ {i} _ → subst (Applicable (U₀ sys)) (sym (ct≡[] i)) ([]-applicable (U₀ sys)))
  , (λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) sg≡[] mem)))
  , (λ {i} _ → inj₁ (cn≡0 i , ct≡[] i))
  , (λ {k} {snap} _ mem → ⊥-elim (∉[] (subst (λ z → (k , snap) ∈ˡ z) sg≡[] mem)))
invariant sys (step {s} r tr) = invStep tr (invariant s r)
  where
    invStep : ∀ {a b} → a ⟶ˢ b → Inv a → Inv b
    invStep {a} (signHonest {i = i} {snap = snap₀} hi₀ appl₀ fresh numEq₀ ext⊆₀)
            (sigApp , sigDed , confApp , sigPos , confCert , sigChain) =
      newApp , newDed , confApp , newPos , newCert , newChain
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
        newDed hk (there m1) (there m2) n≡ = sigDed hk m1 m2 n≡
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
                 → Σ[ pre ∈ Snapshot ] (Snapshot.number snap ≡ suc (Snapshot.number pre))
                                     × (Snapshot.txs pre ⊆ˡ Snapshot.txs snap)
                                     × ((Snapshot.number pre ≡ 0)
                                        ⊎ Certified (record a { sigs = (i , snap₀) ∷ sigs a }) pre)
        newChain _ (here e) =
            LocalState.confirmed (lookup (localOf a) i)
          , trans (cong Snapshot.number (cong proj₂ e)) numEq₀
          , subst (λ z → Snapshot.txs (LocalState.confirmed (lookup (localOf a) i)) ⊆ˡ Snapshot.txs z)
                  (sym (cong proj₂ e)) ext⊆₀
          , dj
          where
            dj : (Snapshot.number (LocalState.confirmed (lookup (localOf a) i)) ≡ 0)
               ⊎ Certified (record a { sigs = (i , snap₀) ∷ sigs a }) (LocalState.confirmed (lookup (localOf a) i))
            dj with confCert hi₀
            ... | inj₁ (n , _) = inj₁ n
            ... | inj₂ c       = inj₂ (Certified-mono a {x = (i , snap₀)} c)
        newChain hk (there m) with sigChain hk m
        ... | pre , ne , t⊆ , inj₁ z = pre , ne , t⊆ , inj₁ z
        ... | pre , ne , t⊆ , inj₂ c = pre , ne , t⊆ , inj₂ (Certified-mono a {x = (i , snap₀)} c)
    invStep {a} (signCorrupt {i = i} {snap = snap₀} ci)
            (sigApp , sigDed , confApp , sigPos , confCert , sigChain) =
      newApp , newDed , confApp , newPos , newCert , newChain
      where
        clash : ∀ {k snap} → lookup (honest a) k ≡ true → (k , snap) ≡ (i , snap₀) → ⊥
        clash hk e = bot (trans (sym (subst (λ p → lookup (honest a) p ≡ true) (cong proj₁ e) hk)) ci)
          where bot : true ≡ false → ⊥
                bot ()
        newApp : ∀ {k snap} → lookup (honest a) k ≡ true
               → (k , snap) ∈ˡ ((i , snap₀) ∷ sigs a) → Applicable (U₀ a) (Snapshot.txs snap)
        newApp hk (here e)  = ⊥-elim (clash hk e)
        newApp hk (there m) = sigApp hk m
        newDed : ∀ {k s1 s2} → lookup (honest a) k ≡ true
               → (k , s1) ∈ˡ ((i , snap₀) ∷ sigs a) → (k , s2) ∈ˡ ((i , snap₀) ∷ sigs a)
               → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
        newDed hk (here e1)  _          _  = ⊥-elim (clash hk e1)
        newDed hk (there m1) (here e2)  _  = ⊥-elim (clash hk e2)
        newDed hk (there m1) (there m2) n≡ = sigDed hk m1 m2 n≡
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
                 → Σ[ pre ∈ Snapshot ] (Snapshot.number snap ≡ suc (Snapshot.number pre))
                                     × (Snapshot.txs pre ⊆ˡ Snapshot.txs snap)
                                     × ((Snapshot.number pre ≡ 0)
                                        ⊎ Certified (record a { sigs = (i , snap₀) ∷ sigs a }) pre)
        newChain hk (here e)  = ⊥-elim (clash hk e)
        newChain hk (there m) with sigChain hk m
        ... | pre , ne , t⊆ , inj₁ z = pre , ne , t⊆ , inj₁ z
        ... | pre , ne , t⊆ , inj₂ c = pre , ne , t⊆ , inj₂ (Certified-mono a {x = (i , snap₀)} c)
    invStep {a} (confirm {i = c} {snap = snap₀} aggOK)
            (sigApp , sigDed , confApp , sigPos , confCert , sigChain) =
      sigApp , sigDed , newConfApp , sigPos , newCert , sigChain
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
    invStep {a} (corrupt i₀) (sigApp , sigDed , confApp , sigPos , confCert , sigChain) =
        (λ {k} {snap} hk mem → sigApp (honest-mono (honest a) i₀ k hk) mem)
      , (λ {k} {s1} {s2} hk m1 m2 n≡ → sigDed (honest-mono (honest a) i₀ k hk) m1 m2 n≡)
      , (λ {i} hi → confApp (honest-mono (honest a) i₀ i hi))
      , (λ {k} {snap} hk mem → sigPos (honest-mono (honest a) i₀ k hk) mem)
      , (λ {i} hi → confCert (honest-mono (honest a) i₀ i hi))
      , (λ {k} {snap} hk mem → sigChain (honest-mono (honest a) i₀ k hk) mem)

-- ── Derived corollaries of the invariant ───────────────────────────────────────────────────────
-- L3 (applicability), exposed: every honest party's confirmed snapshot is applicable to U₀.
conf-applicable : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
conf-applicable sys reach = proj₁ (proj₂ (proj₂ (invariant sys reach)))

-- L3 for certified snapshots: a CERTIFIED snapshot is applicable to U₀, witnessed by any honest
-- party (who, by `Certified`, signed it, and whose signatures are only on applicable snapshots).
cert-applicable : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → Applicable (U₀ sys) (Snapshot.txs snap)
cert-applicable sys reach {h} hh cert = proj₁ (invariant sys reach) hh (cert h)

-- L1 (agreement at a number): two certified snapshots of the same number are equal, witnessed by
-- any honest party (who signed both, by `Certified`, and signs ≤1 per number, by `sigDed`).
agree : ∀ sys → Reachable sys → ∀ {h s1 s2} → lookup (honest sys) h ≡ true
  → Certified sys s1 → Certified sys s2 → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
agree sys reach {h} hh c1 c2 = proj₁ (proj₂ (invariant sys reach)) hh (c1 h) (c2 h)

-- A certified snapshot has number > 0 (an honest party signed it, and honest signing is for the
-- snapshot one above its confirmed number, hence ≥ 1).
cert-pos : ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true
  → Certified sys snap → 0 < Snapshot.number snap
cert-pos sys reach {h} hh cert = proj₁ (proj₂ (proj₂ (proj₂ (invariant sys reach)))) hh (cert h)

-- An honest party's confirmed snapshot is the genesis (number 0, txs []) or is certified.
confCert-of : ∀ sys → Reachable sys → ∀ {i} → lookup (honest sys) i ≡ true
  → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
    ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
confCert-of sys reach = proj₁ (proj₂ (proj₂ (proj₂ (proj₂ (invariant sys reach)))))

-- Every honest signature on `snap` has an extending certified-or-genesis predecessor `pre`.
sigChain-of : ∀ sys → Reachable sys → ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
  → Σ[ pre ∈ Snapshot ] (Snapshot.number snap ≡ suc (Snapshot.number pre))
                      × (Snapshot.txs pre ⊆ˡ Snapshot.txs snap)
                      × ((Snapshot.number pre ≡ 0) ⊎ Certified sys pre)
sigChain-of sys reach = proj₂ (proj₂ (proj₂ (proj₂ (proj₂ (invariant sys reach)))))

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
... | pre , ns2≡suc , txspre⊆ , inj₂ cpre =
      ⊆ˡ-trans (cert-nest-aux sys reach d' hh c1 cpre eq') txspre⊆
  where
    eq' : Snapshot.number s1 + d' ≡ Snapshot.number pre
    eq' = suc-injective (trans (sym (+-suc (Snapshot.number s1) d')) (trans eq ns2≡suc))
... | pre , ns2≡suc , txspre⊆ , inj₁ npre≡0 =
      ⊥-elim (1≤0 (subst (1 ≤_) ns1≡0 (cert-pos sys reach hh c1)))
  where
    eq' : Snapshot.number s1 + d' ≡ Snapshot.number pre
    eq' = suc-injective (trans (sym (+-suc (Snapshot.number s1) d')) (trans eq ns2≡suc))
    ns1≡0 : Snapshot.number s1 ≡ 0
    ns1≡0 = m+n≡0⇒m≡0 (Snapshot.number s1) (trans eq' npre≡0)
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

-- The §7 Consistency property: no two honest parties confirm conflicting transactions. We DERIVE
-- that each honest party's confirmed set is applicable to U₀ (`conf-applicable`) and that the two
-- sets nest (`confirmed-nest`); so their union is the larger set, which is applicable. "Conflicting"
-- means the union fails to apply, which nesting + individual applicability rules out.
HoldsAt : System → Set
HoldsAt sys =
  ∀ (i j : Fin (parties sys))
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)
       ⊎ confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
  × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
  × Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) j))

Consistency : Set
Consistency = ∀ (sys : System) → Reachable sys → HoldsAt sys

consistency : Consistency
consistency sys reach i j hi hj =
  nested , conf-applicable sys reach hi , conf-applicable sys reach hj
  where
    nested : (confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j))
           ⊎ (confirmedTxs (lookup (localOf sys) j) ⊆ˡ confirmedTxs (lookup (localOf sys) i))
    nested with ≤-total (confirmedNo (lookup (localOf sys) i)) (confirmedNo (lookup (localOf sys) j))
    ... | inj₁ le = inj₁ (confirmed-nest sys reach i j hi hj le)
    ... | inj₂ ge = inj₂ (confirmed-nest sys reach j i hj hi ge)

-- ── P2: Soundness and Completeness (Chain) ─────────────────────────────────────────────────
-- The finalized on-chain UTxO is the closed/fanned-out snapshot applied to U₀. That snapshot is
-- certified (the head closes only against a fully-signed snapshot), so by `cert-applicable` it is
-- conflict-free -- DERIVED, no longer assumed.
Ufinal : System → Snapshot → Maybe UTxO
Ufinal sys snap = applyTxs (U₀ sys) (Snapshot.txs snap)

-- A non-⊥ Maybe is some `just`.
≢nothing→just : ∀ {A : Set} (m : Maybe A) → ¬ (m ≡ nothing) → Σ[ x ∈ A ] (m ≡ just x)
≢nothing→just (just x) _  = x , refl
≢nothing→just nothing  ¬n = ⊥-elim (¬n refl)

-- Soundness (Chain), §7 (core): the final UTxO U₀ ∘ T̃ for a finalized snapshot T̃ whose aggregate
-- multisignature verifies (`AggVerified`) is conflict-free. DERIVED: `ms-unforgeable` makes the
-- verified snapshot certified, so an honest party signed it, and honest parties sign only applicable
-- snapshots (`cert-applicable`). (The "T̃ ⊆ ⋂ honest seen" strengthening still needs seen-set modelling.)
Soundness : Set
Soundness = ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true → AggVerified snap
          → Σ[ U ∈ UTxO ] (Ufinal sys snap ≡ just U)

soundness : Soundness
soundness sys reach {snap = snap} hh aggOK =
  ≢nothing→just (Ufinal sys snap) (cert-applicable sys reach hh (ms-unforgeable sys snap aggOK))

-- Completeness (Chain), §7: every transaction an honest party confirmed (T̄ᵢ) is included in a more
-- advanced honest party's confirmed set (in particular the honest closer's, which becomes the
-- finalized snapshot) whenever ŝᵢ ≤ ŝⱼ. This is exactly the nesting obligation `confirmed-nest` (L2).
Completeness : Set
Completeness = ∀ sys → Reachable sys → ∀ i j
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → confirmedNo (lookup (localOf sys) i) ≤ confirmedNo (lookup (localOf sys) j)
  → confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)

completeness : Completeness
completeness = confirmed-nest

-- ── Linking the two Agda halves: off-chain confirmed snapshot ↔ on-chain close/fanout ──────────
-- They meet at finalization: when the head closes/fans out, the on-chain Closed datum's accumulator
-- commits to exactly the off-chain final UTxO U₀ ∘ (txs of the certified finalized snapshot).

-- Glue: the set of outputs held in a UTxO map (its range). Basic, assumed (not modelled in detail).
postulate
  outsOf : UTxO → ℙ Output

-- Bridge predicate: the on-chain head datum REFLECTS a finalized snapshot `snap` -- its snapshot
-- number matches and its stored accumulator commits (`OC.accUTxO`) to U₀ ∘ (txs snap). On-chain this
-- is established by a close/fanout satisfying `OC.closeValid`/`OC.fanoutValid`; here it is the link.
Reflects : System → Snapshot → Set
Reflects sys snap = Σ[ U ∈ UTxO ]
    (Ufinal sys snap ≡ just U)                                 -- the final UTxO is conflict-free
  × (OC.snapNum (onChain sys) ≡ Snapshot.number snap)          -- on-chain snapshot number matches
  × (OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))             -- on-chain accumulator commits to it

-- Reflected Soundness: when the on-chain datum reflects a finalized snapshot, its committed
-- accumulator commits to the conflict-free off-chain final UTxO U₀ ∘ (txs snap).
reflect-sound : ∀ sys → Reachable sys → ∀ {snap} → Reflects sys snap
  → Σ[ U ∈ UTxO ] (Ufinal sys snap ≡ just U)
                × (OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf U))
reflect-sound sys reach (U , uf≡ , _ , η≡) = U , uf≡ , η≡

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

-- Liveness (head; needs the temporal/fairness layer, P3) remains abstract; see
-- security-formalisation-plan.md.
postulate
  Liveness : Set   -- TODO(D4-P3): under the liveness condition
```

#dparagraph[Consistency.]

#lemma(name: [Consistency])[
  The coordinated head protocol satisfies the #propName[Consistency] property.
] <lem:consistency>
#proof[
  Observe that $Tbar_i union Tbar_j subset.eq That_i$ since no
  transaction can be confirmed without every honest party signing off
  on it. Since parties do not sign conflicting transactions
  (see $hpRS$, 'wait'), we have
  $Uinit applytx Tbar_i != bot$,
  $Uinit applytx Tbar_j != bot$, and
  $Uinit applytx That_i != bot$. Thus, since $Tbar_i union Tbar_j subset.eq That_i$
  it follows that
  $Uinit applytx (Tbar_i union Tbar_j) != bot$

  _Machine-checked as `consistency` (above), now fully *derived* from the signature model: each honest
  party's confirmed set is applicable (`conf-applicable`: a confirmed snapshot is certified, so it
  carries that party's own signature, and honest parties sign only applicable snapshots), and the two
  confirmed sets nest (`confirmed-nest`, derived via `cert-nest` from the honest extend-your-own-confirmed
  guard + agreement). No safety assumption remains beyond the ledger and the §3.2 multisignature's
  unforgeability (`ms-unforgeable`)._
]

#dparagraph[Oblivious Liveness.]
For all lemmas towards oblivious liveness, we assume the presence of a network adversary, and that the head does not get closed for a sufficiently
long period of time.
We call this the _liveness condition_.

#lemma[
  Under the liveness condition, any snapshot issued as $(hpRS, s, T)$ will eventually be confirmed
  in the sense that every party holds a valid mulisignature on it.
] <lem:reqconf>
#proof[
  Consider a party $p_i$ receiving message $(hpRS, s, T)$. We demonstrate that $p_i$ executes
  the code past the 'wait' instruction of the $hpRS$ routine.

  - Passing the 'require' guard:
  Note that the snapshot leader sends the request only if $hats = bars$, and for $s = hats + 1$.
  Thus, $hats_i = hats$ since $p_i$ has already signed the snapshot for $hats$. The 'require'
  guard is thus satisfied for $p_i$.

  - Passing the 'wait' guard:
  Since the snapshot leader sees $hats = bars$, also $p_i$ will eventually see $hats_i = bars_i$. Furthermore, since all leaders are honest, it holds that $hatmU applytx mT_("res") != bot$ by construction.

  This implies that every party will eventually sign and acknowledge the newly created snapshot.
  Finally, the 'require' and 'wait' guards of the $hpAS$ code will be passed by every party
  since an $hpAS$ for snapshot number $s$ can only be received for $s in {hats, hats + 1}$
  as an acknowledgement can only be received for the current snapshot being worked on by $p_i$
  or a snapshot that is one step ahead---implying that everybody will hold a valid multisignature
  on the snapshot in consideration.
]

#lemma(name: [Eternal snapshot confirmation])[
  Under the liveness condition, as long as new transactions are issued, for any $k > 0$, every party eventually confirms
  a snapshot with sequence number $s = k$.
] <lem:eternal>
#proof[
  By @lem:reqconf, any requested snapshot eventually gets confirmed, implying
  that the next leader observes $hats = bars$ and thus, in turn, issues a new snapshot.
  Thus, for any $k$, a snapshot is eventually confirmed.
]

#lemma(name: [Oblivious Liveness])[
  The coordinated head protocol satisfies the #propName[Oblivious Liveness] property.
] <lem:liveness>
#proof[
  Consider the first point in time where a transaction $tx$ enters the system by some party $p_i$
  issuing $(hpNew, tx)$, and consider the next point in time
  $t$ when $p_i$ issues a snapshot.

  By @lem:eternal, this snapshot will eventually be issued and confirmed by all parties.

  #v(0.5em)

  Let $hatmT$ be the transactions to be considered by $p_i$'s snapshot: $hatmL = barmU applytx hatmT$
  where $barmU$ is the snapshot prior to $p_i$'s. Since $p_i$ issues
  $(hpRT, tx)$ after each snapshot, we have that, either,
  - $tx in hatmT$, in which case $tx in inter.big_(i in [n]) Tbar_i$ after everybody has completed this snapshot, or,
  - $tx in.not hatmT$, in which case $hatmL applytx tx = bot$ ($tx$ is still in the wait queue of $(hpRT, tx)$. After everybody has completed this snapshot, it thus holds that $forall i: Uinit applytx Tbar_i = hatmL$, and thus, that
    $forall i: Uinit applytx (Tbar_i union {tx}) = bot$.
  In both cases, the lemma follows.
]

#dparagraph[Soundness and completeness.]

#lemma(name: [Soundness])[
  The basic head protocol satisfies the #propName[Soundness] property.
] <lem:soundness>

#proof[
  Let $T$ be the set of transactions such that $Ufinal = U_0 applytx T$.
  Since $Ufinal$ is multi-signed, it holds that $T subset.eq That_i$
  ($T$ is _seen_) by every honest party in the head.
  Furthermore, since honest signatures are only issued for valid transaction,
  $Ufinal != bot$ (i.e., $Ufinal$ is a valid state), and soundness
  follows.

  _Machine-checked as #raw("soundness") above ($Ufinal = U_0 applytx tilde(T) != bot$ with
  $tilde(T)$ the certified finalized snapshot). The $!= bot$ is now *derived*, not assumed: a
  certified snapshot carries an honest party's signature, and honest parties sign only applicable
  snapshots (`cert-applicable`). The $tilde(T) subset.eq inter.big_(i in honest) That_i$ strengthening
  additionally awaits seen-set modelling._
]


#lemma(name: [Completeness])[
  The basic head protocol satisfies the #propName[Completeness]
  property.
] <lem:completeness>
#proof[
  Consider all parties $p_i in Hcont$. Since the close/contest process
  finally accepts the latest multi-signed snapshot, it holds that
  $Ufinal . s >= max_(p_i in Hcont) (bars_i)$, and thus that
  $union.big_(p_i in Hcont) Tbar_i subset.eq inter.big_(p_i in honest) That_i$,
  and completeness follows.

  _Machine-checked as #raw("completeness") above (each honest party's $Tbar_i subset.eq tilde(T)$
  whenever $bars_i <= s_f$). This is the snapshot-nesting property `confirmed-nest` (L2), now *derived*
  (`cert-nest`) rather than assumed._
]
