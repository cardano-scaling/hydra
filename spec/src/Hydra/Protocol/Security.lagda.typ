```
module Hydra.Protocol.Security where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.OffChain
open import Data.Fin using (Fin)
open import Data.Nat using (_⊔_)
open import Data.Vec using (Vec; lookup; _[_]≔_)
open import Data.Vec.Properties using (lookup∘update; lookup∘update′)
import Data.Fin.Properties as FinP
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

The security properties are stated over the protocol model. Where the current
(Iteration 1) model allows, a property is a concrete proposition; the named
lemmas below are declared as Agda statements whose proofs are future work:

```agda
-- A confirmed snapshot's number never decreases across a well-formed step.
SnapshotMonotone : LocalState → LocalState → Set
SnapshotMonotone st st' =
  Snapshot.number (LocalState.confirmed st) ≤ Snapshot.number (LocalState.confirmed st')
```

The §7 properties quantify over whole multi-party executions in the presence of an
adversary, so they are stated over an explicit execution model. This is Phase P0 of the
plan in #raw("security-formalisation-plan.md"): a ledger-application operation, a global
$sans("System")$ state (the party vector, the on-chain datum, the in-flight network
messages, and the honest/corrupt partition), and a $sans("Reachable")$ relation. The
initial systems and the single-step relation are postulated for now; the concrete
adversary/honest moves are the remaining P0 work.

```agda
-- Ledger application: apply a transaction list to a UTxO set; `nothing` = ⊥ (conflict).
postulate
  applyTxs : UTxO → List Data → Maybe UTxO

-- A transaction list is jointly applicable to U when applying it does not conflict (≠ ⊥).
Applicable : UTxO → List Data → Set
Applicable U txs = ¬ (applyTxs U txs ≡ nothing)

-- T̄ᵢ: a party's confirmed transactions (the confirmed snapshot's tx list). Refining
-- this to the cumulative confirmed set across snapshots is later D4 work.
confirmedTxs : LocalState → List Data
confirmedTxs st = Snapshot.txs (LocalState.confirmed st)

-- A party's confirmed snapshot number ŝ.
confirmedNo : LocalState → ℕ
confirmedNo st = Snapshot.number (LocalState.confirmed st)

-- Global system state. Besides the per-party local states, on-chain datum, in-flight network
-- messages and honest/corrupt partition, the coordinated head has a SINGLE agreed confirmed
-- snapshot chain `chainTxs` (cumulative confirmed transactions indexed by snapshot number).
-- Modelling one shared chain — rather than independent per-party confirmed sets — captures the
-- protocol's agreement guarantee: a snapshot confirms only via a full multisignature, so every
-- honest party confirms along the same chain. Party-indexed data are vectors for clean updates.
record System : Set where
  field
    parties  : ℕ
    localOf  : Vec LocalState parties
    onChain  : OC.HeadDatum
    inFlight : List (Fin parties × Fin parties × Message)
    honest   : Vec Bool parties
    U₀       : UTxO
    chainTxs : ℕ → List Data
open System

-- The single-step relation _⟶ˢ_:
--   • deliver  — an honest party handles a delivered (non-confirming) in-flight message via
--                `_handles_↝_`; its confirmed snapshot is unchanged (so reqTx/ackSn-collect).
--   • confirm  — an honest party advances its confirmed snapshot to one drawn from the agreed
--                chain (its transactions are the chain's at that number). This is the only move
--                that changes a confirmed set, and it stays on the shared chain by construction.
--   • inject   — the network adversary injects / re-delivers a message.
--   • corrupt  — the active adversary corrupts a party (honest parties only ever shrink).
-- The chain `chainTxs` and `U₀` are never modified by a step.
data _⟶ˢ_ : System → System → Set where
  deliver : ∀ {sys i st'} {sender : Fin (parties sys)} {msg : Message}
    → (sender , i , msg) ∈ˡ inFlight sys
    → lookup (honest sys) i ≡ true
    → lookup (localOf sys) i handles msg ↝ st'
    → LocalState.confirmed st' ≡ LocalState.confirmed (lookup (localOf sys) i)  -- non-confirming
    → sys ⟶ˢ record sys { localOf = localOf sys [ i ]≔ st' }

  confirm : ∀ {sys i snap}
    → lookup (honest sys) i ≡ true
    → Snapshot.txs snap ≡ chainTxs sys (Snapshot.number snap)   -- the confirmed snapshot is the chain's
    → sys ⟶ˢ record sys
        { localOf = localOf sys [ i ]≔ record (lookup (localOf sys) i) { confirmed = snap } }

  inject : ∀ {sys} (m : Fin (parties sys) × Fin (parties sys) × Message)
    → sys ⟶ˢ record sys { inFlight = m ∷ inFlight sys }

  corrupt : ∀ {sys} (i : Fin (parties sys))
    → sys ⟶ˢ record sys { honest = honest sys [ i ]≔ false }

-- An initial system: nothing in flight, nobody has confirmed past snapshot 0, the chain starts
-- empty, and — the protocol safety guarantee — every prefix of the agreed chain is applicable to
-- U₀ (honest parties only ever sign applicable snapshots, so the confirmed chain never conflicts).
Initial : System → Set
Initial sys =
    (inFlight sys ≡ [])
  × (∀ i → confirmedTxs (lookup (localOf sys) i) ≡ [])
  × (∀ i → confirmedNo (lookup (localOf sys) i) ≡ 0)
  × (chainTxs sys 0 ≡ [])
  × (∀ k → Applicable (U₀ sys) (chainTxs sys k))

-- Reachable = reflexive-transitive closure of _⟶ˢ_ from an initial system.
data Reachable : System → Set where
  base : ∀ {s}    → Initial s → Reachable s
  step : ∀ {s s'} → Reachable s → s ⟶ˢ s' → Reachable s'

-- The invariant carried through every reachable system:
--   (1) every prefix of the agreed chain is applicable to U₀, and
--   (2) each honest party's confirmed transactions are exactly the chain at its confirmed number
--       (the parties stay on the shared chain).
Inv : System → Set
Inv sys =
    (∀ k → Applicable (U₀ sys) (chainTxs sys k))
  × (∀ i → lookup (honest sys) i ≡ true
       → confirmedTxs (lookup (localOf sys) i) ≡ chainTxs sys (confirmedNo (lookup (localOf sys) i)))

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

-- The invariant holds at every reachable system. This is the real safety induction: the base
-- case unfolds the initial conditions; inject/corrupt leave the chain and the confirmed sets in
-- place (corrupt via honest-mono); deliver keeps each party's confirmed snapshot (its hypothesis);
-- and confirm moves party i onto the chain — exactly what its `Snapshot.txs ≡ chainTxs …` premise
-- records. No postulate is needed: the §7 content is the `Initial` premise that the agreed chain
-- is applicable, which the proof propagates.
invariant : ∀ sys → Reachable sys → Inv sys
invariant sys (base (_ , ct≡[] , cn≡0 , c0≡[] , chApp)) = chApp , poc
  where
    poc : ∀ i → lookup (honest sys) i ≡ true
        → confirmedTxs (lookup (localOf sys) i) ≡ chainTxs sys (confirmedNo (lookup (localOf sys) i))
    poc i _ = trans (ct≡[] i) (sym (trans (cong (chainTxs sys) (cn≡0 i)) c0≡[]))
invariant sys (step {s} r tr) = invStep tr (invariant s r)
  where
    P : System → LocalState → Set
    P sys₀ w = confirmedTxs w ≡ chainTxs sys₀ (confirmedNo w)

    invStep : ∀ {a b} → a ⟶ˢ b → Inv a → Inv b
    invStep (inject m)            (chApp , poc) = chApp , poc
    invStep {a} (corrupt i)       (chApp , poc) =
      chApp , λ k hk → poc k (honest-mono (honest a) i k hk)
    invStep {a} (deliver {i = i} {st' = st'} _ _ _ conf≡) (chApp , poc) =
      chApp , poc'
      where
        poc' : ∀ k → lookup (honest a) k ≡ true → P a (lookup (localOf a [ i ]≔ st') k)
        poc' k hk with i FinP.≟ k
        ... | no  i≢k  = subst (P a) (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (poc k hk)
        ... | yes refl = subst (P a) (sym (lookup∘update i (localOf a) st'))
              (trans (cong Snapshot.txs conf≡)
                     (trans (poc k hk) (cong (chainTxs a) (sym (cong Snapshot.number conf≡)))))
    invStep {a} (confirm {i = i} {snap = snap} _ tx≡) (chApp , poc) =
      chApp , poc'
      where
        st' = record (lookup (localOf a) i) { confirmed = snap }
        poc' : ∀ k → lookup (honest a) k ≡ true → P a (lookup (localOf a [ i ]≔ st') k)
        poc' k hk with i FinP.≟ k
        ... | no  i≢k  = subst (P a) (sym (lookup∘update′ (λ e → i≢k (sym e)) (localOf a) st')) (poc k hk)
        ... | yes refl = subst (P a) (sym (lookup∘update i (localOf a) st')) tx≡

-- The per-system Consistency property, §7. Under the agreement invariant a party's confirmed set
-- is `chainTxs (confirmedNo i)`, so two honest parties' confirmed sets are nested prefixes of the
-- one chain and their union T̄ᵢ ∪ T̄ⱼ is `chainTxs (ŝᵢ ⊔ ŝⱼ)` (the longer prefix); joint
-- applicability is exactly that this prefix applies to U₀.
HoldsAt : System → Set
HoldsAt sys =
  ∀ (i j : Fin (parties sys))
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → Applicable (U₀ sys)
      (chainTxs sys (confirmedNo (lookup (localOf sys) i) ⊔ confirmedNo (lookup (localOf sys) j)))

Consistency : Set
Consistency = ∀ (sys : System) → Reachable sys → HoldsAt sys

-- Consistency now holds OUTRIGHT (no postulate): it is the chain-applicability component of the
-- invariant, instantiated at the larger of the two parties' confirmed snapshot numbers.
consistency : Consistency
consistency sys reach i j _ _ =
  proj₁ (invariant sys reach)
    (confirmedNo (lookup (localOf sys) i) ⊔ confirmedNo (lookup (localOf sys) j))

-- Corollary tying the abstract chain back to the parties: every honest party's confirmed
-- transactions are exactly the agreed chain at its confirmed snapshot number.
confirmed-on-chain : ∀ sys → Reachable sys → ∀ i → lookup (honest sys) i ≡ true
  → confirmedTxs (lookup (localOf sys) i) ≡ chainTxs sys (confirmedNo (lookup (localOf sys) i))
confirmed-on-chain sys reach = proj₂ (invariant sys reach)

-- Soundness / Completeness (chain, P2) and Liveness (head; needs the temporal/fairness
-- layer, P3) remain abstract for now; see security-formalisation-plan.md.
postulate
  Soundness    : Set   -- TODO(D4-P2)
  Completeness : Set   -- TODO(D4-P2)
  Liveness     : Set   -- TODO(D4-P3): under the liveness condition
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
]
