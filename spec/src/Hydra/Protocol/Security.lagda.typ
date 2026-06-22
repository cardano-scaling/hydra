```
module Hydra.Protocol.Security where

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
```

#import "/template.typ": *
#import "/macros.typ": *

= Security (WIP - Iteration 1) <sec:security>
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
*machine-checked* in Agda - #propName[Consistency] (`consistency`), #propName[Soundness]
(`soundness`) and #propName[Completeness] (`completeness`) - with the safety content *derived*
from a signature model (below): individual party signatures, a snapshot _confirmable_ only once
*every* party signed it (the coordinated head's full multisignature), and honest parties signing
only _applicable_ snapshots, at most one per number, each extending the signer's own confirmed
snapshot. From these the Agda machine-checks that every honest party's confirmed snapshot is
applicable to $Uinit$ (so confirmed sets never conflict), that two confirmations of the same snapshot
number coincide, and that confirmed snapshots nest by number (`confirmed-nest`).
`confirm` checks the §3.2 aggregate multisignature (`msVfy`); `msgOf` is the snapshot's
own serialised content (`snapMsg` of its version, number and η-hash), so the verified message depends
only on the snapshot's identifying fields rather than being a free token. The binding of a verifying
signature to a snapshot is formally carried by `ms-unforgeable`. These are theorems about every
#emph[currently]-honest party's confirmed snapshot (the random variables $That_i$/$Tbar_i$ are scoped
to a party _while uncorrupted_; corruption only shrinks the honest set, and the theorems do not
constrain a once-honest-now-corrupt party's confirmed set). The safety perimeter - the assumptions
the proofs rest on - is: (a) the ledger semantics (`applyTxs`); (b) per-signature _unforgeability_
(`sigUnforge`, EUF-CMA) plus the aggregation scheme's n-of-n decomposition (`aggSound`), from which
the aggregate-level `ms-unforgeable` is *derived*; (c) the honest-signing
discipline baked into `signHonest` (sign only applicable,
≤1 per number, extend own confirmed) - an explicit honest-_behaviour_ assumption from §6.2,
not derived from the off-chain handlers; and (d) for the on-chain bridge only, that the finalized
datum's stored accumulator commits to the off-chain final UTxO (the `ηEq` hypothesis of `reflects`,
supplied per finalization), irreducible because νHead authenticates η via the multisignature, not by
recomputing it. The verified aggregate is SYSTEM-RELATIVE (`AggVerified sys snap` checks the
aggregate `aggSigOf sys snap` built from `sigs sys`), which keeps the confirmation layer
non-vacuous: tying it to the signing system's recorded signatures makes `AggVerified sys snap`
correctly false where the signatures are absent yet satisfiable where every party signed, so a model
with genuine confirmations exists. #propName[Liveness] remains a postulate pending the
temporal/fairness layer (P3). The prose lemmas
further below give the informal arguments these proofs mirror.

#dparagraph[Scope (what these proofs do and do not cover).] To avoid over-reading the word "unified":
these §7 proofs and the on-chain validity bundles of @sec:on-chain (`closeValid`, `incrementValid`, …)
are *two distinct formalizations that meet only at datum-field accessors*. The security model reads the
on-chain datum only through `OC.snapNum`/`OC.ηOf`/`OC.accUTxO`; the `finalize` step admits *any*
datum with a matching snapshot number, so no safety theorem ever consumes a bundle's value-conservation,
deadline, signature or contester checks. Those bundles are instead cross-checked against the real Plutus
validator by the extracted differential oracle (the `Reference`/`ReferenceBridge` modules),
not by these theorems. Two further honesty notes: (i) *non-vacuity* (that some confirmation is reachable) is a
meta-level model-existence argument, not machine-checked, because `msVfy` is an abstract postulate so no
closed term proves `AggVerified`; (ii) the `ηEq` accumulator-commitment is supplied by the finalizer, not
enforced by the model, so `Reflects` is conditional on the finalizer having posted the η it signed. The
νDeposit validator (`deposit.ak`) and the off-chain handlers are likewise hand-reviewed coverage
boundaries (see `claimTxValid`), not part of any machine-checked theorem here.

As an aside (not used in the proofs below), a monotonicity proposition on a
party's confirmed snapshot number is stated in @agda-appendix.

```agda
-- An illustrative standalone proposition (not part of the proved properties below): a
-- confirmed snapshot's number does not decrease from one local state to another.
SnapshotMonotone : LocalState → LocalState → Set
SnapshotMonotone st st' =
  Snapshot.number (LocalState.confirmed st) ≤ Snapshot.number (LocalState.confirmed st')
```

The §7 properties quantify over whole multi-party executions in the presence of an
adversary, so they are stated over an explicit execution model: a ledger-application operation `applyTxs` (@agda-appendix), a global
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
gap induction using (ii). `confirm` checks the §3.2 aggregate multisignature (`AggVerified`/`msVfy`).
Beyond the ledger `applyTxs` and the scheme's unforgeability (per-signature `sigUnforge` + the
`aggSound` decomposition, from which `ms-unforgeable` is derived), the safety argument
relies on the honest-_behaviour_ assumption encoded in `signHonest` (see the modelling note below;
it is what makes the confirmed chain linear and monotone, and is not derived from the off-chain
handlers) and, for the on-chain side, on the finalization bridge's accumulator-commitment hypothesis.
The off-chain⇒on-chain link is CONSTRUCTED (`reflects`, from a `finalize` step): the
conflict-freedom and snapshot-number conjuncts of `Reflects` are derived, leaving the stored
accumulator's commitment to the off-chain UTxO as the single assumed conjunct, supplied per
finalization as the explicit hypothesis `ηEq` (a hypothesis, not a global axiom, since `finalize`
admits any matching-number datum). *Liveness* additionally needs a
temporal/fairness layer (P3).

This section states the model and the property statements; the machine-checked *proof terms* (the
`invariant` and its L1/L2/L3 corollaries, the `consistency`/`soundness`/`completeness` derivations, the
once-honest-then-corrupt extension and the `reflects` bridge) live in the companion module
#raw("Hydra.Protocol.SecurityProofs") so the prose stays focused on the statements. They are still
checked by `nix build` (imported by `Main`), so the properties remain machine-verified.

#dparagraph[Modelling note (honest signing discipline, derived).]
The `signHonest` move is DRIVEN by the off-chain handler model: an honest party signs by FIRING the
`reqSn-sign` handler (OffChain `_handles_↝_`) with no snapshot in flight ($hats = bars$). From these
operational inputs the four honest-signing guards are PROVED, not assumed: it signs the snapshot
numbered exactly one above its _own_ confirmed snapshot (the handler's $s = bars + 1$, with $hats = bars$),
whose transactions extend that snapshot's by an applicable observed delta, and signs at most one snapshot
per number (because signing advances $hats$, so the invariant `signNumBound` bounds every prior signature
strictly below the new number). Applicability to $Uinit$ follows by ledger compositionality
(`applyTxs-compose`) from the party's confirmed-applicability invariant; only-seen follows from the
delta-observed input and the `sigSeen` invariant. The §6 prose specifies this regime operationally
(round-robin snapshot leader, $s = hats + 1$, the $hpRS$ 'wait' guards); the security model derives
its safety consequences from a faithful transcription of that handler. It is
what makes the confirmed chain provably linear (`agree`) and monotone (`confirmed-nest`).

```agda
-- Ledger application: apply a transaction list to a UTxO set; `nothing` = ⊥ (conflict).
-- `applyTxs-nil` is the (trivial) ledger law that applying no transactions never conflicts.
-- `applyTxs-compose` is ledger COMPOSITIONALITY: applying `txs₁` then `txs₂` equals applying their
-- concatenation. The one ledger law (alongside the nil law) admitted to DERIVE the honest signer's
-- applicability guard (A4/D1): an honest party checks the requested txs apply on top of its confirmed
-- ledger, and compositionality lifts that to applicability against the initial U₀.
postulate
  applyTxs     : UTxO → List Data → Maybe UTxO
  applyTxs-nil : ∀ U → applyTxs U [] ≡ just U
  applyTxs-compose : ∀ {U U′} txs₁ txs₂ → applyTxs U txs₁ ≡ just U′
                   → applyTxs U (txs₁ ++ txs₂) ≡ applyTxs U′ txs₂

-- A transaction list is jointly applicable to U when applying it does not conflict (≠ ⊥).
Applicable : UTxO → List Data → Set
Applicable U txs = ¬ (applyTxs U txs ≡ nothing)


-- T̄ᵢ / ŝᵢ: a party's confirmed transactions and confirmed snapshot number.
confirmedTxs : LocalState → List Data
confirmedTxs st = Snapshot.txs (LocalState.confirmed st)

confirmedNo : LocalState → ℕ
confirmedNo st = Snapshot.number (LocalState.confirmed st)

-- ════════════════════════════════════════════════════════════════════════════════════════════
-- P1-real: DERIVING the agreement/applicability of confirmed snapshots from a signature model.
-- We record individual party
-- signatures, declare a snapshot CONFIRMABLE (`Certified`) only once EVERY party signed it (the
-- coordinated head's full multisignature), and constrain HONEST signing to applicable snapshots,
-- at most one per number, each extending its own confirmed snapshot. From these we DERIVE below:
-- every honest party's confirmed snapshot is applicable to U₀ (L3); two certified snapshots of the
-- same number are equal (L1); and confirmed snapshots NEST by number (L2, `confirmed-nest`).
-- `confirm` checks the §3.2 aggregate multisignature (`msVfy`); the only irreducible
-- assumptions are the ledger `applyTxs` / nil law and the scheme's unforgeability (per-signature
-- `sigUnforge` + `aggSound`, from which the aggregate `ms-unforgeable` is derived).
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
    seen     : Vec (List Data) parties   -- T̂ᵢ: the txs each party has observed (hpSeen), monotone
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
-- (§4) over the snapshot's message cid‖v‖s‖η# (§6). `aggKey` is that aggregate key; `snapMsg` the §6
-- message SERIALISATION, a function of the snapshot's OWN identifying fields (version, number, η#) --
-- so the verified message `msgOf snap` manifestly depends only on those fields, not on a free
-- content-independent token (two snapshots agreeing on them have the same message, by definition).
--
-- `aggSigOf sys snap` is the AGGREGATE signature verified for `snap` -- the combination of the
-- individual signatures the SYSTEM has recorded on `snap` (in `sigs sys`). It is therefore a function
-- of BOTH the system and the snapshot, NOT of the snapshot alone, which keeps the model non-vacuous.
-- Tying the verified
-- aggregate to `sigs sys` makes `AggVerified sys snap` correctly FALSE for a system missing signatures
-- yet SATISFIABLE for one where every party signed -- so an execution can genuinely confirm.
postulate
  aggKey      : VKey
  snapMsg     : ℕ → ℕ → Maybe ℍ → ℍ
  aggSigOf    : System → Snapshot → AggSig

-- The message a snapshot's aggregate signature is verified against: its own (version, number, η#).
msgOf : Snapshot → ℍ
msgOf snap = snapMsg (Snapshot.version snap) (Snapshot.number snap) (Snapshot.etaHash snap)

-- The operational check `confirm` performs: the aggregate built from THIS system's recorded signatures
-- on `snap` verifies under the head key over `snap`'s message. System-relative (see above).
AggVerified : System → Snapshot → Set
AggVerified sys snap = msVfy aggKey (msgOf snap) (aggSigOf sys snap) ≡ true

-- Aggregate unforgeability is FACTORED through the per-signature level (A2): rather than postulate
-- "verifying aggregate ⇒ every party signed" monolithically, we postulate the two more-elementary facts
-- it rests on and DERIVE it. `PartyVerified sys i snap` is party i's individual component of the
-- aggregate verifying under i's own key (the σⱼ the system recorded on snap).
postulate
  PartyVerified : (sys : System) → Fin (parties sys) → Snapshot → Set
  -- §3.2 scheme STRUCTURE: a verifying n-of-n aggregate decomposes -- if `msVfy` accepts the aggregate
  -- (`AggVerified`), then every party's individual component verifies. (A property of the aggregation
  -- scheme, e.g. BLS, where the aggregate verifies iff each constituent does.)
  aggSound  : ∀ sys snap → AggVerified sys snap → (i : Fin (parties sys)) → PartyVerified sys i snap
  -- per-signature UNFORGEABILITY (EUF-CMA, the irreducible cryptographic hardness assumption): a
  -- verifying individual signature on `snap` means that party actually signed it (recorded in `sigs`).
  sigUnforge : ∀ sys snap (i : Fin (parties sys)) → PartyVerified sys i snap → Signed sys i snap

-- MS-scheme unforgeability is a DERIVED THEOREM: a verifying aggregate ⇒
-- every party signed. It FACTORS through the scheme's decomposition (`aggSound`) and per-signature
-- unforgeability (`sigUnforge`) -- so the trusted base is the standard per-signature EUF-CMA assumption
-- plus the aggregation scheme's structure, not a monolithic aggregate-level axiom. Downstream uses
-- (`confirm`, `soundness`, `reflects`, `confCert-all`) consume it through this unchanged type.
ms-unforgeable : ∀ sys snap → AggVerified sys snap → Certified sys snap
ms-unforgeable sys snap aggOK i = sigUnforge sys snap i (aggSound sys snap aggOK i)

-- The single-step relation _⟶ˢ_:
--   signHonest  : an honest party signs a snapshot by FIRING the off-chain `reqSn-sign` handler
--                 (OffChain `_handles_↝_`): it requires no snapshot in flight (ŝ = s̄), the requested
--                 txs Δ extend its OWN confirmed snapshot and apply on top of it, and Δ is already
--                 observed. The handler advances ŝ ← s. The four honest-signing safety guards L1/L3
--                 rest on (applicability, one-per-round, chain-extension, only-seen) are DERIVED from
--                 these operational inputs + the invariant (see
--                 `invStep`'s signHonest arm and `signNumBound`/`sigSeen` in `Inv`).
--   signCorrupt : a corrupt party may sign ANY snapshot (the adversary forges nothing honest).
--   confirm     : a party adopts a snapshot whose AGGREGATE multisignature verifies (`AggVerified`,
--                 i.e. `msVfy` passes); unforgeability then makes it certified (all parties signed).
--   corrupt     : the active adversary corrupts a party (honest parties only ever shrink).
-- `sigs` only grows; `U₀` and `onChain` are never changed by a step. `signHonest` additionally bumps
-- the signer's `seenNumber` (ŝ); `confirm` updates a party's `confirmed`; `see` grows `seen`.
data _⟶ˢ_ : System → System → Set where
  signHonest : ∀ {sys i snap Δ txReq txα txω}
    → lookup (honest sys) i ≡ true                                                   -- honest signer
    → LocalState.seenNumber (lookup (localOf sys) i) ≡ confirmedNo (lookup (localOf sys) i)  -- no snapshot in flight (ŝ = s̄)
    -- FIRE the reqSn-sign handler: witnesses the §6.4 `require` guards (s = s̄+1, v = v̂) and advances
    -- ŝ ← s. This is the "every step ≈ a handler execution" link. (The requested txs the snapshot
    -- includes are the list Δ below; the message's `txReq` payload is the abstract §6 encoding.)
    → (lookup (localOf sys) i) handles
          (reqSn (Snapshot.version snap) (Snapshot.number snap) txReq txα txω)
        ↝ record (lookup (localOf sys) i) { seenNumber = Snapshot.number snap }
    → Snapshot.txs snap ≡ confirmedTxs (lookup (localOf sys) i) ++ Δ                  -- snapshot = confirmed ++ requested
    → (∀ {U′} → applyTxs (U₀ sys) (confirmedTxs (lookup (localOf sys) i)) ≡ just U′
              → Applicable U′ Δ)                                                      -- Δ applies on top of confirmed (requireApplyTxs)
    → Δ ⊆ˡ lookup (seen sys) i                                                        -- Δ already observed (only-seen)
    → sys ⟶ˢ record sys
        { localOf = localOf sys [ i ]≔ record (lookup (localOf sys) i) { seenNumber = Snapshot.number snap }
        ; sigs    = (i , snap) ∷ sigs sys }

  signCorrupt : ∀ {sys i snap}
    → lookup (honest sys) i ≡ false
    → sys ⟶ˢ record sys { sigs = (i , snap) ∷ sigs sys }

  confirm : ∀ {sys i snap}
    → AggVerified sys snap
    → sys ⟶ˢ record sys
        { localOf = localOf sys [ i ]≔ record (lookup (localOf sys) i) { confirmed = snap } }

  corrupt : ∀ {sys} (i : Fin (parties sys))
    → sys ⟶ˢ record sys { honest = honest sys [ i ]≔ false }

  -- finalize: the head posts an on-chain datum `d'` (a close/fanout) for a snapshot whose AGGREGATE
  -- multisignature verifies, carrying that snapshot's number. This is what CONNECTS the otherwise
  -- frozen `onChain` field to the dynamics, so `Reflects` (below) is CONSTRUCTED from a step.
  -- It changes only `onChain`; `U₀`/`sigs`/`localOf`/`honest` are untouched, so it preserves
  -- every `Inv` component (none of which mentions `onChain`).
  finalize : ∀ {sys snap d'}
    → AggVerified sys snap
    → OC.snapNum d' ≡ Snapshot.number snap
    → sys ⟶ˢ record sys { onChain = d' }

  -- see: an honest (or any) party OBSERVES some transactions, growing its seen set `T̂` (models the
  -- §6.4 hpSeen output / processing a reqTx). `seen` only grows; everything else is untouched, so it
  -- preserves every `Inv` component (none of which mentions `seen`).
  see : ∀ {sys i txs}
    → sys ⟶ˢ record sys { seen = seen sys [ i ]≔ (txs ++ lookup (seen sys) i) }

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

-- Every honest signature on `snap` carries a predecessor snapshot `pre` it extends: `snap` is one
-- number higher, contains `pre`'s txs, and `pre` is the genesis or is itself certified. (This is the
-- §7 snapshot-extension discipline that yields L2 `confirmed-nest`.)
-- Parameterised by the `certified` predicate (`Certified sys`), NOT by `sys` itself: this keeps the
-- witness STABLE across steps that leave `sigs` (hence `Certified`) unchanged (confirm/corrupt only
-- touch localOf/honest), so it can be carried through those steps without coercion.
record PredecessorWitness (certified : Snapshot → Set) (snap : Snapshot) : Set where
  constructor mkPredecessor
  field
    pre              : Snapshot
    numberSuc        : Snapshot.number snap ≡ suc (Snapshot.number pre)
    txsExtend        : Snapshot.txs pre ⊆ˡ Snapshot.txs snap
    preGenesisOrCert : (Snapshot.number pre ≡ 0) ⊎ certified pre

-- The DERIVED invariants carried through every reachable system, one per field. Each honest-signature
-- fact is DERIVED at the `signHonest` step from the `reqSn-sign` handler + the no-in-flight
-- precondition + the invariants (`signNumBound`/`sigSeen`):
--   sigApp   : every honest signature is on a snapshot applicable to U₀ (via `applyTxs-compose`);
--   sigDedup : an honest party signs at most one snapshot per number (via `signNumBound`);
--   confApp  : every honest party's confirmed snapshot is applicable to U₀ (L3), DERIVED rather than
--              assumed for the whole chain.
--   sigPos   : an honest signature is on a snapshot of number > 0 (the handler's s = s̄+1);
--   confCert : an honest party's confirmed snapshot is the genesis or is certified;
--   sigChain : every honest signature has an extending certified-or-genesis `PredecessorWitness`.
--              The last three give L2 (`confirmed-nest`).
record Inv (sys : System) : Set where
  field
    sigApp   : ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
             → Applicable (U₀ sys) (Snapshot.txs snap)
    sigDedup : ∀ {k s1 s2} → lookup (honest sys) k ≡ true → Signed sys k s1 → Signed sys k s2
             → Snapshot.number s1 ≡ Snapshot.number s2 → s1 ≡ s2
    confApp  : ∀ {i} → lookup (honest sys) i ≡ true
             → Applicable (U₀ sys) (confirmedTxs (lookup (localOf sys) i))
    sigPos   : ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap → 0 < Snapshot.number snap
    confCert : ∀ {i} → lookup (honest sys) i ≡ true
             → (confirmedNo (lookup (localOf sys) i) ≡ 0 × confirmedTxs (lookup (localOf sys) i) ≡ [])
               ⊎ Certified sys (LocalState.confirmed (lookup (localOf sys) i))
    sigChain : ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap → PredecessorWitness (Certified sys) snap
    -- signNumBound: every honest signature's number is ≤ that party's last-signed number ŝ. With the
    --   `signHonest` no-in-flight precondition (ŝ = s̄) and the handler signing s = s̄+1 and bumping ŝ,
    --   this DERIVES the one-signature-per-round guard (`sigDedup`): a fresh sign is strictly above ŝ.
    signNumBound : ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
                 → Snapshot.number snap ≤ LocalState.seenNumber (lookup (localOf sys) k)
    -- sigSeen: every honest signature is on txs the party has SEEN. DERIVES the only-seen guard; from
    --   the handler's Δ ⊆ seen + (confirmedTxs ⊆ seen, itself from confCert+sigSeen). Feeds the
    --   second conjunct of `soundness`.
    sigSeen      : ∀ {k snap} → lookup (honest sys) k ≡ true → Signed sys k snap
                 → Snapshot.txs snap ⊆ˡ lookup (seen sys) k


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


-- ── P2: Soundness and Completeness (Chain) ─────────────────────────────────────────────────
-- The finalized on-chain UTxO is the closed/fanned-out snapshot applied to U₀. That snapshot is
-- certified (the head closes only against a fully-signed snapshot), so by `cert-applicable` it is
-- conflict-free.
Ufinal : System → Snapshot → Maybe UTxO
Ufinal sys snap = applyTxs (U₀ sys) (Snapshot.txs snap)


-- Soundness (Chain), §7: the final UTxO U₀ ∘ T̃ for a finalized snapshot T̃ whose aggregate
-- multisignature verifies (`AggVerified`) is conflict-free AND its transactions were seen by EVERY
-- honest party (`T̃ ⊆ ⋂_{j∈H} seen_j`). DERIVED: `ms-unforgeable` makes the verified snapshot certified
-- (every party signed it); each honest signer signed only applicable txs (`cert-applicable`, giving
-- conflict-freedom) it had seen (`sigSeen-inv`, giving the ⋂-seen subset).
Soundness : Set
Soundness = ∀ sys → Reachable sys → ∀ {h snap} → lookup (honest sys) h ≡ true → AggVerified sys snap
          → Σ[ U ∈ UTxO ] (Ufinal sys snap ≡ just U)
                        × (∀ {j} → lookup (honest sys) j ≡ true → Snapshot.txs snap ⊆ˡ lookup (seen sys) j)


-- Completeness (Chain), §7: every transaction an honest party confirmed (T̄ᵢ) is included in a more
-- advanced honest party's confirmed set (in particular the honest closer's, which becomes the
-- finalized snapshot) whenever ŝᵢ ≤ ŝⱼ. This is exactly the nesting obligation `confirmed-nest` (L2).
Completeness : Set
Completeness = ∀ sys → Reachable sys → ∀ i j
  → lookup (honest sys) i ≡ true → lookup (honest sys) j ≡ true
  → confirmedNo (lookup (localOf sys) i) ≤ confirmedNo (lookup (localOf sys) j)
  → confirmedTxs (lookup (localOf sys) i) ⊆ˡ confirmedTxs (lookup (localOf sys) j)


-- ── Linking the two Agda halves: off-chain confirmed snapshot ↔ on-chain close/fanout ──────────
-- They meet at finalization: when the head closes/fans out, the on-chain Closed datum's accumulator
-- commits to exactly the off-chain final UTxO U₀ ∘ (txs of the certified finalized snapshot).

-- Glue: the set of outputs held in a UTxO map (its range). Basic, assumed (not modelled in detail).
postulate
  outsOf : UTxO → ℙ Output

-- Bridge predicate: the on-chain head datum REFLECTS a finalized snapshot `snap` -- its snapshot
-- number matches and its stored accumulator commits (`OC.accUTxO`) to U₀ ∘ (txs snap).
record Reflects (sys : System) (snap : Snapshot) : Set where
  constructor mkReflects
  field
    finalUtxo     : UTxO
    conflictFree  : Ufinal sys snap ≡ just finalUtxo                  -- the final UTxO is conflict-free
    numberMatches : OC.snapNum (onChain sys) ≡ Snapshot.number snap   -- on-chain snapshot number matches
    accCommits    : OC.ηOf (onChain sys) ≡ OC.accUTxO (outsOf finalUtxo)  -- on-chain accumulator commits to it


-- Liveness (head; needs the temporal/fairness layer, P3) remains abstract.
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

  _Machine-checked as `consistency` (above), *derived* from the signature model: each honest
  party's confirmed set is applicable (`conf-applicable`: a confirmed snapshot is certified, so it
  carries that party's own signature, and honest parties sign only applicable snapshots), and the two
  confirmed sets nest (`confirmed-nest`, derived via `cert-nest` from the honest extend-your-own-confirmed
  guard + agreement). The only safety assumptions are the ledger and the §3.2 multisignature's
  unforgeability (`ms-unforgeable`). The statement also covers parties corrupted AFTER
  confirming (`consistency-uncorrupted`): since `confirm` requires a multisignature regardless of the
  confirmer's honesty, EVERY confirmed snapshot is certified (`confCert-all`), so any party's confirmed
  set -- including a once-honest party's, the one an on-chain close could be built against -- stays
  consistent with every other's, given at least one honest party._
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
  or a snapshot that is one step ahead - implying that everybody will hold a valid multisignature
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
  $tilde(T)$ the certified finalized snapshot). The $!= bot$ is *derived*: a
  certified snapshot carries an honest party's signature, and honest parties sign only applicable
  snapshots (`cert-applicable`). The $tilde(T) subset.eq inter.big_(i in honest) That_i$ conjunct
  is machine-checked too: an honest party signs only transactions it has observed (the `signHonest`
  seen guard), so a certified snapshot's transactions lie in every honest party's seen set
  (`sigSeen-inv`), proved as the second conjunct of #raw("soundness")._
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
  whenever $bars_i <= s_f$). This is the snapshot-nesting property `confirmed-nest` (L2), *derived*
  (`cert-nest`)._
]
