```
-- Solvency: the head output's L1 value covers exactly the L2 UTxO set the accumulator
-- commits to. This is the global invariant whose absence let the deposit-binding bug
-- pass every per-transition check (see the changelog entry for "Bind deposits to the
-- snapshots that authorize them"): a look-alike deposit satisfied signature, value
-- conservation and spentness locally, while the accumulator credited more value than
-- the head absorbed. The induction below cannot close its increment case without the
-- transaction-id binding in `depositCommitsHashOf`, the first-output rule, and the
-- single-claimed-deposit discipline; its decrement case cannot close without the
-- materialized-outputs rule and the no-both-in-flight snapshot shape. Each of those
-- is a component of the fix - reverting any one reopens a hole here, at typecheck
-- time, which is the point of this module.
module Hydra.Protocol.Solvency where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.OffChain
open import Hydra.Protocol.Preliminaries using (Output; OutputRef; Context; _‖_)
open import Hydra.Protocol.Security
import Hydra.Protocol.OnChain as OC
open import Data.Empty using (⊥-elim)
open import Data.Unit using (⊤; tt)
open import Data.Product using (_×_; _,_; proj₁; proj₂)
open import Data.List using (take; drop)
open import Relation.Binary.PropositionalEquality using (trans; sym; cong; cong₂; subst; _≢_)
```

#import "/template.typ": *
#import "/macros.typ": *

== Solvency <sec:solvency>

The per-transaction validity bundles of @sec:on-chain each conserve value
locally, and the security results of @sec:security-theorems establish that
every settled snapshot is unanimously certified. Neither layer states the
property that actually protects funds: that the head output's value _covers_
the UTxO set the accumulator commits to, so that fanout can distribute what L2
accounting says participants own. The deposit-binding vulnerability violated
exactly this while satisfying every local check. This section states the
invariant, proves it inductively over the head's on-chain transitions, and
derives the payoff at fanout: every distributed output is one the committed
L2 set actually contains, funded by the head value the invariant accounts
for.

The L2 UTxO set is not recoverable from the on-chain state (the datum carries
only the commitment $eta$), so the induction carries it as _ghost state_: a
set $U$ alongside the datum, with the two-part invariant

$ eta = accUTxO(U) quad "and" quad valHead = r_0 plus.o Sigma_(o in U) val(o), $

where $r_0$ is the value the head output carried at init (the $adaO$ overhead
plus the state and participation tokens, preserved by every transition) and
$Sigma$ is the value of a UTxO set. The value function and its empty-set law
are the only new postulates besides two digest constants; every other
ingredient is a named hypothesis with a clear owner, gathered in the
`Assumptions` record or supplied per step in the reachability relation below.

```
-- The value of a UTxO set in L1-originating assets - the fanoutable component.
-- Abstract, in the same trust family as `setSize`: the set model is opaque, so the
-- fold is postulated with its empty-set law rather than derived. The L1 restriction
-- is the intended reading, not a checked side condition: L2 transactions may mint,
-- but a minted token can never cross the L1 boundary (the increment, decrement and
-- fanout value equations compare against a head output that never absorbed it), so
-- the solvency accounting is over L1-originating value throughout.
postulate
  sumValue   : ℙ Output → Value
  sumValue-∅ : sumValue ∅ˢ ≡ εᵛ

-- The digest components an honest node uses when no deposit is pending: the node's
-- `commitOutputsHash` over an empty commit set and an absent deposit id. Opaque
-- constants; what matters is that a real spent out-ref's transaction id differs from
-- `noTxId` (a per-step hypothesis, owner: the ledger - L1 transaction ids are hashes,
-- never the absent-marker). Encoding fidelity note: in the implementation the
-- no-deposit digest hashes a shorter preimage (no tx-id bytes appended) rather than a
-- pair with a marker; the pair encoding models the same distinguishability and its
-- faithfulness is an encoding assumption of the same family as the bridge's
-- `refCodeOf`/`cidToNat`.
postulate
  noCommitHash : ℍ
  noTxId       : ℍ

-- The value of a list of outputs (the decrement side works over the positional
-- decommit-output list). Definitional, no postulate.
listValue : List Output → Value
listValue []       = εᵛ
listValue (o ∷ os) = Output.value o +ᵛ listValue os
```

What honest signing guarantees about a certified snapshot, in the on-chain
vocabulary the induction consumes. A certificate is n-of-n (@thm:unanimity),
so at least one honest party enforced the §6 handler `require`s before
signing; these fields are that party's obligations. The pending shape is a
_sum_: a snapshot carries a pending commit, a pending decommit, or neither -
never both. That is the machine-checked `NoBothInFlight` state invariant of
@sec:offchain-theorems lifted to the snapshot level, and it is load-bearing
below: without it a decrement could settle an increment-shaped snapshot,
adopting an accumulator that counts deposited UTxOs no transaction absorbed.

```agda
data PendingShape (snap : Snapshot) : Set where
  -- a commit is pending: κ# binds the recorded commit set's hash and the deposit's
  -- transaction id (node `commitOutputsHash`); `incVal` is the recorded set's value
  -- (`observeDepositTx` refuses deposits whose value differs); no decommit digest.
  pendingCommit :
    (cHash depId : ℍ) (incVal : Value)
    → Snapshot.comHash snap ≡ hash (cHash ‖ depId)
    → depId ≢ noTxId
    → Snapshot.decHash snap ≡ hash {A = List Output} []
    → PendingShape snap
  -- a decommit is pending: δ# binds the decommitted output list; no commit digest.
  pendingDecommit :
    (decOuts : List Output)
    → Snapshot.decHash snap ≡ hash decOuts
    → Snapshot.comHash snap ≡ hash (noCommitHash ‖ noTxId)
    → PendingShape snap
  -- neither is pending.
  pendingNone :
    Snapshot.comHash snap ≡ hash (noCommitHash ‖ noTxId)
    → Snapshot.decHash snap ≡ hash {A = List Output} []
    → PendingShape snap

record HonestFacts (snap : Snapshot) : Set where
  field
    -- the L2 UTxO set the snapshot's accumulator commits to (the honest node computes
    -- η from the snapshot UTxO it signs).
    committed : ℙ Output
    ηCoheres  : Snapshot.etaHash snap ≡ hash (OC.accUTxO committed)
    shape     : PendingShape snap
```

The assumption bundle, relative to a system of parties. The three digest
hypotheses are information-theoretic _idealizations_ of second-preimage
resistance on the digest shapes the induction inverts - stated as injectivity
on those shapes, which is stronger than the computational property, exactly as
the model's multisignature unforgeability postulates are. The computational
reading is the standard reduction: a violation of solvency under the other
hypotheses yields a concrete digest collision. They are not global injectivity
of `hash` (which would be inconsistent with compression); they apply only at
the pair, commitment and output-list shapes actually signed. Owner: crypto.
`honest-certified` is the ≥1-honest-signer argument as a _named, consumed_
assumption rather than prose: an n-of-n certificate (@thm:unanimity) contains
at least one honest party, whose §6 handler `require`s enforced exactly the
`HonestFacts` fields before signing. Owner: the honest-majority premise of
@sec:security.

```agda
record Assumptions (sys : System) : Set₁ where
  field
    -- the commit digest κ# = hash(commit-list hash ‖ deposit tx id) determines the
    -- deposit's transaction id. This hypothesis is only applicable because
    -- `depositCommitsHashOf` binds the tx id by definition: under the pre-fix
    -- digest hash(C) it says nothing, and the increment case below cannot close.
    κ#-pair-inj : ∀ {x y r s : ℍ} → hash (x ‖ r) ≡ hash (y ‖ s) → r ≡ s
    -- the accumulator hash η# determines the commitment.
    η#-inj      : ∀ {a b : AccCommitment} → hash a ≡ hash b → a ≡ b
    -- the decommit digest δ# determines the output list.
    outs#-inj   : ∀ {xs ys : List Output} → hash xs ≡ hash ys → xs ≡ ys
    -- a certificate implies the honest-signing facts above.
    honest-certified : ∀ {snap : Snapshot} → Certified sys snap → HonestFacts snap
```

Per-step hypotheses, dispatched on the pending shape (trivial for the shapes a
step contradicts). `ObservedDeposit` is ledger resolution faithfulness at the
observed deposit: any input of _this_ transaction whose out-ref names output 0
of the deposit transaction the parties observed resolves to that deposit
output, whose value is the recorded `incVal` (owners: the ledger for
resolution, the honest `observeDepositTx` for the value). `IncCoherent` /
`DecCoherent` relate consecutive committed sets by the settled delta; they
package L2 value coherence between settlements and are stated per step.
Their reading is over L1-originating value (see `sumValue`): L2-minted
assets are outside the accounting and cannot cross the L1 boundary anyway,
while an L2 _burn_ of an L1-originating asset would leave the head strictly
richer than the committed set - safe for coverage, but breaking the equality
form, so the hypotheses additionally assume the L2 traffic between
settlements burns no L1-originating value. Deriving them from the off-chain
ledger laws is future work alongside the closed-head cases.

```agda
-- Guard for reviewers: this hypothesis is stated against the _observed_ deposit id
-- (`depId`, what the parties signed for), never against the claimed ref. That
-- placement is the load-bearing modeling choice of the whole invariant - restated
-- at the claimed ref it would make solvency vacuously provable for the pre-fix
-- protocol, which the counter-model (`SolvencyCounterModel`) shows is unsound.
-- The proof must earn the crossing from claimed ref to observed deposit, and can
-- only do so through the transaction-id binding in the signed digest.
ObservedDeposit : Context → ∀ {snap} → PendingShape snap → Set
ObservedDeposit ctx (pendingCommit _ depId incVal _ _ _) =
  ∀ (r : OutputRef) → OutputRef.txId r ≡ depId → OutputRef.index r ≡ 0
    → OC.depositValueAt ctx r ≡ incVal
ObservedDeposit _ _ = ⊤

IncCoherent : ℙ Output → ℙ Output → ∀ {snap} → PendingShape snap → Set
IncCoherent U U' (pendingCommit _ _ incVal _ _ _) = sumValue U' ≡ sumValue U +ᵛ incVal
IncCoherent _ _ _ = ⊤

DecCoherent : ℙ Output → ℙ Output → ∀ {snap} → PendingShape snap → Set
DecCoherent U U' (pendingDecommit decOuts _ _) = sumValue U ≡ sumValue U' +ᵛ listValue decOuts
DecCoherent _ _ _ = ⊤
```

The ghost-state reachability relation. Each step takes the on-chain validity
bundle, the snapshot's honest facts, the unforgeability-derived digest
equalities (the per-instance idiom of the `*-certified` corollaries of
@sec:security-theorems), and the per-step ledger hypotheses; the
`headValueIn ctx ≡ w` premise is L1 UTxO continuity - the head output this
step spends is the one the previous step produced (owner: the ledger).
Certification enters through the derived `*-certified` step forms inside
`Invariant` below, which consume a `Certified` certificate through the named
`honest-certified` assumption instead of taking the honest facts on faith.

Which transitions the relation reaches is a real limit on what the theorem
says, and nothing about adding a transition to @sec:on-chain forces a step to
be added here - the theorem would simply say less. So the correspondence is
enumerated and gated rather than left to inspection: `check-trust-ledger.sh`
fails unless every `*Valid` bundle declared in @sec:on-chain is either
consumed by a step below or listed there with the reason it is out of reach.
Six of the twelve are consumed; the batched fan-out arms are out because value
leaves the head across several transactions, which the single-step shape does
not express, and the νDeposit arms because they govern the deposit UTxO rather
than the head output whose value this invariant tracks.

```agda
data SolventReach (r₀ : Value) : OC.HeadDatum → ℙ Output → Value → Set where
  s-init : ∀ {ctx seed cid n cp v η ada}
    → OC.InitValid ctx seed cid n v η
    → OC.headValue ctx ≡ r₀
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) ∅ˢ r₀

  s-inc : ∀ {ctx cid n cp v η η' ada ξ s ref δ# U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → (b : OC.IncrementValid ctx aggKey cid v (OC.Open cid aggKey n cp v η ada)
                             (OC.Open cid aggKey n cp (suc v) η' ada) ξ s ref δ#)
    → (hf : HonestFacts snap)
    → Snapshot.etaHash snap ≡ hash η'                          -- unforgeability: η# is the signed one
    → Snapshot.comHash snap ≡ OC.depositCommitsHashOf ctx ref  -- unforgeability: κ# is the signed one
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → OutputRef.txId ref ≢ noTxId                              -- a spent out-ref's tx id is real (ledger)
    → ObservedDeposit ctx (HonestFacts.shape hf)
    → IncCoherent U (HonestFacts.committed hf) (HonestFacts.shape hf)
    → SolventReach r₀ (OC.Open cid aggKey n cp (suc v) η' ada)
                   (HonestFacts.committed hf) (OC.headValue ctx)

  s-dec : ∀ {ctx cid n cp v η η' ada ξ s m κ# U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → (b : OC.DecrementValid ctx aggKey cid v (OC.Open cid aggKey n cp v η ada)
                             (OC.Open cid aggKey n cp (suc v) η' ada) ξ s m κ#)
    → (hf : HonestFacts snap)
    → Snapshot.etaHash snap ≡ hash η'                          -- unforgeability: η# is the signed one
    → Snapshot.decHash snap ≡ OC.decommitOutputsHashOf ctx m   -- unforgeability: δ# is the signed one
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → DecCoherent U (HonestFacts.committed hf) (HonestFacts.shape hf)
    → SolventReach r₀ (OC.Open cid aggKey n cp (suc v) η' ada)
                   (HonestFacts.committed hf) (OC.headValue ctx)

  -- closing on the initial (empty) snapshot: the stored accumulator is the empty
  -- commitment and the head value is preserved exactly.
  s-closeInitial : ∀ {ctx cid n cp v η ada s' η' C tfin U w}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → OC.CloseValid ctx aggKey cid v cp s' (OC.Open cid aggKey n cp v η ada)
                    (OC.Closed cid aggKey n cp v s' η' C tfin ada) OC.closeInitial
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s' η' C tfin ada) ∅ˢ (OC.headValue ctx)

  -- closing on a certified snapshot at the current version (the closeUnused
  -- redeemer; closeAny differs only in its snapshot-number conjunct and follows
  -- identically). The stored accumulator jumps to the closing snapshot's; the
  -- head value is preserved exactly, so the per-step hypothesis is that the
  -- closing snapshot's committed value equals the settled one - L2 transactions
  -- between settlements preserve value (owner: the L2 ledger rules).
  s-close : ∀ {ctx cid n cp v η ada ξ η# δ# κ# s' η' C tfin U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → OC.CloseValid ctx aggKey cid v cp s' (OC.Open cid aggKey n cp v η ada)
                    (OC.Closed cid aggKey n cp v s' η' C tfin ada) (OC.closeUnused ξ η# δ# κ#)
    → (hf : HonestFacts snap)
    → Snapshot.etaHash snap ≡ η#                               -- unforgeability: η# is the signed one
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → sumValue (HonestFacts.committed hf) ≡ sumValue U         -- L2 value preservation
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s' η' C tfin ada)
                   (HonestFacts.committed hf) (OC.headValue ctx)

  -- contesting with a newer certified snapshot (the contestUnused redeemer;
  -- contestUsed combines a pending delta into the stored accumulator and is
  -- future work, as is closeUsed). Same jump-and-preserve pattern as close.
  s-contest : ∀ {ctx cid n cp v s η C tfin ada ξ η# δ# κ# s' η' kh tfin' U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s η C tfin ada) U w
    → OC.ContestValid ctx aggKey cid v s tfin (OC.Closed cid aggKey n cp v s η C tfin ada)
                      (OC.Closed cid aggKey n cp v s' η' (kh ∷ C) tfin' ada)
                      (OC.contestUnused ξ η# δ# κ#) kh
    → (hf : HonestFacts snap)
    → Snapshot.etaHash snap ≡ η#                               -- unforgeability: η# is the signed one
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → sumValue (HonestFacts.committed hf) ≡ sumValue U         -- L2 value preservation
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s' η' (kh ∷ C) tfin' ada)
                   (HonestFacts.committed hf) (OC.headValue ctx)
```

```
-- Helper lemmas tying the positional decommit machinery together (definitional
-- inductions, no assumptions).
takeSumᵛ-take : ∀ m os → OC.takeSumᵛ m os ≡ listValue (take m os)
takeSumᵛ-take zero    _        = refl
takeSumᵛ-take (suc _) []       = refl
takeSumᵛ-take (suc k) (o ∷ os) = cong (Output.value o +ᵛ_) (takeSumᵛ-take k os)

decommitValue-take : ∀ ctx m
  → OC.decommitValue ctx m ≡ listValue (take m (drop 1 (Context.outputs ctx)))
decommitValue-take ctx m with Context.outputs ctx
decommitValue-take ctx zero    | []     = refl
decommitValue-take ctx (suc _) | []     = refl
decommitValue-take ctx m       | _ ∷ os = takeSumᵛ-take m os

-- A materialized-output list that hashes like the empty list contradicts the
-- decrement bundle's at-least-one rule.
0<length[]-elim : ∀ {xs : List Output} → xs ≡ [] → 0 < length xs → ⊥
0<length[]-elim refl ()
```

The invariant. The increment case is where the deposit-binding fix is
consumed: the digest equality inverts (`κ#-pair-inj`) _only because_
`depositCommitsHashOf` binds the transaction id, the first-output rule pins
the claimed out-ref to the observed deposit output, and
`IncrementValid.onlyClaimedDeposit` equates the absorbed value with that one
deposit's. Under the pre-fix digest the chain from "the signature verifies"
to "the head absorbed the recorded value" breaks at the first link.

All three of those are _fields of the validity bundle_ the step already
takes, not hypotheses this relation grants itself. That is what makes the
dependency load-bearing rather than asserted: drop the anti-siphon conjunct
from `IncrementValid`, or weaken `depositCommitsHashOf` back to hashing the
datum alone, and this proof stops typechecking. The
decrement case consumes `decommitNonEmpty` and the pending-shape sum: an
increment-shaped snapshot presented to a decrement forces the materialized
output list to hash like the empty list, contradicting the at-least-one rule.

```
-- A version-0 open head has never settled an increment or decrement (both bump the
-- version), so its ghost set is still empty. Consumed by the closeInitial case.
open-v0-empty : ∀ {r₀ cid hk n cp η ada U w}
  → SolventReach r₀ (OC.Open cid hk n cp 0 η ada) U w → U ≡ ∅ˢ
open-v0-empty (s-init _ _) = refl
```

```agda
module Invariant {sys : System} (H : Assumptions sys) where
  open Assumptions H

  solvency : ∀ {r₀ d U w}
    → SolventReach r₀ d U w
    → (OC.ηOf d ≡ OC.accUTxO U) × (w ≡ r₀ +ᵛ sumValue U)
```

```
  solvency {r₀ = r₀} (s-init iv hvEq) =
    OC.InitValid.etaEmpty iv ,
    sym (trans (cong (r₀ +ᵛ_) sumValue-∅) (+ᵛ-identityʳ r₀))

  solvency {r₀ = r₀} (s-inc {ctx = ctx} {ref = ref} {U = U} {w = w} r b hf ηEq κEq chain realId obs valCo)
    with HonestFacts.shape hf | solvency r
  ... | pendingCommit cHash depId incVal comCo _ _ | (_ , ih) =
    η#-inj (trans (sym ηEq) (HonestFacts.ηCoheres hf)) ,
    trans stepVal
      (trans (cong (_+ᵛ incVal) ih)
        (trans (+ᵛ-assoc r₀ (sumValue U) incVal)
          (cong (r₀ +ᵛ_) (sym valCo))))
   where
    refIs : OutputRef.txId ref ≡ depId
    refIs = sym (κ#-pair-inj (trans (sym comCo) κEq))
    absorbed : OC.depositsValue ctx ≡ incVal
    absorbed = trans (OC.IncrementValid.onlyClaimedDeposit b)
                     (obs ref refIs (OC.IncrementValid.depositFirstOutput b))
    stepVal : OC.headValue ctx ≡ w +ᵛ incVal
    stepVal = trans (sym (OC.IncrementValid.valueOK b)) (cong₂ _+ᵛ_ chain absorbed)
  ... | pendingDecommit _ _ comCo | _ =
    ⊥-elim (realId (κ#-pair-inj (trans (sym κEq) comCo)))
  ... | pendingNone comCo _ | _ =
    ⊥-elim (realId (κ#-pair-inj (trans (sym κEq) comCo)))

  solvency {r₀ = r₀} (s-dec {ctx = ctx} {m = m} {U = U} {w = w} r b hf ηEq δEq chain valCo)
    with HonestFacts.shape hf | solvency r
  ... | pendingDecommit decOuts decCo _ | (_ , ih) =
    η#-inj (trans (sym ηEq) (HonestFacts.ηCoheres hf)) ,
    +ᵛ-cancelʳ (listValue decOuts) (trans lhs rhs)
   where
    decVal : OC.decommitValue ctx m ≡ listValue decOuts
    decVal = trans (decommitValue-take ctx m)
                   (cong listValue (outs#-inj (trans (sym δEq) decCo)))
    -- headValue + listValue decOuts = headValueIn = w = r₀ + sumValue U
    --   = r₀ + (sumValue U' + listValue decOuts) = (r₀ + sumValue U') + listValue decOuts
    rhs : w ≡ (r₀ +ᵛ sumValue (HonestFacts.committed hf)) +ᵛ listValue decOuts
    rhs = trans ih (trans (cong (r₀ +ᵛ_) valCo)
                          (sym (+ᵛ-assoc r₀ (sumValue (HonestFacts.committed hf)) (listValue decOuts))))
    lhs : OC.headValue ctx +ᵛ listValue decOuts ≡ w
    lhs = trans (cong (OC.headValue ctx +ᵛ_) (sym decVal))
                (trans (OC.DecrementValid.valueOK b) chain)
  ... | pendingCommit _ _ _ _ _ decEmpty | _ =
    ⊥-elim (0<length[]-elim (outs#-inj (trans (sym δEq) decEmpty))
                            (OC.DecrementValid.decommitNonEmpty b))
  ... | pendingNone _ decEmpty | _ =
    ⊥-elim (0<length[]-elim (outs#-inj (trans (sym δEq) decEmpty))
                            (OC.DecrementValid.decommitNonEmpty b))

  solvency {r₀ = r₀} (s-closeInitial {ctx = ctx} {v = v} {U = U} {w = w} r b chain) =
    ηE ,
    trans (trans (sym (OC.CloseValid.valuePreserved b)) (trans chain ih))
          (cong (λ u → r₀ +ᵛ sumValue u) (open-v0-empty r0))
   where
    ini = OC.CloseValid.initialOK b
    v≡0 = proj₁ ini
    ηE  = proj₂ (proj₂ ini)
    ih  = proj₂ (solvency r)
    r0 : SolventReach r₀ (OC.Open _ aggKey _ _ 0 _ _) U w
    r0 = subst (λ vv → SolventReach r₀ (OC.Open _ aggKey _ _ vv _ _) U w) v≡0 r

  solvency {r₀ = r₀} (s-close {U = U} r b hf ηEq chain sumEq) =
    η#-inj (trans (sym (trans ηEq (OC.CloseValid.etaOK b))) (HonestFacts.ηCoheres hf)) ,
    trans (sym (OC.CloseValid.valuePreserved b))
      (trans chain (trans (proj₂ (solvency r)) (cong (r₀ +ᵛ_) (sym sumEq))))

  solvency {r₀ = r₀} (s-contest {U = U} r b hf ηEq chain sumEq) =
    η#-inj (trans (sym (trans ηEq (OC.ContestValid.etaOK b))) (HonestFacts.ηCoheres hf)) ,
    trans (sym (OC.ContestValid.valuePreserved b))
      (trans chain (trans (proj₂ (solvency r)) (cong (r₀ +ᵛ_) (sym sumEq))))
```

The certified step forms: the same steps, taking a `Certified` certificate
and deriving the honest facts through the named `honest-certified` assumption.
These are the intended entry points - a real chain history enters the
relation through a certificate, never through free-floating honest facts.

```agda
  s-inc-certified : ∀ {r₀ ctx cid n cp v η η' ada ξ s ref δ# U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → (b : OC.IncrementValid ctx aggKey cid v (OC.Open cid aggKey n cp v η ada)
                             (OC.Open cid aggKey n cp (suc v) η' ada) ξ s ref δ#)
    → (cert : Certified sys snap)
    → Snapshot.etaHash snap ≡ hash η'
    → Snapshot.comHash snap ≡ OC.depositCommitsHashOf ctx ref
    → OC.headValueIn ctx ≡ w
    → OutputRef.txId ref ≢ noTxId
    → ObservedDeposit ctx (HonestFacts.shape (honest-certified cert))
    → IncCoherent U (HonestFacts.committed (honest-certified cert))
                    (HonestFacts.shape (honest-certified cert))
    → SolventReach r₀ (OC.Open cid aggKey n cp (suc v) η' ada)
                   (HonestFacts.committed (honest-certified cert)) (OC.headValue ctx)

  s-dec-certified : ∀ {r₀ ctx cid n cp v η η' ada ξ s m κ# U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → (b : OC.DecrementValid ctx aggKey cid v (OC.Open cid aggKey n cp v η ada)
                             (OC.Open cid aggKey n cp (suc v) η' ada) ξ s m κ#)
    → (cert : Certified sys snap)
    → Snapshot.etaHash snap ≡ hash η'
    → Snapshot.decHash snap ≡ OC.decommitOutputsHashOf ctx m
    → OC.headValueIn ctx ≡ w
    → DecCoherent U (HonestFacts.committed (honest-certified cert))
                    (HonestFacts.shape (honest-certified cert))
    → SolventReach r₀ (OC.Open cid aggKey n cp (suc v) η' ada)
                   (HonestFacts.committed (honest-certified cert)) (OC.headValue ctx)
```

```
  s-inc-certified r b cert ηEq κEq chain realId obs valCo =
    s-inc r b (honest-certified cert) ηEq κEq chain realId obs valCo

  s-dec-certified r b cert ηEq δEq chain valCo =
    s-dec r b (honest-certified cert) ηEq δEq chain valCo

  -- the close/contest forms follow the same one-line pattern.
  s-close-certified : ∀ {r₀ ctx cid n cp v η ada ξ η# δ# κ# s' η' C tfin U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Open cid aggKey n cp v η ada) U w
    → OC.CloseValid ctx aggKey cid v cp s' (OC.Open cid aggKey n cp v η ada)
                    (OC.Closed cid aggKey n cp v s' η' C tfin ada) (OC.closeUnused ξ η# δ# κ#)
    → (cert : Certified sys snap)
    → Snapshot.etaHash snap ≡ η#
    → OC.headValueIn ctx ≡ w
    → sumValue (HonestFacts.committed (honest-certified cert)) ≡ sumValue U
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s' η' C tfin ada)
                   (HonestFacts.committed (honest-certified cert)) (OC.headValue ctx)
  s-close-certified r b cert ηEq chain sumEq =
    s-close r b (honest-certified cert) ηEq chain sumEq

  s-contest-certified : ∀ {r₀ ctx cid n cp v s η C tfin ada ξ η# δ# κ# s' η' kh tfin' U w} {snap : Snapshot}
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s η C tfin ada) U w
    → OC.ContestValid ctx aggKey cid v s tfin (OC.Closed cid aggKey n cp v s η C tfin ada)
                      (OC.Closed cid aggKey n cp v s' η' (kh ∷ C) tfin' ada)
                      (OC.contestUnused ξ η# δ# κ#) kh
    → (cert : Certified sys snap)
    → Snapshot.etaHash snap ≡ η#
    → OC.headValueIn ctx ≡ w
    → sumValue (HonestFacts.committed (honest-certified cert)) ≡ sumValue U
    → SolventReach r₀ (OC.Closed cid aggKey n cp v s' η' (kh ∷ C) tfin' ada)
                   (HonestFacts.committed (honest-certified cert)) (OC.headValue ctx)
  s-contest-certified r b cert ηEq chain sumEq =
    s-contest r b (honest-certified cert) ηEq chain sumEq
```

The payoff. `membersOK` alone says the distributed outputs are members of
_whatever_ the stored accumulator commits to; only together with the
invariant does that become "members of the L2 set the parties actually
built". Combined with the fanout value equation, a solvent head can neither
fan out phantom outputs nor come up short funding the real ones.

```agda
  fanout-covered : ∀ {r₀ d U w ctx m π crs}
    → SolventReach r₀ d U w
    → (fb : OC.FanoutValid ctx d m π crs)
    → OC.headValueIn ctx ≡ w                                   -- L1 continuity (ledger)
    → (OC.distributedOuts ctx m ⊆ U)
      × (w ≡ (OC.takeSumᵛ m (Context.outputs ctx) +ᵛ (OC.burnedValue ctx +ᵛ OC.headAda d)))
```

```
  fanout-covered {d = d} {ctx = ctx} {m = m} {π = π} r fb chain =
    OC.accVerify-sound
      (subst (λ e → OC.accVerify e (OC.distributedOuts ctx m) π ≡ true)
             (proj₁ (solvency r)) (OC.FanoutValid.membersOK fb)) ,
    trans (sym chain) (OC.FanoutValid.valueOK fb)
```

Two companions round the section off. `SolvencyCounterModel` (typecheck-only,
not rendered) keeps the pre-fix attack derivable in miniature: against a
digest covering datum content alone, a copied-datum deposit holding less
value is accepted under the real deposit's signature and leaves the head
insolvent, while the id-binding digest rejects the same claim by identity -
regression documentation the checker rebuilds on every build. Not yet
covered here: the `closeUsed`/`contestUsed` redeemers (their stored
accumulator combines a pending delta, needing a generalized commitment
invariant), partial fanout, and deriving the per-step L2 value-preservation
hypotheses from the off-chain ledger laws.
