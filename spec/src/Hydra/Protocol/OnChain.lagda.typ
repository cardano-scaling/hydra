```
module Hydra.Protocol.OnChain where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Data.Product using (∃-syntax)
open import Data.Integer using (1ℤ)
```

#import "/template.typ": *
#import "/macros.typ": *
#import "/diagrams.typ": transition-arrow, initTx-diagram, depositTx-diagram, recoverTx-diagram, incrementTx-diagram, decrementTx-diagram, closeTx-diagram, contestTx-diagram, fanoutTx-diagram, partialFanoutTx-diagram, finalPartialFanoutTx-diagram

#pagebreak()
= On-chain Protocol <sec:on-chain>
#todo[Update figures]

The following sections describe the _on-chain_ protocol
controlling the life-cycle of a Hydra head, which can be intuitively described
as a state machine (see @fig:head-protocol-states). Each transition
in this state machine is represented and caused by a corresponding Hydra
protocol transaction
on-chain: $mtxInit$~@sec:init-tx, $mtxIncrement$~@sec:increment-tx, $mtxDecrement$~@sec:decrement-tx, $mtxClose$~@sec:close-tx, $mtxContest$~@sec:contest-tx, $mtxFanout$~@sec:fanout-tx, $mtxPartialFanout$~@sec:partial-fanout-tx, and $mtxFinalPartialFanout$~@sec:final-partial-fanout-tx.

The protocol uses KZG accumulators (see @sec:accumulators) to enable partial fanout when UTxO sets exceed transaction size limits. When all UTxOs fit in a single transaction, $mtxFanout$ distributes them all at once. When UTxO sets are too large, $mtxPartialFanout$ distributes subsets across multiple transactions using membership witnesses, transitioning through an intermediate $stFanoutProgress$ state, until $mtxFinalPartialFanout$ completes the distribution.

Besides the main state transitions of the head protocol, there is
the related "deposit protocol" with two transactions in support of
$mtxIncrement$: $mtxDeposit$~@sec:deposit-tx and $mtxRecover$~@sec:recover-tx.
There is also a $mtxDecrement$ transaction~@sec:decrement-tx that allows for taking funds from the Head back to L1.

The head protocol defines one minting policy script and one
validator script:
- $muHead$ governs minting of state and participation tokens in
  $mtxInit$ and burning of these tokens in $mtxFanout$.
- $nuHead$ represents the main protocol state machine logic and ensures
  contract continuity throughout $mtxIncrement$, $mtxDecrement$,
  $mtxClose$, $mtxContest$, $mtxFanout$, $mtxPartialFanout$ and $mtxFinalPartialFanout$.

The deposit protocol defines one validator script:
- $nuDeposit$ controls that $mtxDeposit$ transaction output is
  claimed correctly into a head via $mtxIncrement$ or recovered after
  the deadline has passed in a $mtxRecover$ transaction.

The head output datum $datumHead$ ranges over the protocol states. The state
machine and its per-state fields (as enumerated in the transitions below) are
captured by an Agda type, with the redeemer $redeemerHead$ selecting
the $nuHead$ transition:

```agda
-- Redeemer "hints" for closing/contesting (the CloseType / ContestType unions).
data CloseType : Set where
  closeInitial                  : CloseType
  closeAny closeUnused closeUsed : (ξ : AggSig) (ηhash : ℍ) → CloseType

data ContestType : Set where
  contestUnused contestUsed : (ξ : AggSig) (ηhash : ℍ) → ContestType

data HeadDatum : Set where
  Open : (cid : ℍ) (hydraKey : VKey) (n : ℕ) (contestationPeriod : ℕ)
         (version : ℕ) (η : AccCommitment) (ada : Value) → HeadDatum
  Closed : (cid : ℍ) (hydraKey : VKey) (n : ℕ) (contestationPeriod : ℕ)
           (version : ℕ) (snapshotNumber : ℕ) (η : AccCommitment)
           (contesters : List VKey) (tfinal : ℕ) (ada : Value) → HeadDatum
  FanoutProgress : (cid : ℍ) (hydraKey : VKey) (n : ℕ) (tfinal : ℕ)
                   (η : AccCommitment) (ada : Value) → HeadDatum
  Final : HeadDatum

data HeadRedeemer : Set where
  Increment          : (ξ : AggSig) (s : ℕ) (ref : OutputRef)     → HeadRedeemer
  Decrement          : (ξ : AggSig) (s : ℕ) (m : ℕ)               → HeadRedeemer
  Close              : CloseType                                  → HeadRedeemer
  Contest            : ContestType                                → HeadRedeemer
  Fanout             : (m : ℕ) (π : AccWitness) (crs : OutputRef) → HeadRedeemer
  PartialFanout      : (m : ℕ) (crs : OutputRef)                  → HeadRedeemer
  FinalPartialFanout : (m : ℕ) (π : AccWitness) (crs : OutputRef) → HeadRedeemer
```

The admissible $nuHead$ state transitions are captured as a typed relation
$d ⟶⟨ r ⟩ d'$ ("datum $d$ steps to $d'$ under redeemer $r$"). The relation
encodes the *state-machine shape* and the *version discipline* in the types:
$sans("increment")$/$sans("decrement")$ bump the version (`suc v`),
$sans("close")$/$sans("contest")$ preserve it (the same `v` reappears),
$sans("close")$ initialises the contester list to the empty list, $sans("contest")$
requires the new $keyHash in.not contesters$ (so the list grows by exactly one), and
the partial-fanout rules thread the intermediate $stFanoutProgress$ state through to
$stFinal$. A rule violating any of *these* would fail to type-check. The
remaining per-transaction conditions (signatures, value conservation, deadlines)
are separate predicates (e.g. `closeDeadlineOK`/`contestDeadlineOK` below) applied
alongside it.

```agda
data _⟶⟨_⟩_ : HeadDatum → HeadRedeemer → HeadDatum → Set where

  increment : ∀ {cid hk n cp v η ada η' ξ s ref}
    → Open cid hk n cp v η ada ⟶⟨ Increment ξ s ref ⟩ Open cid hk n cp (suc v) η' ada

  decrement : ∀ {cid hk n cp v η ada η' ξ s m}
    → Open cid hk n cp v η ada ⟶⟨ Decrement ξ s m ⟩ Open cid hk n cp (suc v) η' ada

  close : ∀ {cid hk n cp v η ada s' η' tfin ct}
    → Open cid hk n cp v η ada ⟶⟨ Close ct ⟩ Closed cid hk n cp v s' η' [] tfin ada

  contest : ∀ {cid hk n cp v s η C tfin ada s' η' kh tfin' ct}
    → ¬ (kh ∈ˡ C)                                 -- the contester has not already contested
    → Closed cid hk n cp v s η C tfin ada
        ⟶⟨ Contest ct ⟩ Closed cid hk n cp v s' η' (kh ∷ C) tfin' ada

  fanout : ∀ {cid hk n cp v s η C tfin ada m π crs}
    → Closed cid hk n cp v s η C tfin ada ⟶⟨ Fanout m π crs ⟩ Final

  partialFanoutStart : ∀ {cid hk n cp v s η C tfin ada η' m crs}
    → Closed cid hk n cp v s η C tfin ada
        ⟶⟨ PartialFanout m crs ⟩ FanoutProgress cid hk n tfin η' ada

  partialFanoutStep : ∀ {cid hk n tfin η ada η' m crs}
    → FanoutProgress cid hk n tfin η ada
        ⟶⟨ PartialFanout m crs ⟩ FanoutProgress cid hk n tfin η' ada

  finalPartialFanout : ∀ {cid hk n tfin η ada m π crs}
    → FanoutProgress cid hk n tfin η ada ⟶⟨ FinalPartialFanout m π crs ⟩ Final
```

Beyond the state-machine shape, individual $nuHead$ *checks* are stated as
predicates over the validation $sans("Context")$ and the datums. For example,
the close transaction (@sec:close-tx) requires the recorded contestation
deadline to be the transaction's upper validity bound extended by the
contestation period; as a checkable proposition referencing the context and the
produced datum:

```agda
-- spec §3.4/§5.8 accumulator operations (the §3.4 Accumulator scheme at the
-- protocol's commitment/output/witness types), kept abstract, plus the G1
-- generator representing the empty accumulator.
postulate
  accUTxO          : ℙ Output → AccCommitment   -- commitment to a UTxO set
  accVerify        : AccCommitment → ℙ Output → AccWitness → Bool
  accVerifyExclude : AccCommitment → ℙ Output → AccCommitment → Bool
  G₁               : AccCommitment

-- We do NOT model the KZG construction; we only assume the scheme functions correctly, via
-- these specifying laws (used to connect the on-chain accumulator to the off-chain UTxO sets):
postulate
  -- the empty set commits to the generator (the "empty accumulator" G₁)
  accUTxO-∅       : accUTxO ∅ˢ ≡ G₁
  -- soundness: a verified membership witness attests a genuine subset (no proving non-members)
  accVerify-sound : ∀ {U S π} → accVerify (accUTxO U) S π ≡ true → S ⊆ U
  -- completeness: any genuine subset has a membership witness that verifies
  accVerify-complete : ∀ {U S} → S ⊆ U → ∃[ π ] (accVerify (accUTxO U) S π ≡ true)
  -- a set is always provably a member of its own commitment (the S ≔ U case of completeness; stated
  -- directly to avoid the set-theory `⊆`-reflexivity plumbing). Used by the coverage obligations.
  accVerify-self : ∀ U → ∃[ π ] (accVerify (accUTxO U) U π ≡ true)

-- Set-level cardinality of a UTxO set: the number of outputs a fanout distributes. Postulated (with
-- only the non-emptiness law the coverage proofs need) rather than via the set theory's `card`, which
-- requires strong-finiteness witnesses - this is a SPECIFICATION of the abstract accumulator/fanout's
-- set behaviour, not the KZG construction (cf. the `accVerify-*` laws). It lets the coverage obligations
-- DERIVE `0 < m` for a non-empty remainder instead of assuming it.
postulate
  setSize     : ℙ Output → ℕ
  setSize-pos : ∀ {U} → ¬ (U ≡ ∅ˢ) → 0 < setSize U

-- Sum the value of all outputs / resolved inputs paying to a given script hash (cf. the Plutus
-- `valueLockedBy`/`valueSpent` idiom of folding the tx outputs/inputs at `ownHash`). `ℍ` has
-- decidable equality (`_≟ℍ_`, Prelude), so the per-output membership test is concrete.
valueAtOut : ℍ → List Output → Value
valueAtOut h []       = εᵛ
valueAtOut h (o ∷ os) =
  if ⌊ Output.address o ≟ℍ h ⌋ then Output.value o +ᵛ valueAtOut h os else valueAtOut h os

valueAtIn : ℍ → List Input → Value
valueAtIn h []       = εᵛ
valueAtIn h (i ∷ is) =
  if ⌊ Output.address (Input.resolved i) ≟ℍ h ⌋
    then Output.value (Input.resolved i) +ᵛ valueAtIn h is
    else valueAtIn h is

-- Head value in/out, DERIVED from the context: the value at the νHead script (`Context.ownHash`)
-- among the spent inputs / produced outputs (§5.x head-output identification).
headValueIn : Context → Value
headValueIn ctx = valueAtIn (Context.ownHash ctx) (Context.inputs ctx)

headValue : Context → Value
headValue ctx = valueAtOut (Context.ownHash ctx) (Context.outputs ctx)

-- Decidable equality of output references (txId + index), from `_≟ℍ_` and `_≟_`.
outputRef-eqᵇ : OutputRef → OutputRef → Bool
outputRef-eqᵇ a b = ⌊ OutputRef.txId a ≟ℍ OutputRef.txId b ⌋ ∧ ⌊ OutputRef.index a ≟ OutputRef.index b ⌋

-- Value of the resolved input spending a given output reference (there is exactly one such input).
inputValueAt : OutputRef → List Input → Value
inputValueAt ref []       = εᵛ
inputValueAt ref (i ∷ is) =
  if outputRef-eqᵇ (Input.outputRef i) ref
    then Output.value (Input.resolved i) +ᵛ inputValueAt ref is
    else inputValueAt ref is

-- DERIVED (increment): the value of the spent deposit is the value of the resolved input at the
-- claimed deposit reference `ref` (the same `ref` the increment redeemer carries / `depositSpentOK`
-- requires spent).
depositValueAt : Context → OutputRef → Value
depositValueAt ctx ref = inputValueAt ref (Context.inputs ctx)

-- DERIVED (increment, all deposits): the total value of EVERY spent deposit input -- the value at the
-- νDeposit script (`Context.depHash`) summed over the resolved inputs. This mirrors Plutus
-- `totalNonHeadInputValue` in `checkIncrement` (the sum over every non-head script input, which the
-- non-head SPENT inputs of an increment are exactly the claimed deposits), and is what the value
-- conservation must account for -- forbidding the multi-deposit siphon (an extra deposit whose value
-- is routed away would leave `headValueIn +ᵛ depositsValue ≠ headValue`). For a single-deposit
-- increment it coincides with `depositValueAt ctx ref` (the single-deposit value above).
depositsValue : Context → Value
depositsValue ctx = valueAtIn (Context.depHash ctx) (Context.inputs ctx)

-- DERIVED (decrement): the decommitted value is the total value of the `m` outputs FOLLOWING the head
-- output (output 0), mirroring Plutus `decommitOutputs = take numberOfDecommitOutputs (tail outputs)`
-- in `checkDecrement`. `m` is the decrement redeemer's output count (the same `m` carried by
-- `Decrement ξ s m`). (The head output is identified positionally as output 0,
-- as on-chain; change/fee outputs sit beyond index `m` and are excluded by the `take`.)
takeSumᵛ : ℕ → List Output → Value
takeSumᵛ zero    _        = εᵛ
takeSumᵛ (suc _) []       = εᵛ
takeSumᵛ (suc k) (o ∷ os) = Output.value o +ᵛ takeSumᵛ k os

decommitValue : Context → ℕ → Value
decommitValue ctx m with Context.outputs ctx
... | []     = εᵛ
... | _ ∷ os = takeSumᵛ m os

-- Further obligations whose witnesses involve searching the context value/keys.
postulate
  -- total value of the tokens burned by the transaction (the n+1 νHead PTs + ST at fan-out). The
  -- mint multiset is not modelled, so the burned VALUE stays abstract (the burned COUNT is
  -- `burnedCount` / `burnAllTokensOK`); only the OUTPUT-distribution half of conservation is concrete.
  burnedValue : Context → Value

-- A participant signed (§5.4–5.7): there is a key-hash `kh` that BOTH names one of the transaction's
-- signers AND names a participation token present in the head value - i.e. the value carries the asset
-- (cid , kh) with quantity 1. This is the spec prose `∃ {cid ↦ keyHashᵢ ↦ 1} ∈ valHead' ⇒ keyHashᵢ ∈
-- txKeys`. The PT-presence half is structural via the per-asset
-- projection `quantityOf` (same trust family as `adaOf`/`nonAdaOf`). Only the signer-naming half stays
-- abstract - `keys : ℙ VKey` is the set-theory powerset, whose membership the (opaque) List-Model does
-- not expose for direct querying, so `signerKeyHash ctx kh` (= `∃ vk ∈ keys, hash vk ≡ kh`) is its thin
-- residue. The head currency `cid` is supplied by the caller's datum.
-- SCOPE NOTE: this reads the PT off `headValue` (the produced head OUTPUT). The real validator's
-- `mustBeSignedByParticipant` loops over the spent INPUTS instead. The two coincide here: every bundle
-- carrying this field also preserves/grows the head value with its PTs intact (close/contest/decrement
-- preserve it exactly, increment adds a deposit), so the head input and output carry the same PTs under
-- the single-head-script-UTxO assumption. They could differ only on a malformed multi-head tx.
postulate
  signerKeyHash : Context → ℍ → Set

signedByParticipant : ℍ → Context → Set
signedByParticipant cid ctx =
  ∃[ kh ] (signerKeyHash ctx kh) × (quantityOf (headValue ctx) (cid , kh) ≡ 1ℤ)

-- §5.8 value conservation for (final) fan-out (DERIVED output-sum): the head input value equals the
-- sum of the `m` distributed (fanned-out) outputs PLUS the burned tokens PLUS the ada overhead --
-- mirroring Plutus `mustConserveValue` (`headInValue == Σ fanoutOutputs <> mintValueBurned <> ada`).
-- The distributed outputs are the first `m` (`takeSumᵛ m outputs`, as on-chain `take m txInfoOutputs`,
-- since a full fan-out has no continuing head output); `burnedValue` is the only piece left abstract.
fanoutValueOK : Context → Value → ℕ → Set
fanoutValueOK ctx ada m =
  headValueIn ctx ≡ (takeSumᵛ m (Context.outputs ctx) +ᵛ (burnedValue ctx +ᵛ ada))

-- §5.8.1 value conservation for an intermediate partial fan-out (DERIVED, no burn yet): the head input
-- value equals the CONTINUING head output (the `FanoutProgress` output at the νHead script) plus the
-- `m` distributed outputs after it -- exactly the decrement shape (`headValue +ᵛ decommit ≡ headValueIn`).
partialFanoutValueOK : Context → ℕ → Set
partialFanoutValueOK ctx m =
  headValueIn ctx ≡ (headValue ctx +ᵛ decommitValue ctx m)

-- spec §5.6 (close): the recorded contestation deadline is the transaction's
-- upper validity bound extended by the contestation period, tfinal = txValidityMax
-- + T_contest, and close must produce a Closed datum. (Contest's deadline update
-- in §5.7 is conditional - tfinal' = tfinal if all parties contested, else
-- tfinal + T - and is left to a separate predicate.)
closeDeadlineOK : Context → HeadDatum → Set
closeDeadlineOK ctx (Closed cid hk n cp v s η C tfinal ada) =
  tfinal ≡ ValidityInterval.hi (Context.validity ctx) + cp
closeDeadlineOK ctx _ = ⊥

-- spec §5.6/§5.7: close and contest mint or burn nothing.
noMint : Context → Set
noMint ctx = Context.mint ctx ≡ εᵛ

-- spec §5.6, CloseType = Initial case: closing on the initial snapshot requires
-- version 0, snapshot number 0, and η' = accUTxO(∅). The other close types impose
-- no such constraint.
closeInitialOK : CloseType → HeadDatum → Set
closeInitialOK closeInitial (Closed _ _ _ _ v s η _ _ _) = (v ≡ 0) × (s ≡ 0) × (η ≡ accUTxO ∅ˢ)
closeInitialOK _            _                            = ⊤

-- Value conservation (additive, §5.4/§5.5). The head value grows by the deposit
-- on increment and shrinks by the decommitted value on decrement.
incrementValueOK : (valHead valDeposit valHead' : Value) → Set
incrementValueOK vh vd vh' = vh +ᵛ vd ≡ vh'

decrementValueOK : (valHead valHead' valDecommit : Value) → Set
decrementValueOK vh vh' vdec = vh' +ᵛ vdec ≡ vh

-- spec §5.4–5.7: the snapshot multisignature ξ verifies, under the aggregate
-- hydra key, over the message cid ‖ v ‖ s ‖ η# (shared by increment, decrement,
-- close and contest).
snapshotSigOK : (hydraKey : VKey) (cid : ℍ) (v s : ℕ) (η# : ℍ) (ξ : AggSig) → Set
snapshotSigOK hydraKey cid v s η# ξ = msVfy hydraKey (cid ‖ v ‖ s ‖ η#) ξ ≡ true

-- spec §5.7: contest updates the deadline conditionally - it stays at the previous
-- tfinal once all parties have contested (|contesters'| = n), otherwise it extends
-- by the contestation period T (= cp). Fully computed: contesters is a List, so
-- the cardinality is `length`, and n is carried in the datum.
contestDeadlineOK : HeadDatum → HeadDatum → Set
contestDeadlineOK (Closed _ _ _ _ _ _ _ _ tfinal _) (Closed _ _ n cp _ _ _ C' tfinal' _) =
  tfinal' ≡ (if ⌊ length C' ≟ n ⌋ then tfinal else tfinal + cp)
contestDeadlineOK _ _ = ⊥

-- spec §5.8: (final) fan-out burns all n+1 head tokens (1 ST + n PTs) of policy
-- cid. The number of burned tokens is abstracted by `burnedCount`; the law is the
-- n+1 equality, with n taken from the (FanoutProgress/Closed) datum.
postulate
  burnedCount : Context → ℍ → ℕ  -- count of policy-cid tokens burnt (mint quantity -1)

burnAllTokensOK : Context → HeadDatum → Set
burnAllTokensOK ctx (Closed cid _ n _ _ _ _ _ _ _)      = burnedCount ctx cid ≡ suc n
burnAllTokensOK ctx (FanoutProgress cid _ n _ _ _)      = burnedCount ctx cid ≡ suc n
burnAllTokensOK ctx _                                    = ⊥

-- All distributed outputs are members of the unified accumulator η
-- (fanout §5.8 and final partial fanout §5.8.2).
fanoutMembersOK : (η : AccCommitment) (outs : ℙ Output) (π : AccWitness) → Set
fanoutMembersOK η outs π = accVerify η outs π ≡ true

-- η' is the correct accumulator after excluding the distributed batch S (§5.8.1).
fanoutExcludeOK : (η : AccCommitment) (S : ℙ Output) (η' : AccCommitment) → Set
fanoutExcludeOK η S η' = accVerifyExclude η S η' ≡ true

-- An intermediate partial fan-out has not yet removed everything (η' ≠ G₁); the
-- last batch must use finalPartialFanout instead (§5.8.1).
partialFanoutNotDoneOK : (η' : AccCommitment) → Set
partialFanoutNotDoneOK η' = ¬ (η' ≡ G₁)

-- Signature obligation of a close redeemer: the Initial type carries no signature;
-- the other types must verify a multisignature over cid ‖ v ‖ s ‖ η#.
-- Field extractors used by the validity bundles below.
snapNum : HeadDatum → ℕ
snapNum (Closed _ _ _ _ _ s _ _ _ _) = s
snapNum _ = 0

ηOf : HeadDatum → AccCommitment
ηOf (Open _ _ _ _ _ η _)             = η
ηOf (Closed _ _ _ _ _ _ η _ _ _)     = η
ηOf (FanoutProgress _ _ _ _ η _)     = η
ηOf Final                            = G₁

tfinalOf : HeadDatum → ℕ
tfinalOf (Closed _ _ _ _ _ _ _ _ t _) = t
tfinalOf (FanoutProgress _ _ _ t _ _) = t
tfinalOf _ = 0

-- The ada-overhead value stored in the datum (the head-UTxO lovelace not part of any L2 UTxO, §5.8).
headAda : HeadDatum → Value
headAda (Open _ _ _ _ _ _ ada)             = ada
headAda (Closed _ _ _ _ _ _ _ _ _ ada)     = ada
headAda (FanoutProgress _ _ _ _ _ ada)     = ada
headAda Final                              = εᵛ

-- The redeemer-supplied η# must equal the hash of the accumulator η' actually
-- stored in the produced datum (spec §5.6/§5.7: (η')# = hash(η')) - otherwise the
-- signature would attest to an accumulator unrelated to the on-chain state.
closeηOK : CloseType → HeadDatum → Set
closeηOK closeInitial       _  = ⊤
closeηOK (closeAny _ η#)    d' = η# ≡ hash (ηOf d')
closeηOK (closeUnused _ η#) d' = η# ≡ hash (ηOf d')
closeηOK (closeUsed _ η#)   d' = η# ≡ hash (ηOf d')

contestηOK : ContestType → HeadDatum → Set
contestηOK (contestUnused _ η#) d' = η# ≡ hash (ηOf d')
contestηOK (contestUsed _ η#)   d' = η# ≡ hash (ηOf d')

-- spec §5.6, Any case: the closing snapshot number is positive (s' > 0).
closeAnyOK : CloseType → HeadDatum → Set
closeAnyOK (closeAny _ _) d' = 0 < snapNum d'
closeAnyOK _              _  = ⊤

-- The Used case refers to the *previous* state version v-1 (a pending delta is
-- applied in the snapshot); the others use the current v (spec §5.6).
closeSigOK : (hydraKey : VKey) (cid : ℍ) (v s : ℕ) → CloseType → Set
closeSigOK _  _   _ _ closeInitial       = ⊤
closeSigOK hk cid v s (closeAny ξ η#)    = snapshotSigOK hk cid v s η# ξ
closeSigOK hk cid v s (closeUnused ξ η#) = snapshotSigOK hk cid v s η# ξ
closeSigOK hk cid v s (closeUsed ξ η#)   = snapshotSigOK hk cid (v ∸ 1) s η# ξ

-- A close is *valid* when the state-machine step holds together with all the close
-- checks: the contestation deadline (§5.6), no minting/burning, the Initial-case
-- constraint, and the snapshot signature. The head key/id/version come from the
-- source Open datum, the snapshot number from the produced Closed datum, and the
-- signature/η# from the CloseType redeemer - so the predicate is only inhabited for
-- genuinely valid close transactions. Value is preserved EXACTLY (§5.6: valHead' = valHead).
-- The thin `closeValid` function destructures the source `Open` and produced `Closed` datums
-- (binding the head key/id/version/contestation-period and the produced snapshot number) and is ⊥ for
-- any other shapes; each close check is then a named field of `CloseValid`.
record CloseValid (ctx : Context) (hk : VKey) (cid : ℍ) (v cp s' : ℕ)
                  (d d' : HeadDatum) (ct : CloseType) : Set where
  constructor mkCloseValid
  field
    step              : d ⟶⟨ Close ct ⟩ d'
    deadlineOK        : closeDeadlineOK ctx d'             -- tfinal = validity.hi + cp (§5.6)
    mintEmpty         : noMint ctx
    initialOK         : closeInitialOK ct d'              -- closeInitial ⇒ v=0 ∧ s=0 ∧ η=accUTxO(∅)
    sigOK             : closeSigOK hk cid v s' ct
    etaOK             : closeηOK ct d'                    -- η# bound to stored η'
    anyOK             : closeAnyOK ct d'                  -- closeAny ⇒ 0 < s
    valuePreserved    : headValueIn ctx ≡ headValue ctx   -- value preserved EXACTLY (§5.6; matches Plutus `mustPreserveHeadValue`, `==`)
    participantSigned : signedByParticipant cid ctx
    -- validity range bounded so the deadline is at most 2·T ahead (§5.6)
    validityBounded   : ValidityInterval.hi (Context.validity ctx) ∸ ValidityInterval.lo (Context.validity ctx) ≤ cp

closeValid : Context → HeadDatum → HeadDatum → CloseType → Set
closeValid ctx d@(Open cid hk _ cp v _ _) d'@(Closed _ _ _ _ _ s' _ _ _ _) ct = CloseValid ctx hk cid v cp s' d d' ct
closeValid _ _ _ _ = ⊥

-- Contest signature obligation (both ContestType cases must verify).
-- As for close, the Used case verifies against the previous version v-1 (§5.7).
contestSigOK : (hydraKey : VKey) (cid : ℍ) (v s : ℕ) → ContestType → Set
contestSigOK hk cid v s (contestUnused ξ η#) = snapshotSigOK hk cid v s η# ξ
contestSigOK hk cid v s (contestUsed ξ η#)   = snapshotSigOK hk cid (v ∸ 1) s η# ξ

-- Validity bundles for the remaining transactions: each conjoins the state-machine
-- step with the checks expressible from the datums/redeemer/context. They are only
-- inhabited for genuinely valid transactions.
--
-- SCOPE CAVEAT: what these bundles TYPE-ENFORCE is the state-machine shape, the version discipline,
-- contester growth/dedup, the deadline equations, close-inits-∅, the head value in/out (DERIVED:
-- `headValue`/`headValueIn` sum the value at `ownHash` over the produced outputs / resolved inputs),
-- the increment deposit value (DERIVED: `depositsValue` sums the value at the νDeposit script
-- `depHash` over ALL spent inputs, as Plutus `totalNonHeadInputValue`) and the decrement decommit
-- value (DERIVED: `decommitValue` sums the `m` outputs after the head output, as Plutus
-- `take m (tail outputs)`), and the participant signature (DERIVED for close/contest/increment/decrement
-- only - fanout/partial-fanout have no such field: `signedByParticipant cid ctx` is a structural
-- `∃ kh, signerKeyHash ctx kh × quantityOf valHead (cid, kh) ≡ 1`). What remains abstracted: the value
-- ARITHMETIC laws (`_+ᵛ_`/`_≤ᵛ_`/`εᵛ`) and per-asset projection `quantityOf` on the opaque `Value`,
-- crypto (`msVfy`/`snapshotSigOK`) and accumulator ops (`accVerify`/`accVerifyExclude`/`accUTxO`), all
-- via postulated laws. So value CONSERVATION is stated over real head, increment-deposit AND
-- decrement-decommit values (modulo the abstract value algebra); signature/accumulator soundness is
-- assumed.
-- A contest replaces the closed snapshot with a more recent one and appends the contester. The thin
-- `contestValid` function destructures the source `Closed` datum (binding its key/id/version/snapshot/
-- deadline) and is ⊥ for any other source shape.
record ContestValid (ctx : Context) (hk : VKey) (cid : ℍ) (v s tfin : ℕ)
                    (d d' : HeadDatum) (ct : ContestType) : Set where
  constructor mkContestValid
  field
    step              : d ⟶⟨ Contest ct ⟩ d'
    deadlineOK        : contestDeadlineOK d d'
    mintEmpty         : noMint ctx
    sigOK             : contestSigOK hk cid v (snapNum d') ct
    etaOK             : contestηOK ct d'                      -- η# bound to stored η' (§5.7)
    snapIncreases     : s < snapNum d'                        -- snapshot strictly increases (§5.7)
    beforeDeadline    : ValidityInterval.hi (Context.validity ctx) ≤ tfin  -- posted before the deadline
    valuePreserved    : headValueIn ctx ≡ headValue ctx       -- value preserved EXACTLY (§5.7; matches Plutus `mustPreserveHeadValue`, `==`)
    participantSigned : signedByParticipant cid ctx

contestValid : Context → HeadDatum → HeadDatum → ContestType → Set
contestValid ctx d@(Closed cid hk _ _ v s _ _ tfin _) d' ct = ContestValid ctx hk cid v s tfin d d' ct
contestValid _ _ _ _ = ⊥

-- The claimed deposit OutputRef is actually spent by the transaction (§5.4: txOutRef_increment =
-- txOutRef_deposit). Unlike the postulated value/crypto extractors this is a STRUCTURAL check over
-- the context's inputs, so it is defined concretely. (The νDeposit validator's own checks remain
-- out of scope; see the deposit/recover note.)
depositSpentOK : Context → OutputRef → Set
depositSpentOK ctx ref = ∃[ i ] (i ∈ˡ Context.inputs ctx) × (Input.outputRef i ≡ ref)

-- Increment: a confirmed deposit is collected into the head (version bumps to `suc v`, head value
-- grows by ALL spent deposits). The thin `incrementValid` function destructures the source `Open`
-- datum to feed the head key/id/version into the record, and is ⊥ for any other source shape.
record IncrementValid (ctx : Context) (hk : VKey) (cid : ℍ) (v : ℕ)
                      (d d' : HeadDatum) (ξ : AggSig) (s : ℕ) (ref : OutputRef) : Set where
  constructor mkIncrementValid
  field
    step              : d ⟶⟨ Increment ξ s ref ⟩ d'
    mintEmpty         : noMint ctx
    sigOK             : snapshotSigOK hk cid v s (hash (ηOf d')) ξ
    valueOK           : incrementValueOK (headValueIn ctx) (depositsValue ctx) (headValue ctx)  -- ALL deposits (§5.4, Plutus `totalNonHeadInputValue`)
    depositSpent      : depositSpentOK ctx ref            -- claimed deposit is spent (§5.4)
    participantSigned : signedByParticipant cid ctx

incrementValid : Context → HeadDatum → HeadDatum → AggSig → ℕ → OutputRef → Set
incrementValid ctx d@(Open cid hk _ _ v _ _) d' ξ s ref = IncrementValid ctx hk cid v d d' ξ s ref
incrementValid _ _ _ _ _ _ = ⊥

-- Decrement: a decommit removes the `m` outputs after the head output (version bumps, head value
-- shrinks by the decommit). Same source-shape dispatch as increment.
record DecrementValid (ctx : Context) (hk : VKey) (cid : ℍ) (v : ℕ)
                      (d d' : HeadDatum) (ξ : AggSig) (s : ℕ) (m : ℕ) : Set where
  constructor mkDecrementValid
  field
    step              : d ⟶⟨ Decrement ξ s m ⟩ d'
    mintEmpty         : noMint ctx
    sigOK             : snapshotSigOK hk cid v s (hash (ηOf d')) ξ
    valueOK           : decrementValueOK (headValueIn ctx) (headValue ctx) (decommitValue ctx m)
    participantSigned : signedByParticipant cid ctx

decrementValid : Context → HeadDatum → HeadDatum → AggSig → ℕ → ℕ → Set
decrementValid ctx d@(Open cid hk _ _ v _ _) d' ξ s m = DecrementValid ctx hk cid v d d' ξ s m
decrementValid _ _ _ _ _ _ = ⊥

-- Fan-out is posted after the deadline (txValidityMin > tfinal), distributes m
-- outputs that are members of η, conserves value, and burns all n+1 tokens (§5.8).
-- NB m = 0 is permitted: it is the (only) way to finalise a genuinely EMPTY head - distribute
-- nothing, burn the n+1 tokens. Value conservation (exact, via `valueOK`) prevents any theft at m = 0,
-- so no `0 < m` guard is imposed on the FULL fanout (unlike the partial paths, where a 0-output batch
-- makes no progress); this matches the real νHead `headIsFinalizedWith` (no `numberOfFanoutOutputs > 0`).
record FanoutValid (ctx : Context) (d : HeadDatum) (outs : ℙ Output) (m : ℕ) (π : AccWitness) (crs : OutputRef) : Set where
  constructor mkFanoutValid
  field
    step            : d ⟶⟨ Fanout m π crs ⟩ Final
    burnAllTokens   : burnAllTokensOK ctx d                    -- burns the n+1 tokens (§5.8)
    membersOK       : fanoutMembersOK (ηOf d) outs π           -- distributed outputs ∈ η
    afterDeadline   : tfinalOf d < ValidityInterval.lo (Context.validity ctx)
    valueOK         : fanoutValueOK ctx (headAda d) m

fanoutValid : Context → HeadDatum → ℙ Output → ℕ → AccWitness → OutputRef → Set
fanoutValid = FanoutValid

-- A *partial* fan-out distributes m > 0 of the still-locked outputs and carries the head on to a
-- `FanoutProgress`. Each conjunct is a named field; the `step` field forces d' to be a `FanoutProgress`
-- (so `ηOf d'` is its accumulator η'), which is why no wrong-shape ⊥ case is needed.
record PartialFanoutValid (ctx : Context) (d d' : HeadDatum) (S : ℙ Output) (m : ℕ) (crs : OutputRef) : Set where
  constructor mkPartialFanoutValid
  field
    step            : d ⟶⟨ PartialFanout m crs ⟩ d'
    excludeOK       : fanoutExcludeOK (ηOf d) S (ηOf d')      -- distributed outputs removed from η (→ η')
    notDoneOK       : partialFanoutNotDoneOK (ηOf d')          -- η' still non-empty (more to fan out)
    outputsPositive : 0 < m                                   -- §5.8 no zero-output batch
    afterDeadline   : tfinalOf d < ValidityInterval.lo (Context.validity ctx)  -- posted after tfinal
    mintEmpty       : noMint ctx
    valueOK         : partialFanoutValueOK ctx m              -- value conserved (modulo abstract algebra)

partialFanoutValid : Context → HeadDatum → HeadDatum → ℙ Output → ℕ → OutputRef → Set
partialFanoutValid = PartialFanoutValid

-- The last batch of a multi-step fan-out: like `FanoutValid` but from a `FanoutProgress` source.
record FinalPartialFanoutValid (ctx : Context) (d : HeadDatum) (outs : ℙ Output) (m : ℕ) (π : AccWitness) (crs : OutputRef) : Set where
  constructor mkFinalPartialFanoutValid
  field
    step            : d ⟶⟨ FinalPartialFanout m π crs ⟩ Final
    burnAllTokens   : burnAllTokensOK ctx d
    membersOK       : fanoutMembersOK (ηOf d) outs π
    outputsPositive : 0 < m
    afterDeadline   : tfinalOf d < ValidityInterval.lo (Context.validity ctx)
    valueOK         : fanoutValueOK ctx (headAda d) m

finalPartialFanoutValid : Context → HeadDatum → ℙ Output → ℕ → AccWitness → OutputRef → Set
finalPartialFanoutValid = FinalPartialFanoutValid

-- ── §5.1 init (μHead minting policy) ────────────────────────────────────────────────────────────
-- A head is created by the seed-parameterised μHead policy. `cid` is the seed-derived policy id, the
-- seed is spent (so the EUTxO ledger guarantees `cid` is unique), exactly n+1 tokens of `cid` are
-- minted (1 ST + n PTs), and the produced head output is a well-formed initial Open (version 0,
-- η = accUTxO ∅). Init has no predecessor datum, so it is a creation PREDICATE, not a `_⟶⟨_⟩_` step.
-- Token PLACEMENT into the head output is modelled (`stPlaced`/`tokensPlaced`, via the `stQty`/
-- `headTokenCount` value projections): the head output carries exactly the n+1 head-policy tokens, one
-- being the ST. Together with the n+1 MINT count (`mintedCountOK`) this pins that every minted token is
-- placed in the head output (form (a): the count + ST presence; naming the individual PTs would need the
-- per-party key list, which the on-chain `Open` datum abstracts into `hk`/`n`).
postulate
  μHead       : OutputRef → Script  -- the seed-parameterised minting policy script
  mintedCount : Context → ℍ → ℕ     -- count of policy-cid tokens minted (positive mint quantity)

-- `mintedCountOK`/`stPlaced`/`tokensPlaced` are bridged + differentially tested (`initValid→ref`, the
-- `InitDifferential` suite). The thin `initValid` function dispatches on the produced `Open` head datum
-- (binding its cid/n/v/η) and is ⊥ for any other produced shape.
-- WHY count + ST presence pins placement: each head-policy token is minted with quantity 1 (the μHead
-- policy), so the mint COUNT (n+1) equals the number of DISTINCT tokens; with the head output carrying
-- n+1 DISTINCT head-policy tokens (`tokensPlaced`) and the ST among them (`stPlaced`), every minted token
-- lands in the head output (pigeonhole) - no token can be minted-but-not-placed or duplicated.
record InitValid (ctx : Context) (seed : OutputRef) (cid : ℍ) (n v : ℕ) (η : AccCommitment) : Set where
  constructor mkInitValid
  field
    cidIsSeedHash : cid ≡ hash (μHead seed)        -- cid = hash(μHead(seed)) (§5.1)
    seedSpent     : depositSpentOK ctx seed         -- the seed output is spent (uniqueness of cid)
    mintedCountOK : mintedCount ctx cid ≡ suc n     -- mints exactly n+1 tokens of policy cid (1 ST + n PTs)
    stPlaced      : stQty (headValue ctx) cid ≡ 1   -- the ST is placed in the head output
    tokensPlaced  : headTokenCount (headValue ctx) cid ≡ suc n  -- all n+1 head-policy tokens placed there
    versionZero   : v ≡ 0                           -- initial snapshot version
    etaEmpty      : η ≡ accUTxO ∅ˢ                   -- accumulator commits to the empty initial UTxO set

initValid : Context → OutputRef → HeadDatum → Set
initValid ctx seed (Open cid hk n cp v η ada) = InitValid ctx seed cid n v η
initValid _ _ _ = ⊥

-- ── §5.2–5.3 deposit / recover (νDeposit) ───────────────────────────────────────────────────────
-- A deposit locks funds for later collection into the head via increment. The datum records the
-- target head `cid`, a recover deadline, and the committed outputs C (refs + serialised outputs).
record DepositDatum : Set where
  constructor mkDepositDatum
  field
    depCid    : ℍ                      -- target head currency id
    tRecover  : ℕ                      -- deadline after which the deposit can be recovered
    committed : List (OutputRef × ℍ)   -- C: deposited output refs + their serialisations

data DepositRedeemer : Set where
  claim   : DepositRedeemer
  recover : ℕ → DepositRedeemer        -- m = number of outputs to recover

-- The deposit transaction itself has NO on-chain verification (§5.2: any payment to νDeposit is a
-- deposit; eligibility is an off-chain check), so there is no `depositValid`. §5.3 recover: νDeposit,
-- on `recover m`, checks the recovered outputs match the deposited ones (a serialisation-hash
-- equality, abstracted) and that the transaction is posted strictly after the recover deadline.
postulate
  recoveredMatchesDeposited : Context → List (OutputRef × ℍ) → ℕ → Set  -- §5.3.1 hash(⊕oⱼ)=hash(C)

-- The after-deadline conjunct is bridged + differentially tested (`recoverValid→ref` in
-- `ReferenceBridge`, exercised by the `DepositDifferential` hydra-tx suite); the hash-equality conjunct
-- stays abstracted (injected mock in the differential), as for the close/inc crypto conjuncts.
record RecoverValid (ctx : Context) (dd : DepositDatum) (m : ℕ) : Set where
  constructor mkRecoverValid
  field
    recoveredMatches     : recoveredMatchesDeposited ctx (DepositDatum.committed dd) m  -- recovered exactly as deposited
    afterRecoverDeadline : DepositDatum.tRecover dd < ValidityInterval.lo (Context.validity ctx)  -- §5.3.2: txValidityMin > t_recover

recoverValid : Context → DepositDatum → ℕ → Set
recoverValid = RecoverValid

-- §5.2 Claim: a deposit is collected by an increment of its OWN head. The head-binding `cid = hcid`:
depositClaimedBy : DepositDatum → HeadDatum → Set
depositClaimedBy (mkDepositDatum cid _ _) (Open hcid _ _ _ _ _ _) = cid ≡ hcid
depositClaimedBy _ _ = ⊥

-- §5.2 Claim validity (νDeposit), mirroring `deposit.ak`'s Claim arm: a deposit output may be spent on
-- `claim` only if (i) it is consumed by an increment of ITS OWN head -- the deposit datum's `cid`
-- equals the head datum `hd` being spent in the same transaction (deposit.ak
-- `expect_increment_redeemer(self, datum.head_id)`, modelled here as `depositClaimedBy`), and (ii) the
-- transaction is posted strictly BEFORE the recover deadline (deposit.ak `before_deadline`:
-- txValidityMax ≤ tRecover). These are the two checks νDeposit performs that νHead does NOT; the
-- "spent with the Increment redeemer" half is enforced head-side by `incrementValid`/`depositSpentOK`.
-- (claim carries no payload, so `claimValid` takes the head datum directly rather than via a redeemer.)
record ClaimValid (ctx : Context) (dd : DepositDatum) (hd : HeadDatum) : Set where
  constructor mkClaimValid
  field
    claimedByOwnHead      : depositClaimedBy dd hd             -- deposit's cid binds to the head's (deposit.ak `expect_increment_redeemer`)
    beforeRecoverDeadline : ValidityInterval.hi (Context.validity ctx) ≤ DepositDatum.tRecover dd  -- deposit.ak `before_deadline`

claimValid : Context → DepositDatum → HeadDatum → Set
claimValid = ClaimValid

-- A deposit is collected by an INCREMENT transaction that spends both the head and the deposit. Such a
-- transaction must satisfy BOTH validators run in it: νHead's `incrementValid` (version/value/signature)
-- AND νDeposit's `claimValid` for the claimed deposit datum `dd` (its `cid` binds to the head, and the
-- claim is before the recover deadline). Conjoining them here makes the deposit→head binding a
-- type-enforced part of a valid claim transaction, mirroring
-- that on-chain BOTH scripts must pass. (`dd` is supplied like `recoverValid`'s, not decoded here.)
-- NB: this type-ENCODES the joint requirement (so `depositClaimedBy`/`claimValid` are both used);
-- the Claim arm is not PROVED-against-Plutus -- there is no `claimTxValid→ref` bridge lemma and no
-- Claim-path differential test, so deposit.ak's Claim arm remains a hand-reviewed coverage boundary.
-- (The Recover arm IS bridged + differentially tested: `recoverValid→ref` + the `DepositDifferential`
-- suite cover deposit.ak's after-deadline check.)
record ClaimTxValid (ctx : Context) (dd : DepositDatum) (headIn headOut : HeadDatum)
                    (ξ : AggSig) (s : ℕ) (ref : OutputRef) : Set where
  constructor mkClaimTxValid
  field
    headSideOK    : incrementValid ctx headIn headOut ξ s ref  -- νHead: version / value / signature
    depositSideOK : claimValid ctx dd headIn                   -- νDeposit: cid-binding + before-deadline

claimTxValid : Context → DepositDatum → HeadDatum → HeadDatum → AggSig → ℕ → OutputRef → Set
claimTxValid = ClaimTxValid
```

== Init transaction <sec:init-tx>

The $mtxInit$ transaction creates a head instance and establishes the initial
state of the protocol and is shown in @fig:initTx. The head
instance is represented by the unique currency identifier $cid$ created by
minting tokens using the $muHead$ minting policy script which is parameterized
by a single output reference parameter $seed in tyOutRef$:
$ cid = hash(muHead(seed)) $

#todo[Update initTx.svg to show direct Open output with all tokens.]

Two kinds of tokens are minted:
- A single _State Thread (ST)_ token marking the head output. This
  output contains the state of the protocol on-chain and the token ensures
  contract continuity. The token name is the well known string
  `HydraHeadV2`, i.e.
  $st = {cid |-> #raw("HydraHeadV2") |-> 1}$.
- One _Participation Token (PT)_ per participant
  $i in {1 dots.h |hydraKeys|}$ to be used for authenticating further
  transactions. The token name is the participant's verification key hash
  $keyHash_i = hash(msVK_i)$ of the verification key as received
  during protocol setup, i.e.
  $pt_i = {cid |-> keyHash_i |-> 1}$.

All minted tokens ($st$ and all $pt_i$) are placed directly into
the head output, which is created in the $stOpen$ state with an empty UTxO set.
Consequently, the $mtxInit$ transaction
- has at least input $seed$,
- mints the state thread token $st$, and one $pt$ for each of the $|hydraKeys|$
  participants with policy $cid$,
- has one head output
  $o_(sans("head"))$, which captures
  the open state of the protocol in the datum, i.e. the `Open` constructor of
  `HeadDatum` (defined above) instantiated with
  - $stOpen$ is the state identifier,
  - $cid'$ is the unique currency id of this instance,
  - $hydraKeys$ are the off-chain multi-signature keys from the setup
    phase,
  - $nop$ is the number of participants,
  - $Tcontest$ is the contestation period,
  - $v = 0$ is the initial snapshot version,
  - $eta = accUTxO(emptyset)$ is the accumulator commitment to the (empty) initial
    UTxO set (its hash $eta^(\#) = hash(eta)$ is what later snapshot signatures attest to).

#block(inset: (left: 1em), {
  emph[Implementation note (datum representation).]
  [ The Agda `Open` datum carries the accumulator _commitment_ $eta$, whereas the on-chain
  implementation stores only its _hash_ $eta^(\#)$ in the open state (the
  `OpenDatum.accumulatorHash` field); the full commitment is materialised only in the
  $stClosed$ / $stFanoutProgress$ states, where fan-out membership proofs require it. The
  two coincide for every open-state check, since those reference $eta$ only through
  $hash(eta)$ (e.g. the close / increment / decrement signature over
  $cid || v || s || eta^(\#)$).]
})

The $muHead(seed)$ minting policy is the only script that verifies
init transactions and can be redeemed with either a $sans("Mint")$ or
$sans("Burn")$ redeemer:
- When evaluated with the $sans("Mint")$ redeemer,
  + The seed output is spent in this transaction. This guarantees uniqueness of the policy $cid$ because the EUTxO ledger ensures that $seed$ cannot be spent twice in the same chain.
    $(seed, dot.c) in txInputs$
  + All entries of $txMint$ are of this policy and of single quantity $forall {c |-> dot.c |-> q} in txMint : c = cid and q = 1$
  + Right number of tokens are minted $|txMint| = |hydraKeys| + 1$
  + State token is sent to the head validator $st in valHead$
  + All participation tokens are sent to the head output alongside the state token $forall i in [1 dots.h |hydraKeys|] : pt_i in valHead$
  + The $datum_(sans("head"))$ contains own currency id $cid = cid'$ and the right seed reference $seed = seed'$
- When evaluated with the $sans("Burn")$ redeemer,
  + All tokens for this policy in $txMint$ need to be of negative quantity
    $forall {cid |-> dot.c |-> q} in txMint : q < 0$.

*Important:* The $muHead$ minting policy only ensures
uniqueness of $cid$ and that the right amount of tokens have been minted and
sent to $nuHead$, while $nuHead$ in turn ensures continuity of the contract.
However, it is *crucial* that all head members check:
- That the transaction mints exactly the correct tokens: one $st$ token and one $pt$ for each head member (total $|hydraKeys| + 1$ tokens). This distinguishes $mtxInit$ from $mtxIncrement$ and $mtxDecrement$ transactions, which only move tokens without minting.
- That the head output contains an $st$ token of policy $cid$ which satisfies $cid = hash(muHead(seed))$. The $seed$ spent by this transaction can be used to determine this.
- That the correct verification key hashes are used in the $pt$s and the open state is consistent with parameters agreed during setup.
See the initialTx behavior in @fig:off-chain-prot for details about these checks.
The decidable core of these checks is formalised as the `initValid` predicate (a _creation_
predicate - init has no predecessor datum, so it is not a `_⟶⟨_⟩_` step): $cid = hash(muHead(seed))$,
the seed is spent, exactly $nop + 1$ tokens of $cid$ are minted, and the produced Open is initial
($v = 0$, $eta = accUTxO(emptyset)$). Token placement into the head value is also modelled (the state
token is present and the head output carries exactly the $nop + 1$ head-policy tokens); the seed-spend
and datum binding remain hand-reviewed boundaries.

#figure(initTx-diagram, caption: [$mtxInit$ transaction spending a seed UTxO and producing the head output directly in state $stOpen$.]) <fig:initTx>

== Deposit Transaction <sec:deposit-tx>

The $mtxDeposit$ transaction locks funds in $nuDeposit$ for later
collection into the head via an $mtxIncrement$ transaction. Any transaction
paying to $nuDeposit$ is a $mtxDeposit$ transaction as there is no on-chain
verification in $mtxDeposit$ transactions. Consequently, protocol actors
*must ensure off-chain* that a valid datum is used when paying to the
$nuDeposit$ validator.#todo[explain why this is enough?]

A valid deposit output is governed by $nuDeposit$ with value $valDeposit$ and datum
$ datumDeposit = (cid, t_(sans("recover")), C) $
where
- $cid$ is the currency id of the target head protocol instance (see~@sec:init-tx),
- $t_(sans("recover"))$ is a deadline after which the deposit can be recovered, and
- $C in (txInputs times tyBytes)^(*)$ is a list of transaction output
  references with along with serialized outputs that should become available in
  the head.

Head protocol participants determine *off-chain* whether a
deposit output $o_(sans("deposit"))$ is eligible for their head by checking
+ $cid$ matches their head identifier,
+ $t_(sans("recover"))$ is reasonably far in the future, and#todo[explain; relate to contestation period?]
+ $valDeposit$ contains the value of all decoded outputs of $C$ from $datumDeposit$.

An example transaction which records all its spent inputs in a deposit output is
shown in @fig:depositTx. The $j in {1 dots.h m}$ inputs of this example with reference $txOutRef_(sans("deposited")_j)$ each spend output $o_(sans("deposited")_j)$ with $val_(sans("deposited")_j)$ would be recorded in the output datum as
$ C = forall j in {1 dots.h m} : [(txOutRef_(sans("deposited")_j), bytes(o_(sans("deposited")_j)))] $
and the value check would need to satisfy
$ valDeposit supset.eq union.big_(j=1)^(m) val_(sans("deposited")_j) $

#figure(depositTx-diagram, caption: [$mtxDeposit$ transaction spending multiple UTxO into a deposit output.]) <fig:depositTx>

== Recover Transaction <sec:recover-tx>

If a $mtxDeposit$ transaction output (see~@sec:deposit-tx) was
not collected into a head by an $mtxIncrement$
transaction~@sec:increment-tx, the $mtxRecover$ transaction
(@fig:recoverTx) allows for restoring the UTxO as recorded in the
deposit after the deadline has passed. It consists of
- one input spending from $nuDeposit$ with datum $datumDeposit = (cid, t_(sans("recover")), C)$, and
- outputs $o_1 dots.h o_m$ to recover UTxOs.

The script validator $nuDeposit$ is spent with redeemer
$redeemerDeposit = (sans("Recover"), m)$, where $m$ is the number of outputs
to recover, and checks:
+ All UTxOs are recovered exactly as they were deposited. This is done by
  comparing hashes of serialised representations of the $m$ recovering outputs
  $o_1 dots.h o_m$ with the canonically combined deposited UTxOs in $C$:
  $ hash(plus.o.big_(j=1)^(m) bytes(o_j)) = hash(sans("concat")(sortOn(1, C)^(arrow.b 2))) $
+ Transaction is posted after the deadline
  $ txValidityMin > t_(sans("recover")) $

The deposit datum and redeemer are formalised as `DepositDatum` / `DepositRedeemer`, and the recover
checks as the `recoverValid` predicate: the recovered outputs match the deposited ones
(`recoveredMatchesDeposited`, the §5.3.1 serialisation-hash equality, abstracted) and the transaction
is posted strictly after the deadline (`t_recover < txValidityMin`, concrete). A deposit's collection
into the head (Claim) is authorised by the `incrementValid` predicate's `depositSpentOK` check (§5.4);
`depositClaimedBy` records that the deposit's `cid` must match the head it is claimed into.

#figure(recoverTx-diagram, caption: [$mtxRecover$ transaction restoring UTxO of a deposit output.]) <fig:recoverTx>

== Increment Transaction <sec:increment-tx>

The $mtxIncrement$ transaction (@fig:incrementTx) allows
a participant to add a $mtxDeposit$ output~@sec:deposit-tx to an already
open head using a snapshot that approves inclusion. Consequently this
transaction consists of:

- one input spending from $nuHead$ with value $valHead$ holding the
  $st$ and datum $datumHead$,
- one input $txOutRef_(sans("deposit"))$ spending from $nuDeposit$ with value $valDeposit$ and datum
  $datumDeposit = (cid_(sans("deposit")), t_(sans("recover")), C)$,
- one output paying to $nuHead$ with value $valHead'$ and datum
  $datumHead'$.

The deposit validator $nuDeposit$ is spent with
$redeemerDeposit = sans("Claim")$ and ensures:
+ Claiming head id matches the deposit datum
  $ cid = cid_(sans("deposit")) $
+ Transaction is posted before the deadline
  $ txValidityMax <= t_(sans("recover")) $

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("increment"), xi, s, txOutRef_(sans("increment")))$,
where $xi$ is a multi-signature of the increment snapshot which authorizes
addition of deposited UTxO, $s$ is the snapshot number and
$txOutRef_(sans("deposit"))$ points to the claimed deposit. The validator
checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stOpen$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$:
  #transition-arrow("increment")
  (the `increment` rule of `_⟶⟨_⟩_`; the version is bumped, $v |-> v + 1$).
+ New version $v'$ is incremented correctly
  $ v' = v + 1 $
+ Claimed deposit is spent
  $ txOutRef_(sans("increment")) = txOutRef_(sans("deposit")) $
+ $xi$ is a valid multi-signature of the new head state $eta'$
  $ msVfy(hydraKeys, (cid || v || s || (eta')^(\#)), xi) = mtrue $
  where $(eta')^(\#) = hash(eta')$ is the hash of the new accumulator commitment $eta'$
  stored in the output datum, reflecting the UTxO set after adding the deposited UTxOs.
+ The value in the head output is increased accordingly
  $ valHead plus.o valDeposit = valHead' $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ The ADA overhead $adaO$ is preserved across the state transition:
  $ adaO' = adaO $

#figure(incrementTx-diagram, caption: [$mtxIncrement$ transaction spending an open head output, producing a new head output which includes the new UTxO.]) <fig:incrementTx>

== Decrement Transaction <sec:decrement-tx>

The $mtxDecrement$ transaction (@fig:decrementTx) allows
a party to remove a UTxO from an already open head and consists of:

- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$,
- one or more decommit outputs $o_2 dots.h o_(m+1)$ with values $val_2 dots.h val_(m+1)$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("decrement"), xi, s, m)$, where $xi$ is a multi-signature of
the decrement snapshot which authorizes removal of some UTxO, $s$ is the
used snapshot number and $m$ is the number of outputs to distribute. The
validator checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stOpen$, parameters $cid, hydraKeys, nop, Tcontest$ stay
  unchanged and the new state is governed again by $nuHead$
  #transition-arrow("decrement")
  (the `decrement` rule of `_⟶⟨_⟩_`; the version is bumped, $v |-> v + 1$).
+ New version $v'$ is incremented correctly
  $ v' = v + 1 $
+ $xi$ is a valid multi-signature of the new snapshot state $eta'$
  $ msVfy(hydraKeys, (cid || v || s || (eta')^(\#)), xi) = mtrue $
  where $(eta')^(\#) = hash(eta')$ is the hash of the new accumulator commitment $eta'$
  stored in the output datum, reflecting the UTxO set after removing the decommitted UTxOs.
+ The value in the head output is decreased accordingly
  $ valHead' plus.o (plus.o.big_(j=2)^(m+1) val_j) = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ The ADA overhead $adaO$ is preserved across the state transition:
  $ adaO' = adaO $

#figure(decrementTx-diagram, caption: [$mtxDecrement$ transaction spending an open head output, producing a new head output and multiple decommitted outputs.]) <fig:decrementTx>

== Close Transaction <sec:close-tx>

In order to close a head, a head member may post the $mtxClose$ transaction
(see @fig:closeTx). This transaction has
- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("close"), sans("CloseType"))$, where
$sans("CloseType")$ is a hint against which open state to close. (The closing
party posts $sans("postTx")(mtxClose, hatv, macron(mc(S)).v, macron(mc(S)).s, (eta')^(\#), xi)$
off-chain; on-chain the redeemer carries only $(xi, (eta')^(\#))$ in
$sans("CloseType")$, while the version $v$ and snapshot number $s$ are
authenticated by the multisignature $xi$ over $cid || v || s || (eta')^(\#)$ and
recorded in the datum, rather than being separate redeemer fields.) The
transaction checks:
+ State is advanced from $datumHead tilde stOpen$ to
  $datumHead' tilde stClosed$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$
  #transition-arrow("close")
  (the `close` rule of `_⟶⟨_⟩_`; the version is preserved, $v' = v$).
  The closed state carries a single unified accumulator $eta'$ that combines the snapshotted UTxO set with any pending increment or decrement UTxOs using $accCombine$.
+ Last known open state version is recorded in closed state
  $ v' = v $

+ Based on the redeemer $sans("CloseType") = sans("Initial") union (sans("Any"), xi, (eta')^(\#)) union (sans("Unused"), xi, (eta')^(\#)) union (sans("Used"), xi, (eta')^(\#))$, where $xi$ is a multi-signature of the closing snapshot and $(eta')^(\#)$ is the hash of the unified accumulator commitment stored in the output datum, four cases are distinguished. In each case the closed state carries a single unified accumulator $eta'$:
  + $sans("Initial")$: The initial snapshot is used to close the head and open state was not updated. No signatures are available and it suffices to check
    $ v = 0 $
    $ s' = 0 $
    $ eta' = accUTxO(emptyset) $
  + $sans("Any")$: Closing snapshot refers to current state version $v$ with no pending increments or decrements, and $s' > 0$. The unified accumulator is simply the snapshotted state:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Unused")$: Closing snapshot refers to current state version $v$ and a pending increment or decrement is _not_ applied in the snapshot. The unified accumulator is the snapshotted state only:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Used")$: Closing snapshot refers to the previous state version $v - 1$ and a pending increment or decrement _is_ applied in the snapshot. The unified accumulator combines the snapshotted UTxOs with the pending delta:
    $ eta' = accCombine(accUTxO(U'), eta_Delta) $
    $ msVfy(hydraKeys, (cid || v - 1 || s' || (eta')^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
    where $eta_Delta$ is the accumulator commitment of the pending delta UTxOs.

+ Initializes the set of contesters
  $ contesters = emptyset $
  This allows the closing party to also contest and is required for use
  cases where pre-signed, valid in the future, close transactions are
  used to delegate head closing.

+ Correct contestation deadline is set
  $ tfinal = txValidityMax + T $
+ Transaction validity range is bounded by
  $ txValidityMax - txValidityMin <= T $
  to ensure the contestation deadline $tfinal$ is at most $2*T$ in the future.
+ Value in the head is preserved exactly
  $ valHead' = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
+ The ADA overhead $adaO$ is propagated unchanged from the open datum to the closed datum:
  $ adaO' = adaO $
  where $adaO$ is the ADA in the head UTxO not belonging to any L2 UTxO (minimum-UTxO overhead), set at initialisation time and unchanged for the head's lifetime. On fanout, the on-chain value conservation check treats $adaO$ as released from the head UTxO without requiring it in any distributed output, so it flows to whoever submits the fanout transaction as change (offsetting their transaction fee).

#dparagraph[Implementation note (accumulator construction).]
The per-case formulas for $eta'$ above ($accUTxO(U')$ for $sans("Any")$/$sans("Unused")$,
$accCombine(accUTxO(U'), eta_Delta)$ for $sans("Used")$) describe how the closing party constructs the
unified accumulator _off-chain_ before signing: $nuHead$ has neither $U'$ nor $eta_Delta$ and does not
recompute them. On-chain it verifies only the multisignature $xi$ over $cid || v || s' || (eta')^(\#)$
and the binding $(eta')^(\#) = hash(eta')$; the $sans("Initial")$ case additionally fixes $eta' =
accUTxO(emptyset) = G_1$ (a constant). The Agda `closeValid` bundle mirrors exactly this on-chain
view - `closeSigOK` (the multisignature, at $v$ or $v-1$ for $sans("Used")$), `closeηOK`
($(eta')^(\#) = hash(eta')$), and `closeInitialOK` ($eta = accUTxO(emptyset)$) - and likewise does
not recompute the off-chain $accUTxO$/$accCombine$ constructions, which are authenticated by $xi$.

#figure(closeTx-diagram, caption: [$mtxClose$ transaction spending the $stOpen$ head output and producing a $stClosed$ head output with unified accumulator $eta'$.]) <fig:closeTx>

== Contest Transaction <sec:contest-tx>

The $mtxContest$ transaction (see @fig:contestTx) is posted by a
party to prove the currently $stClosed$ state is not the latest one. This
transaction has
- one input spending from $nuHead$ holding the $st$ with $datumHead$,
- one output paying to $nuHead$ with value $valHead'$ and
  datum $datumHead'$.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("contest"), sans("ContestType"))$, where
$sans("ContestType")$ is a hint how to verify the snapshot and checks:
+ State is advanced from $datumHead tilde stClosed$ to
  $datumHead' tilde stClosed$, parameters $cid, hydraKeys, Tcontest$
  stay unchanged and the new state is governed again by $nuHead$
  #transition-arrow("contest")
  (the `contest` rule of `_⟶⟨_⟩_`; the version is preserved and the contester set
  grows by one key, $contesters' = contesters union { keyHash }$).
  The closed state carries a single unified accumulator $eta'$ computed using $accCombine$ based on the contest type.

+ Last known open state version stays recorded in closed state
  $ v' = v $

+ Contested snapshot number $s'$ is higher than the currently stored snapshot number $s$
  $ s' > s $
+ Based on the redeemer $sans("ContestType") = (sans("Unused"), xi, (eta')^(\#)) union (sans("Used"), xi, (eta')^(\#))$, where $xi$ is a multi-signature of the contesting snapshot and $(eta')^(\#) = hash(eta')$ is the hash of the unified accumulator commitment stored in the output datum, two cases are distinguished:

  + $sans("Unused")$: Contesting snapshot refers to current state version $v$ (pending delta not applied in snapshot). The unified accumulator reflects only the snapshotted UTxOs:
    $ eta' = accUTxO(U') $
    $ msVfy(hydraKeys, (cid || v || s' || (eta')^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
  + $sans("Used")$: Contesting snapshot refers to the previous state version $v - 1$ (pending delta applied in snapshot). The unified accumulator combines the snapshotted UTxOs with the pending delta:
    $ eta' = accCombine(accUTxO(U'), eta_Delta) $
    $ msVfy(hydraKeys, (cid || v - 1 || s' || (eta')^(\#)), xi) = mtrue $
    $ (eta')^(\#) = hash(eta') $
    where $eta_Delta$ is the accumulator commitment of the pending delta UTxOs.

+ The single signer ${keyHash} = txKeys$ has not already contested and is added to the set of contesters
  $ keyHash in.not contesters $
  $ contesters' = contesters union keyHash $
+ Transaction is posted before deadline
  $ txValidityMax <= tfinal $
+ Contestation deadline is updated correctly to
  $ tfinal' = cases(
    tfinal & upright("if") ~ |contesters'| = n",",
    tfinal + T & upright("otherwise.")
  ) $
+ Value in the head is preserved exactly
  $ valHead' = valHead $
+ Transaction is signed by a participant
  $ exists {cid |-> keyHash_i |-> 1} in valHead' => keyHash_i in txKeys $
+ No minting or burning
  $ txMint = emptyset $
+ The ADA overhead $adaO$ is preserved in the output closed datum:
  $ adaO' = adaO $

#figure(contestTx-diagram, caption: [$mtxContest$ transaction spending the $stClosed$ head output and producing a different $stClosed$ head output.]) <fig:contestTx>

== Fan-Out Transaction <sec:fanout-tx>

Once the contestation phase is over, a head may be finalized by posting a
$mtxFanout$ transaction (see @fig:fanoutTx), which
distributes all UTxOs from the head according to the unified accumulator in the closed state. A fanout transaction consists of
- one input spending from $nuHead$ holding the $st$, and
- outputs $o_1 dots.h o_m$ to distribute all UTxOs.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("fanout"), m, pi, sans("crsRef"))$, where:
- $m$ is the number of outputs to distribute from the $stClosed$ state,
- $pi$ is the KZG membership witness,
- $sans("crsRef")$ is the output reference of the reference input holding the Common Reference String (CRS).
The validator checks:
+ State is advanced from $datumHead tilde stClosed$ to terminal state $stFinal$:
  #transition-arrow("fanout")
  (the `fanout` rule of `_⟶⟨_⟩_`; a $stClosed$ datum steps to the terminal state).
+ All $m$ outputs are verified as members of the unified accumulator $eta$ using the membership witness $pi$:
  $ accVerify(eta, {o_1, dots.h, o_m}, pi) = mtrue $
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ All tokens are burnt
  $|{cid |-> dot.c |-> -1} in txMint| = n + 1$.
+ The head input value is fully conserved:
  $ val_(sans("head"))^(sans("in")) = plus.o.big_(i=1)^(m) val(o_i) plus.o val_(sans("burned")) plus.o adaO $
  where $adaO$ is the ADA overhead carried from the closed datum, and $val_(sans("burned"))$ is the value of all burned head tokens.

#figure(fanoutTx-diagram, caption: [$mtxFanout$ transaction spending the $stClosed$ head output with unified accumulator $eta$ and distributing funds with outputs $o_1 dots.h o_m$.]) <fig:fanoutTx>

=== Intermediate Partial Fan-Out Transaction <sec:partial-fanout-tx>

When UTxO sets exceed transaction size limits, the protocol distributes UTxOs across
multiple transactions using partial fanout. Each intermediate step distributes a subset of UTxOs and
transitions the head into the $stFanoutProgress$ state, which carries only the fields needed for
subsequent steps:
$ (stFanoutProgress, cid, hydraKeys, nop, tfinal, eta, adaO) $
where $nop$ is the number of participants, $tfinal$ is the contestation deadline (carried from $stClosed$), $eta$ is the
current accumulator commitment, and $adaO$ is the ADA overhead in the head UTxO not
belonging to any L2 UTxO (propagated from the closed datum).

An intermediate partial fanout transaction (see @fig:partialFanoutTx) consists of:
- one input spending from $nuHead$ in state $stClosed$ or $stFanoutProgress$, and
- a continuing head output at index 0 in state $stFanoutProgress$ with updated accumulator, and
- outputs $o_1 dots.h o_m$ at indices $1 dots.h m$ distributing a subset of UTxOs.

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("partialFanout"), m, sans("crsRef"))$, where $m$ is the number of UTxO outputs to distribute in this step and $sans("crsRef")$ is the output reference of the reference input holding the CRS.
The validator checks:
+ $m > 0$ (no zero-output batches).
+ State transitions into $stFanoutProgress$ with updated accumulator. For a $stClosed$ input:
  #transition-arrow("partialFanoutStart")
  (the `partialFanoutStart` rule of `_⟶⟨_⟩_`; $stClosed$ steps to $stFanoutProgress$).
  For a $stFanoutProgress$ input:
  #transition-arrow("partialFanoutStep")
  (the `partialFanoutStep` rule of `_⟶⟨_⟩_`; $stFanoutProgress$ steps to itself).
+ The new accumulator $eta'$ (from the output datum) is not the G1 generator -- all elements have _not_ yet been removed (use $stFinalPartialFanout$ for the last batch):
  $ eta' != G_1 $
+ No minting or burning $txMint = emptyset$.
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ Parameters $cid$, $hydraKeys$, $tfinal$, and $adaO$ are preserved in the output $stFanoutProgress$ datum.
+ Value is conserved: head input value equals head output value plus all distributed outputs:
  $ val_(sans("head"))^(sans("in")) = val_(sans("head"))^(sans("out")) plus.o plus.o.big_(i=1)^(m) val(o_i) $
+ The new accumulator $eta'$ from the output datum correctly represents the remaining UTxOs after removing $S = {o_1, dots.h, o_m}$. The output datum value serves as the exclusion witness:
  $ accVerifyExclude(eta, S, eta') = mtrue $

#figure(partialFanoutTx-diagram, caption: [$mtxPartialFanout$ transaction spending the $stFanoutProgress$ head output, distributing outputs $o_1 dots.h o_m$, and producing a new $stFanoutProgress$ head output with updated accumulator $eta'$.]) <fig:partialFanoutTx>

=== Final Partial Fan-Out Transaction <sec:final-partial-fanout-tx>

Once all UTxOs except the last batch have been distributed via $stPartialFanout$ steps,
the final step burns all head tokens and distributes the remaining UTxOs.
A final partial fanout transaction (see @fig:finalPartialFanoutTx) consists of:
- one input spending from $nuHead$ in state $stFanoutProgress$, and
- outputs $o_1 dots.h o_m$ distributing the remaining UTxOs (no continuing head output).

The state-machine validator $nuHead$ is spent with
$redeemerHead = (sans("finalPartialFanout"), m, pi, sans("crsRef"))$, where $m$ is the number of UTxO outputs, $pi$ is the KZG membership witness, and $sans("crsRef")$ is the CRS reference.
The validator checks:
+ $m > 0$ (prevents fund theft via a zero-output proof bypass).
+ State is advanced from $stFanoutProgress$ to terminal state $stFinal$:
  #transition-arrow("finalPartialFanout")
  (the `finalPartialFanout` rule of `_⟶⟨_⟩_`; $stFanoutProgress$ steps to the terminal state).
+ All head tokens are burnt
  $|{cid |-> dot.c |-> -1} in txMint| = n + 1$.
+ Transaction is posted after contestation deadline $txValidityMin > tfinal$.
+ The $m$ distributed outputs are verified as members of the accumulator $eta$ using the membership witness $pi$:
  $ accVerify(eta, {o_1, dots.h, o_m}, pi) = mtrue $
+ Value is conserved:
  $ val_(sans("head"))^(sans("in")) = plus.o.big_(i=1)^(m) val(o_i) plus.o val_(sans("burned")) plus.o adaO $

#figure(finalPartialFanoutTx-diagram, caption: [$mtxFinalPartialFanout$ transaction spending the $stFanoutProgress$ head output, distributing the final batch of UTxOs $o_1 dots.h o_m$, and burning all head tokens to reach $stFinal$.]) <fig:finalPartialFanoutTx>

The $muHead(seed)$ minting policy governs the burning of tokens via
redeemer $sans("burn")$ that:
+ All tokens in $txMint$ need to be of negative quantity
  $forall {cid |-> dot.c |-> q} in txMint : q < 0$.
