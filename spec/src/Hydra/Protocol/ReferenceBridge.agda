-- Bridge: the extractable decidable checker `Reference.closeRefᵇ` faithfully reflects the
-- (unit-robust) DECIDABLE conjuncts of the abstract `closeValid` (OnChain.lagda.typ). Typecheck-
-- only - this module is NOT extracted (it imports OnChain / set-theory). Imported by Main so the
-- spec build (`spec/build.sh`) verifies the correspondence.
--
-- Direction proved (completeness): `closeValid ⇒ closeRefᵇ ≡ true`. The reference therefore
-- accepts every spec-valid close (a reference REJECT implies the spec rejects), making the reference
-- a faithful proxy for the spec. The hydra-tx `HeadValidatorAgreement` test then checks
-- `reference === validator` on the SAME inputs, so spec-valid ⇒ reference-accepts ⇒ validator-accepts.
--
-- The deadline (`tfinal = hi + cp`) and bounded-validity (`hi - lo <= cp`) conjuncts ARE part of
-- `closeRefᵇ` and are discharged from the bundle's `deadlineOK`/`validityBounded` fields; the
-- crypto/accumulator conjuncts stay in the injected (mock) `Ops`.
--
-- One conjunct is NOT a closed proof: `participantSigned→ref` rests on a POSTULATED extraction-
-- faithfulness boundary (`signerCodes`/`ptCodes` encode the tx's signer key-hashes / head PT names, and
-- a spec-valid tx is ASSUMED to make those lists overlap). It is honest about this in-line; it is the
-- one bridge clause whose correspondence is assumed rather than derived (same trust family as the
-- `cidToNat`/`refCodeOf` encoding postulates). Every other `*Valid → ref` clause is a genuine proof.
module Hydra.Protocol.ReferenceBridge where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Hydra.Protocol.OnChain
import Hydra.Protocol.Reference as R
-- The Bool-check ⇄ proposition reflection lemmas (==ᵇ-refl, ≡→==ᵇ, ≤→≤ᵇ, <→<ᵇ, &&-intro) and the
-- builtin-soundness lemmas (==-sound, <ᴮ-sound, both PROVED by induction, not postulated) live in
-- `RefReflection`; this module is just the `*Valid → ref` correspondence.
open import Hydra.Protocol.RefReflection
open import Relation.Binary.PropositionalEquality using (trans; sym; cong)
open import Relation.Nullary using (yes; no)
open import Data.List using (map; take; drop)
open import Data.Product using (_,_; _×_; ∃-syntax)
open import Data.List.Relation.Unary.Any using (here; there)
open import Agda.Builtin.Bool using (true; false)
open import Agda.Builtin.Nat using (_==_)

-- abstraction map: abstract close-redeemer tag → concrete Reference tag.
-- (The Haskell `HeadValidatorAgreement` test passes the matching `R.CloseTagᶜ` constructor directly.)
closeTagOf : CloseType → R.CloseTagᶜ
closeTagOf closeInitial     = R.closeInitialᶜ
closeTagOf (closeAny _ _ _ _)    = R.closeAnyᶜ
closeTagOf (closeUnused _ _ _ _) = R.closeUnusedᶜ
closeTagOf (closeUsed _ _ _ _)   = R.closeUsedᶜ

-- The injected boundary, mocked to `true` (the differential test supplies the same).
mockOps : R.Ops
mockOps = record { closeCryptoOK = λ _ _ _ → true }

-- ── the bridge ──────────────────────────────────────────────────────────────────────────────
-- For any spec-valid close (the produced Closed datum shares the preserved parameters, as the
-- `close` rule guarantees), the reference checker accepts.
-- `validityHi := ValidityInterval.hi (Context.validity ctx)` and the Closed datum's deadline `tfin`
-- feed the reference's deadline conjunct `tfinalC ≡ validityHi + cp`, discharged from the bundle's
-- `closeDeadlineOK` (the 2nd conjunct `dl`) via `==-sound`. The conjunct holds in all four close cases.
closeValid→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin ct
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct
  → R.closeRefᵇ mockOps (R.mkOpenᶜ v cp) (R.mkClosedᶜ v cp s′ (length C) tfin) (closeTagOf ct)
       (ValidityInterval.hi (Context.validity ctx)) (ValidityInterval.lo (Context.validity ctx)) ≡ true
-- Record-pattern destructuring: `step = close` matches the close rule's constructor, which refines
-- the produced contesters `C` to `[]` (the rule's output) - so the reference's `length C ==ᵇ zero`
-- discharges by `refl`. The named fields (`deadlineOK`, `initialOK`, `anyOK`) give field access.
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial
  record { step = close ; deadlineOK = dl ; initialOK = (v≡0 , s≡0 , _) ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro (&&-intro (≡→==ᵇ v≡0) (≡→==ᵇ s≡0))
   (&&-intro refl
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeAny ξ η# δ# κ#)
  record { step = close ; deadlineOK = dl ; anyOK = anyOK ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro (<→<ᵇ anyOK)
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUnused ξ η# δ# κ#)
  record { step = close ; deadlineOK = dl ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUsed ξ η# δ# κ#)
  record { step = close ; deadlineOK = dl ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))

-- ── increment / decrement ───────────────────────────────────────────────────────────────────
-- The produced Open datum carries `suc v`; the reference's `versionOut ==ᵇ suc versionIn` holds.
mockOpsInc : R.OpsInc
mockOpsInc = record { incCryptoOK = λ _ → true }

-- Increment: version bumps AND the head value grows by ALL deposits. The reference's lovelace check
-- `adaIn + adaDelta ≡ adaOut` follows from `incrementValueOK` (headValueIn +ᵛ depositsValue ≡ headValue)
-- via the `adaOf` additivity law - so a reference value-reject implies the spec rejects. `adaDelta` is
-- the lovelace of ALL spent deposits (`depositsValue`, Plutus `totalNonHeadInputValue`), which is what
-- lets the differential catch the multi-deposit siphon.
-- BOTH projections (`adaOf` and `nonAdaOf`) are checked: each is an additive homomorphism, so each
-- conjunct follows from the SAME value equation `headValueIn +ᵛ depositsValue ≡ headValue` by the
-- respective additivity law. Checking the non-ada total catches a native-token siphon the lovelace
-- check alone misses.
incrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref δ#
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref δ#
  → R.incRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (depositsValue ctx)) (adaOf (headValue ctx))
          (nonAdaOf (headValueIn ctx)) (nonAdaOf (depositsValue ctx)) (nonAdaOf (headValue ctx))
          (OutputRef.index ref) 0)
     ≡ true
incrementValid→ref ctx cid hk n cp v η ada η′ ξ s ref δ# b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (≡→==ᵇ (IncrementValid.depositFirstOutput b))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValueIn ctx) (depositsValue ctx)))
                            (cong adaOf (IncrementValid.valueOK b))))
 (&&-intro (==-sound (trans (sym (nonAdaOf-+ᵛ (headValueIn ctx) (depositsValue ctx)))
                            (cong nonAdaOf (IncrementValid.valueOK b))))
           refl)))

-- PER-ASSET refinement of the increment value check: for EVERY native asset `k` (the caller supplies the
-- list), `quantityOfᴺ headValueIn k + quantityOfᴺ depositsValue k ≡ quantityOfᴺ headValue k`, each derived
-- from the SAME `incrementValueOK` value equation by the per-asset additivity law `quantityOfᴺ-+ᵛ` (exactly
-- as the ada/non-ada totals are, but per asset). So a reference per-asset reject ⇒ the spec rejects ⇒ the
-- validator rejects. This catches a selective single-token siphon the two scalar totals miss. Inducts on
-- the asset list; nil is `perAssetConservedᵇ [] = true`.
incPerAsset→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref δ# (assets : List (CId × Token))
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref δ#
  → R.perAssetConservedᵇ
       (map (λ k → R.mkAssetIOᶜ (quantityOfᴺ (headValueIn ctx) k)
                                (quantityOfᴺ (depositsValue ctx) k)
                                (quantityOfᴺ (headValue ctx) k)) assets)
     ≡ true
incPerAsset→ref ctx cid hk n cp v η ada η′ ξ s ref δ# []       b = refl
incPerAsset→ref ctx cid hk n cp v η ada η′ ξ s ref δ# (k ∷ ks) b =
  &&-intro (==-sound (trans (sym (quantityOfᴺ-+ᵛ (headValueIn ctx) (depositsValue ctx) k))
                            (cong (λ w → quantityOfᴺ w k) (IncrementValid.valueOK b))))
           (incPerAsset→ref ctx cid hk n cp v η ada η′ ξ s ref δ# ks b)

-- Decrement: version bumps AND the head value shrinks by the decommit. The reference's lovelace check
-- `adaOut + adaDelta ≡ adaIn` follows from `decrementValueOK` (headValue +ᵛ decommitValue ≡ headValueIn)
-- via the `adaOf` additivity law -- so a reference value-reject implies the spec rejects. The ada
-- fields carry: adaIn = head input, adaDelta = decommit value, adaOut = head output (the larger side
-- is the head INPUT, unlike increment).
decrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m κ#
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m κ#
  → R.decRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (decommitValue ctx m)) (adaOf (headValue ctx))
          (nonAdaOf (headValueIn ctx)) (nonAdaOf (decommitValue ctx m)) (nonAdaOf (headValue ctx))
          0 (length (take m (drop 1 (Context.outputs ctx)))))
     ≡ true
decrementValid→ref ctx cid hk n cp v η ada η′ ξ s m κ# b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (<→<ᵇ (DecrementValid.decommitNonEmpty b))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValue ctx) (decommitValue ctx m)))
                            (cong adaOf (DecrementValid.valueOK b))))
 (&&-intro (==-sound (trans (sym (nonAdaOf-+ᵛ (headValue ctx) (decommitValue ctx m)))
                            (cong nonAdaOf (DecrementValid.valueOK b))))
           refl)))

-- PER-ASSET refinement of the DECREMENT value check (mirror of `incPerAsset→ref`): for EVERY native asset
-- `k`, `quantityOfᴺ headValue k + quantityOfᴺ decommitValue k ≡ quantityOfᴺ headValueIn k`, each derived from
-- the SAME `decrementValueOK` equation (`headValue +ᵛ decommitValue ≡ headValueIn`) by the per-asset
-- additivity law `quantityOfᴺ-+ᵛ`. The caller supplies each AssetIOᶜ as (qIn := headValue, qDelta :=
-- decommitValue, qOut := headValueIn), so the shared `perAssetConservedᵇ` sum-check `qIn + qDelta == qOut`
-- IS this equation. Catches a selective single-token over-decommit the two scalar totals miss.
decPerAsset→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m κ# (assets : List (CId × Token))
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m κ#
  → R.perAssetConservedᵇ
       (map (λ k → R.mkAssetIOᶜ (quantityOfᴺ (headValue ctx) k)
                                (quantityOfᴺ (decommitValue ctx m) k)
                                (quantityOfᴺ (headValueIn ctx) k)) assets)
     ≡ true
decPerAsset→ref ctx cid hk n cp v η ada η′ ξ s m κ# []       b = refl
decPerAsset→ref ctx cid hk n cp v η ada η′ ξ s m κ# (k ∷ ks) b =
  &&-intro (==-sound (trans (sym (quantityOfᴺ-+ᵛ (headValue ctx) (decommitValue ctx m) k))
                            (cong (λ w → quantityOfᴺ w k) (DecrementValid.valueOK b))))
           (decPerAsset→ref ctx cid hk n cp v η ada η′ ξ s m κ# ks b)

-- ── contest ─────────────────────────────────────────────────────────────────────────────────
-- Version preserved (both v), snapshot strictly increases (s < s′ from the bundle), one contester
-- appended (output contesters ≡ kh ∷ C, so length ≡ suc (length C)), and posted before the
-- contestation deadline (`validityHi ≤ tfin` from `beforeDeadline`, via `≤ᴮ-sound`). The conditional
-- deadline-UPDATE rule stays in the injected (mock) `contestCryptoOK`.
mockOpsContest : R.OpsContest
mockOpsContest = record { contestCryptoOK = λ _ → true }

-- The deadline-UPDATE conjunct `tfinalOut == if (lenOut ==ᵇ n) then tfin else tfin+cp` is discharged
-- from `contestDeadlineOK` (the bundle's `deadlineOK`, stated with the stdlib `if ⌊ len ≟ n ⌋`): case on
-- the decidable `length (kh ∷ C) ≟ n` and reflect its `⌊_⌋` into the structural `_==ᵇ_` via `≡→==ᵇ`
-- (yes) / `¬→==ᵇfalse` (no), so the two conditionals pick the same branch.
contestValid→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.contestRefᵇ mockOpsContest
       (R.mkContestIOᶜ v v s s′ (length C) (length (kh ∷ C))
          tfin (ValidityInterval.hi (Context.validity ctx)) tfin′ n cp) ≡ true
contestValid→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct b =
    &&-intro (==ᵇ-refl v)
   (&&-intro (<→<ᵇ (ContestValid.snapIncreases b))
   (&&-intro (==ᵇ-refl (suc (length C)))
   (&&-intro (≤ᴮ-sound (ContestValid.beforeDeadline b))
   (&&-intro (==-sound deadlineEq) refl))))
  where
    deadlineEq : tfin′ ≡ (R.if (length (kh ∷ C) R.==ᵇ n) then tfin else (tfin + cp))
    deadlineEq with length (kh ∷ C) ≟ n | ContestValid.deadlineOK b
    ... | yes p  | dl = trans dl (sym (cong (λ z → R.if z then tfin else (tfin + cp)) (≡→==ᵇ p)))
    ... | no  ¬p | dl = trans dl (sym (cong (λ z → R.if z then tfin else (tfin + cp)) (¬→==ᵇfalse ¬p)))

-- ── value preservation (close / contest mustPreserveHeadValue) ──────────────────────────────────
-- The `valuePreserved` field (headValueIn ctx ≡ headValue ctx, the validator's EXACT
-- `mustPreserveHeadValue`) reflects to `valuePreservedᵇ ≡ true` on BOTH the ada total and the non-ada
-- total: each half is `cong adaOf`/`cong nonAdaOf` applied to the single value equality, then `==-sound`.
-- No additivity law, no injectivity, no new postulate. ct-independent: `closeValid`/`contestValid` reduce
-- to their records on the datum shapes alone. Moves close/contest `mustPreserveHeadValue` out of the
-- injected (mock) `Ops` into the bridged + (differentially) tested layer.
closeValuePreserved→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin ct
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct
  → R.valuePreservedᵇ (adaOf (headValueIn ctx)) (adaOf (headValue ctx))
                       (nonAdaOf (headValueIn ctx)) (nonAdaOf (headValue ctx)) ≡ true
closeValuePreserved→ref ctx cid hk n cp v η ada s′ η′ C tfin ct b =
  &&-intro (==-sound (cong adaOf (CloseValid.valuePreserved b)))
           (==-sound (cong nonAdaOf (CloseValid.valuePreserved b)))

contestValuePreserved→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.valuePreservedᵇ (adaOf (headValueIn ctx)) (adaOf (headValue ctx))
                       (nonAdaOf (headValueIn ctx)) (nonAdaOf (headValue ctx)) ≡ true
contestValuePreserved→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct b =
  &&-intro (==-sound (cong adaOf (ContestValid.valuePreserved b)))
           (==-sound (cong nonAdaOf (ContestValid.valuePreserved b)))

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
-- `burnedCount == n+1` from `burnAllTokensOK` (via `==-sound`); `tfinal < lo` (posted after the
-- deadline) from `afterDeadline` (via `<ᴮ-sound`). The accumulator-membership and value-conservation
-- conjuncts stay in the injected (mock) `fanoutCryptoOK`. No `0 < m` conjunct: the FULL fanout permits
-- m = 0 (empty-head finalisation), so `fanoutRefᵇ` does not gate it (`FanoutValid` carries no
-- `outputsPositive`); `FinalPartialFanoutValid.outputsPositive` is in the spec record but not bridged.
mockOpsFanout : R.OpsFanout
mockOpsFanout = record { fanoutCryptoOK = λ _ → true }

fanoutValid→ref : ∀ ctx cid hk n cp v s η C tfin ada m π crs
  → fanoutValid ctx (Closed cid hk n cp v s η C tfin ada) m π crs
  → R.fanoutRefᵇ mockOpsFanout
       (R.mkFanoutᶜ m (burnedCount ctx cid) n tfin (ValidityInterval.lo (Context.validity ctx))) ≡ true
fanoutValid→ref ctx cid hk n cp v s η C tfin ada m π crs b =
    &&-intro (==-sound (FanoutValid.burnAllTokens b))
   (&&-intro (<ᴮ-sound (FanoutValid.afterDeadline b)) refl)

finalPartialFanoutValid→ref : ∀ ctx cid hk n tfin η ada m π crs
  → finalPartialFanoutValid ctx (FanoutProgress cid hk n tfin η ada) m π crs
  → R.fanoutRefᵇ mockOpsFanout
       (R.mkFanoutᶜ m (burnedCount ctx cid) n tfin (ValidityInterval.lo (Context.validity ctx))) ≡ true
finalPartialFanoutValid→ref ctx cid hk n tfin η ada m π crs b =
    &&-intro (==-sound (FinalPartialFanoutValid.burnAllTokens b))
   (&&-intro (<ᴮ-sound (FinalPartialFanoutValid.afterDeadline b)) refl)

-- ── non-final partial fanout (FanoutProgress → FanoutProgress) ─────────────────────────────────
-- The reference's `0 <ᵇ m` holds from `outputsPositive` (the §5.8 no-zero-output-batch guard, via the
-- structural `<→<ᵇ`); `tfinal < lo` (posted after the deadline) from `afterDeadline` (via `<ᴮ-sound`).
-- UNLIKE the full/final fanout, the partial path forbids m = 0, so `0 < m` IS bridged here. The
-- accumulator-exclusion/non-empty-progress (`excludeOK`/`notDoneOK`) and value-conservation conjuncts stay
-- abstract; `mintEmpty` is the shared `noMintRefᵇ`. So a reference reject ⇒ the spec rejects ⇒ the
-- validator rejects (`PartialFanoutZeroOutputs` / `LowerBoundBeforeContestationDeadline`).
partialFanoutValid→ref : ∀ ctx d d′ m crs
  → partialFanoutValid ctx d d′ m crs
  → R.partialFanoutRefᵇ m (tfinalOf d) (ValidityInterval.lo (Context.validity ctx)) ≡ true
partialFanoutValid→ref ctx d d′ m crs b =
    &&-intro (<→<ᵇ (PartialFanoutValid.outputsPositive b))
             (<ᴮ-sound (PartialFanoutValid.afterDeadline b))

-- ── deposit recover (νDeposit) ────────────────────────────────────────────────────────────────
-- The reference's after-deadline check `tRecover <ᴮ validityLo` holds from the bundle's
-- `tRec < ValidityInterval.lo (validity ctx)` (§5.3.2 `txValidityMin > t_recover`), discharged via
-- `<ᴮ-sound`. The recovered-outputs hash equality (`recoveredMatchesDeposited`) is the injected (mock)
-- `recoverHashOK`, matching the differential. So a reference deadline-reject ⇒ the spec rejects ⇒ (by
-- the deposit.ak Recover arm) the validator rejects (`DepositPeriodNotReached`).
mockOpsRecover : R.OpsRecover
mockOpsRecover = record { recoverHashOK = λ _ → true }

recoverValid→ref : ∀ ctx cid tRec C m
  → recoverValid ctx (mkDepositDatum cid tRec C) m
  → R.recoverRefᵇ mockOpsRecover
       (R.mkRecoverIOᶜ tRec (ValidityInterval.lo (Context.validity ctx)) (depositInputCount ctx)) ≡ true
recoverValid→ref ctx cid tRec C m b =
  &&-intro (<ᴮ-sound (RecoverValid.afterRecoverDeadline b))
 (&&-intro (≡→==ᵇ (RecoverValid.singleDeposit b)) refl)

-- ── init (μHead minting policy: token count + placement) ─────────────────────────────────────────
-- The reference's count check `mintedCount == suc n` holds from `mintedCountOK` (the μHead
-- `checkNumberOfTokens`), and the PLACEMENT checks `stQty == 1` / `headTokenCount == suc n` hold from
-- `stPlaced` / `tokensPlaced` - all `≡`-of-ℕ-projection conjuncts, discharged directly via `==-sound`
-- (no new axiom; the reference IO fields ARE those projections, as for `mintedCount`). The remaining
-- μHead conjuncts (seed-spent, datum binding) are the injected (mock) `initPlacementOK`. So a reference
-- reject ⇒ the spec rejects ⇒ the μHead policy rejects (`WrongNumberOfTokensMinted` / a misplaced-token
-- failure).
mockOpsInit : R.OpsInit
mockOpsInit = record { initPlacementOK = λ _ → true }

initValid→ref : ∀ ctx seed cid hk n cp v η ada
  → initValid ctx seed (Open cid hk n cp v η ada)
  → R.initRefᵇ mockOpsInit
       (R.mkMintIOᶜ n (mintedCount ctx cid) (stQty (headValue ctx) cid) (headTokenCount (headValue ctx) cid))
     ≡ true
initValid→ref ctx seed cid hk n cp v η ada b =
  &&-intro (==-sound (InitValid.mintedCountOK b))
 (&&-intro (==-sound (InitValid.stPlaced b))
 (&&-intro (==-sound (InitValid.tokensPlaced b)) refl))

-- ── deposit claim (νDeposit, Claim redeemer) ──────────────────────────────────────────────────
-- The reference's before-deadline check `validityHi ≤ᴮ tRecover` holds from the bundle's
-- `ValidityInterval.hi (validity ctx) ≤ DepositDatum.tRecover dd` (§5.2 deposit.ak `before_deadline`:
-- txValidityMax ≤ t_recover), discharged via `≤ᴮ-sound`. The own-head binding (`claimedByOwnHead`,
-- deposit.ak `expect_increment_redeemer`) is also checked: the deposit datum's head id `cid` equals
-- the spent head's id `hcid`. Head ids are hashes, which the hash-free reference layer cannot hold, so
-- the boundary represents each as the Integer `cidToNat ·` and the reference compares those. The bridge
-- only needs `cid ≡ hcid ⇒ cidToNat cid ≡ cidToNat hcid` - plain `cong` on the encoding, NO injectivity
-- assumption (that would only be needed for the converse, which the bridge direction `bundle ⇒ reference`
-- does not require). `cidToNat` is a typecheck-only encoding postulate (it is not part of the extracted
-- reference; the `HeadValidatorAgreement` test supplies a concrete deterministic encoding, `cidToInteger`,
-- injective on the distinct head ids it compares, on the Haskell side).
-- The Increment-redeemer coupling comes from the JOINT bundle below. So a reference reject ⇒ the
-- spec rejects ⇒ (by the deposit.ak Claim arm) the validator rejects (`DepositPeriodSurpassed` /
-- `expect_increment_redeemer`).
postulate cidToNat : ℍ → ℕ

-- The deterministic out-ref → ℕ encoding the differential mirrors (same trust family as `cidToNat`;
-- no injectivity needed). Declared here so the claim bridge below can name it; the referenced-output
-- membership lemma (`refSpent→ref`, further down) is its other consumer.
postulate refCodeOf : OutputRef → ℕ

-- The abstract redeemer's pinned on-chain constructor index (the `makeIsDataIndexed ''Input` list in
-- HeadState.hs, which deposit.ak's `is_head_increment` reads raw; the alignment fact is that the Agda
-- and Plutus constructor orders coincide). A definition, not a postulate: the coupling bridge below
-- discharges `redeemerIndex (Increment …) == 0` by computation.
redeemerIndex : HeadRedeemer → ℕ
redeemerIndex (Increment _ _ _ _)        = 0
redeemerIndex (Decrement _ _ _ _)        = 1
redeemerIndex (Close _)                  = 2
redeemerIndex (Contest _)                = 3
redeemerIndex (Fanout _ _ _)             = 4
redeemerIndex (PartialFanout _ _)        = 5
redeemerIndex (FinalPartialFanout _ _ _) = 6

-- Joint claim bundle ⇒ the extracted claim checker. The before-deadline and own-head conjuncts come
-- from the νDeposit side (`ClaimTxValid.depositSideOK`); the Increment-redeemer coupling comes from
-- the νHEAD side - the joint bundle's head input is spent with `Increment ξ s ref δ#` BY TYPE
-- (`ClaimTxValid.headSideOK`'s step), so its pinned index is 0 definitionally and the reference's
-- `headRedeemerIdxC == 0` conjunct is discharged by `refl`. The claims-this-deposit coupling
-- (deposit.ak's ref equality, `claimedRefCodeC == ownRefCodeC`) is likewise definitional here: the
-- joint bundle carries one `ref` shared by both sides, so both codes are `refCodeOf ref` and the
-- conjunct is `==-sound refl`. No mock and no new postulate: the only assumed pieces remain the
-- `cidToNat`/`refCodeOf` encodings (used with `cong`/`refl`, no injectivity).
claimTxValid→ref : ∀ ctx cid tRec C hcid hk n cp v η ada headOut ξ s ref δ#
  → claimTxValid ctx (mkDepositDatum cid tRec C) (Open hcid hk n cp v η ada) headOut ξ s ref δ#
  → R.claimRefᵇ
      (R.mkClaimIOᶜ tRec (ValidityInterval.hi (Context.validity ctx)) (cidToNat cid) (cidToNat hcid)
         (redeemerIndex (Increment ξ s ref δ#)) (refCodeOf ref) (refCodeOf ref)) ≡ true
claimTxValid→ref ctx cid tRec C hcid hk n cp v η ada headOut ξ s ref δ# b =
  &&-intro (≤ᴮ-sound (ClaimValid.beforeRecoverDeadline (ClaimTxValid.depositSideOK b)))
 (&&-intro (==-sound (cong cidToNat (ClaimValid.claimedByOwnHead (ClaimTxValid.depositSideOK b))))
 -- the redeemer-index conjunct computes away (`redeemerIndex (Increment …) == 0` is `true`), leaving
 -- exactly the ref-coupling conjunct: both codes are `refCodeOf ref`.
 (==-refl (refCodeOf ref)))

-- ── contest parameter preservation (scalar half of mustNotChangeParameters: headId + cp) ─────────
-- The contest transition's produced `Closed` reuses the SAME `cid`/`cp` binders, so head-id and
-- contestation-period preservation are definitional: `contestParamsᵇ (cidToNat cid) (cidToNat cid) cp cp`
-- is `refl`-discharged through `==-sound` (no injectivity, only the existing `cidToNat` encoding). The
-- validity premise is unused (the datum shape alone gives preservation). The `parties`-list half stays a
-- documented boundary (the spec abstracts the party list into a count `n`). Placed after `cidToNat`.
contestParams→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.contestParamsᵇ (cidToNat cid) (cidToNat cid) cp cp ≡ true
contestParams→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct _ =
  &&-intro (==-sound {cidToNat cid} {cidToNat cid} refl) (==-sound {cp} {cp} refl)

-- ── init datum head-id binding (decidable half of μHead checkDatum: headId == currency) ───────────
-- In the spec the produced `Open` datum's head id and the policy currency are the SAME `cid`
-- (= hash(μHead seed)), so `initHeadIdᵇ (cidToNat cid) (cidToNat cid)` is `refl`-discharged via `==-sound`
-- and the existing `cidToNat` encoding (no injectivity, no new postulate). The `seed == seedInput` half is
-- unmodelled (the Agda `Open` has no `headSeed`) and `cid = hash(seed)` stays the injected hash boundary.
initHeadId→ref : ∀ ctx seed cid hk n cp v η ada
  → initValid ctx seed (Open cid hk n cp v η ada)
  → R.initHeadIdᵇ (cidToNat cid) (cidToNat cid) ≡ true
initHeadId→ref ctx seed cid hk n cp v η ada _ = ==-sound {cidToNat cid} {cidToNat cid} refl

-- ── participant signature (shared: close / contest / increment / decrement) ──────────────────────
-- The reference's overlap check `anySharedᵇ signerCodes ptCodes` reflects `signedByParticipant`
-- (the §5.4–5.7 `mustBeSignedByParticipant`). The abstract predicate is an existential over the opaque
-- signer set (`signerKeyHash`) and the opaque value (`quantityOf`); NEITHER has a computational link to
-- the extracted Integer code lists, so - unlike the `cong cidToNat` bridge - the correspondence is a
-- POSTULATED extraction-faithfulness boundary (typecheck-only, same trust family as `cidToNat`/
-- `refCodeOf`): `signerCodes`/`ptCodes` are the deterministic hash→Integer encodings the differential
-- supplies for real (the tx signers' key-hashes and the head value's PT names), and a spec-valid tx is
-- asserted to make those two lists overlap. So a reference overlap-reject ⇒ the spec rejects ⇒ the
-- validator rejects (`SignerIsNotAParticipant`). The encoding aligns by construction: a PT's token name
-- IS the participant's key-hash, so the same byte string is encoded on both sides.
postulate
  signerCodes : Context → List ℕ
  ptCodes     : ℍ → Context → List ℕ
  participantSigned→ref : ∀ cid ctx → signedByParticipant cid ctx
    → R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true

-- Per-family wiring: each of the four bundles discharges the shared checker through its own
-- participant evidence - close/increment/decrement project their `participantSigned` field, and
-- contest derives the witness from its sharper contester fields (`contest-participantSigned`).
closeParticipant→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin ct
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct
  → R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true
closeParticipant→ref ctx cid hk n cp v η ada s′ η′ C tfin ct b =
  participantSigned→ref cid ctx (CloseValid.participantSigned b)

incrementParticipant→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref δ#
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref δ#
  → R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true
incrementParticipant→ref ctx cid hk n cp v η ada η′ ξ s ref δ# b =
  participantSigned→ref cid ctx (IncrementValid.participantSigned b)

decrementParticipant→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m κ#
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m κ#
  → R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true
decrementParticipant→ref ctx cid hk n cp v η ada η′ ξ s m κ# b =
  participantSigned→ref cid ctx (DecrementValid.participantSigned b)

contestParticipant→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true
contestParticipant→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct b =
  participantSigned→ref cid ctx (contest-participantSigned b)

-- ── no mint / no burn (shared: close / contest / increment / decrement) ────────────────────────
-- The reference's `noMintRefᵇ (mintEntryCount ctx)` reflects `noMint ctx` (the §5.4–5.7 `mustNotMintOrBurn`).
-- `noMint ctx = Context.mint ctx ≡ εᵛ` is over the opaque `Value`; its NON-ZERO entry count has no
-- computational link to the abstract `≡ εᵛ`, so - exactly as `participantSigned→ref` - the correspondence
-- is a POSTULATED extraction-faithfulness boundary: `mintEntryCount` is the count the differential supplies
-- for real (the length of the flattened tx mint value), and a tx with empty mint is asserted to make it 0.
-- So a reference reject (count ≠ 0) ⇒ the spec rejects ⇒ the validator rejects (`MintingOrBurningIsForbidden`).
postulate
  mintEntryCount : Context → ℕ
  noMint→ref : ∀ ctx → noMint ctx → R.noMintRefᵇ (mintEntryCount ctx) ≡ true

-- ── referenced output is spent (increment claimed deposit / init seed) ─────────────────────────
-- The reference's `refSpentᵇ (refCodeOf ref) (inputRefCodes ctx)` reflects `depositSpentOK ctx ref` (the
-- increment `claimedDepositIsSpent` / the μHead `seedInputIsConsumed`). Unlike `participantSigned→ref`,
-- this is a REAL proof (no faithfulness postulate): `inputRefCodes` is CONCRETE (the out-ref of each spent
-- input, encoded), and the membership is DERIVED from the `depositSpentOK` ∃-witness by induction. Only
-- `refCodeOf` stays a postulate - the deterministic out-ref→ℕ encoding the differential mirrors (same trust
-- family as `cidToNat`; no injectivity needed). So a reference reject ⇒ the spec rejects ⇒ the validator
-- rejects (`DepositNotSpent` / `SeedNotSpent`). One lemma serves increment (ref = the claimed deposit) and
-- init (ref = the seed). (`refCodeOf` itself is declared next to `cidToNat` above.)

-- CONCRETE: the encoded out-ref of every spent input (the differential supplies the same map).
inputRefCodes : Context → List ℕ
inputRefCodes ctx = map (λ i → refCodeOf (Input.outputRef i)) (Context.inputs ctx)

-- `b || true ≡ true` for the extracted `_||_` (used in the `there`/short-circuit step of `elemᵇ-mapped`).
||ᵇ-trueʳ : ∀ b → (b R.|| true) ≡ true
||ᵇ-trueʳ true  = refl
||ᵇ-trueʳ false = refl

-- membership ⇒ the extracted overlap check fires: if `c` encodes some `i ∈ xs` (c ≡ g i), then
-- `elemᵇ c (map g xs) ≡ true`. Induction over the `Any (_≡_)` witness; the head case discharges via
-- `==-sound` (c ≡ g y so `c == g y ≡ true`), the tail case via the IH and `||ᵇ-trueʳ`.
elemᵇ-mapped : ∀ {A : Set} (g : A → ℕ) (c : ℕ) {i : A} (xs : List A)
  → i ∈ˡ xs → c ≡ g i → R.elemᵇ c (map g xs) ≡ true
elemᵇ-mapped g c (y ∷ ys) (here  i≡y) c≡gi =
  cong (R._|| R.elemᵇ c (map g ys)) (==-sound (trans c≡gi (cong g i≡y)))
elemᵇ-mapped g c (y ∷ ys) (there p)   c≡gi =
  trans (cong ((c == g y) R.||_) (elemᵇ-mapped g c ys p c≡gi)) (||ᵇ-trueʳ (c == g y))

refSpent→ref : ∀ ctx ref → depositSpentOK ctx ref
  → R.refSpentᵇ (refCodeOf ref) (inputRefCodes ctx) ≡ true
refSpent→ref ctx ref (i , i∈inputs , outRef≡ref) =
  elemᵇ-mapped (λ j → refCodeOf (Input.outputRef j)) (refCodeOf ref) (Context.inputs ctx)
    i∈inputs (sym (cong refCodeOf outRef≡ref))

-- init's `seedSpent` (the μHead `seedInputIsConsumed`) reuses the real `refSpent→ref` for the seed
-- out-ref, consuming `InitValid.seedSpent`.
initSeedSpent→ref : ∀ ctx seed cid hk n cp v η ada
  → initValid ctx seed (Open cid hk n cp v η ada)
  → R.refSpentᵇ (refCodeOf seed) (inputRefCodes ctx) ≡ true
initSeedSpent→ref ctx seed cid hk n cp v η ada b = refSpent→ref ctx seed (InitValid.seedSpent b)

-- ── composition: the per-conjunct bridges JOIN into one spec ⇒ extracted-reference verdict ──
-- Demonstrated for the close flagship (closeInitial): a SINGLE `closeValid` discharges the core close
-- reference checker, the value-preservation checker, the no-mint checker AND the shared
-- participant-signature checker at once. The other validators compose identically
-- (each `*Valid` discharges its core `*Refᵇ` plus the pulled-out conjuncts via the lemmas above). This is
-- the `spec-bundle ⇒ extracted-reference` half; it is joined to the `extracted-reference === real-validator`
-- differential (Hydra.Tx.Contract.HeadValidatorAgreement) at the SHARED extracted Reference module — the
-- single ANDed end-to-end property there checks that join across every validator family.
closeChainInitial→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin
  → (b : closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) closeInitial)
  → (R.closeRefᵇ mockOps (R.mkOpenᶜ v cp) (R.mkClosedᶜ v cp s′ (length C) tfin) (closeTagOf closeInitial)
        (ValidityInterval.hi (Context.validity ctx)) (ValidityInterval.lo (Context.validity ctx)) ≡ true)
    × (R.valuePreservedᵇ (adaOf (headValueIn ctx)) (adaOf (headValue ctx))
                         (nonAdaOf (headValueIn ctx)) (nonAdaOf (headValue ctx)) ≡ true)
    × (R.noMintRefᵇ (mintEntryCount ctx) ≡ true)
    × (R.participantSignedRefᵇ (R.mkSignerIOᶜ (signerCodes ctx) (ptCodes cid ctx)) ≡ true)
closeChainInitial→ref ctx cid hk n cp v η ada s′ η′ C tfin b =
    closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial b
  , closeValuePreserved→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial b
  , noMint→ref ctx (CloseValid.mintEmpty b)
  , participantSigned→ref cid ctx (CloseValid.participantSigned b)

-- ── §5.1 burn (μHead Burn arm): the bundle implies the extracted burn checker ─────────────────
-- Both conjuncts are genuine proofs: `==-sound` reflects `mintedCount ≡ 0` into the builtin `_==_`,
-- and `<ᴮ-sound` reflects `1 ≤ burnedCount` (= `0 < burnedCount`) into the builtin `_<_`.
burnValid→ref : ∀ ctx cid → burnValid ctx cid
  → R.burnRefᵇ (R.mkBurnIOᶜ (mintedCount ctx cid) (burnedCount ctx cid)) ≡ true
burnValid→ref ctx cid b =
  &&-intro (==-sound (BurnValid.noneMinted b)) (<ᴮ-sound (BurnValid.somethingBurned b))
