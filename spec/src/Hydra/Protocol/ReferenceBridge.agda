-- Bridge: the extractable decidable checker `Reference.closeRefᵇ` faithfully reflects the
-- (unit-robust) DECIDABLE conjuncts of the abstract `closeValid` (OnChain.lagda.typ). Typecheck-
-- only — this module is NOT extracted (it imports OnChain / set-theory). Imported by Main so the
-- build (`nix build .#spec`) verifies the correspondence.
--
-- Direction proved (completeness): `closeValid ⇒ closeRefᵇ ≡ true`. The reference therefore
-- accepts every spec-valid close, so a reference REJECT implies the spec (hence, by the alignment
-- in agda-haskell-alignment.md, the real validator) rejects — which is what makes the hydra-tx
-- differential test's "reference-reject ⇒ validator-reject" assertion sound.
--
-- NB the deadline / bounded-validity conjuncts of `closeValid` are currently absorbed into the
-- reference's injected (mock) `Ops` (they need the tx validity range + POSIXTime unit handling on
-- the Haskell side), so they are NOT part of `closeRefᵇ` yet; only the unit-robust conjuncts are.
module Hydra.Protocol.ReferenceBridge where

open import Hydra.Protocol.Prelude
open import Hydra.Protocol.Preliminaries
open import Hydra.Protocol.OnChain
import Hydra.Protocol.Reference as R
-- The Bool-check ⇄ proposition reflection lemmas (==ᵇ-refl, ≡→==ᵇ, ≤→≤ᵇ, <→<ᵇ, &&-intro) and the
-- builtin-soundness postulates (==-sound, <ᴮ-sound) live in `RefReflection`; this module is just the
-- `*Valid → ref` correspondence.
open import Hydra.Protocol.RefReflection
open import Relation.Binary.PropositionalEquality using (trans; sym; cong)
open import Relation.Nullary using (yes; no)

-- abstraction map: abstract close-redeemer tag → concrete Reference tag.
-- (Matches the Haskell mirror's `tagOf` in CloseDifferential.hs.)
closeTagOf : CloseType → R.CloseTagᶜ
closeTagOf closeInitial     = R.closeInitialᶜ
closeTagOf (closeAny _ _)   = R.closeAnyᶜ
closeTagOf (closeUnused _ _) = R.closeUnusedᶜ
closeTagOf (closeUsed _ _)  = R.closeUsedᶜ

-- The injected boundary, mocked to `true` (the differential test supplies the same).
mockOps : R.Ops
mockOps = record { closeCryptoOK = λ _ _ _ → true }

-- ── the bridge ──────────────────────────────────────────────────────────────────────────────
-- For any spec-valid close (the produced Closed datum shares the preserved parameters, as the
-- `close` rule guarantees), the reference checker accepts.
-- `validityHi := ValidityInterval.hi (Context.validity ctx)` and the Closed datum's deadline `tfin`
-- now feed the reference's deadline conjunct `tfinalC ≡ validityHi + cp`, discharged from the bundle's
-- `closeDeadlineOK` (the 2nd conjunct `dl`) via `==-sound`. The conjunct holds in all four close cases.
closeValid→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin ct
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct
  → R.closeRefᵇ mockOps (R.mkOpenᶜ v cp) (R.mkClosedᶜ v cp s′ (length C) tfin) (closeTagOf ct)
       (ValidityInterval.hi (Context.validity ctx)) (ValidityInterval.lo (Context.validity ctx)) ≡ true
-- Record-pattern destructuring: `step = close` matches the close rule's constructor, which refines
-- the produced contesters `C` to `[]` (the rule's output) — so the reference's `length C ==ᵇ zero`
-- discharges by `refl`. The named fields (`deadlineOK`, `initialOK`, `anyOK`) replace positional access.
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial
  record { step = close ; deadlineOK = dl ; initialOK = (v≡0 , s≡0 , _) ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro (&&-intro (≡→==ᵇ v≡0) (≡→==ᵇ s≡0))
   (&&-intro refl
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeAny ξ η#)
  record { step = close ; deadlineOK = dl ; anyOK = anyOK ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro (<→<ᵇ anyOK)
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUnused ξ η#)
  record { step = close ; deadlineOK = dl ; validityBounded = vb } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro (==-sound dl) (≤ᴮ-sound vb)))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUsed ξ η#)
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
-- via the `adaOf` additivity law — so a reference value-reject implies the spec rejects. `adaDelta` is
-- the lovelace of ALL spent deposits (`depositsValue`, Plutus `totalNonHeadInputValue`), which is what
-- makes the differential catch the multi-deposit siphon.
-- BOTH projections (`adaOf` and `nonAdaOf`) are now checked: each is an additive homomorphism, so each
-- conjunct follows from the SAME value equation `headValueIn +ᵛ depositsValue ≡ headValue` by the
-- respective additivity law. Checking the non-ada total catches a native-token siphon the lovelace
-- check alone misses.
incrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref
  → R.incRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (depositsValue ctx)) (adaOf (headValue ctx))
          (nonAdaOf (headValueIn ctx)) (nonAdaOf (depositsValue ctx)) (nonAdaOf (headValue ctx)))
     ≡ true
incrementValid→ref ctx cid hk n cp v η ada η′ ξ s ref b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValueIn ctx) (depositsValue ctx)))
                            (cong adaOf (IncrementValid.valueOK b))))
 (&&-intro (==-sound (trans (sym (nonAdaOf-+ᵛ (headValueIn ctx) (depositsValue ctx)))
                            (cong nonAdaOf (IncrementValid.valueOK b))))
           refl))

-- Decrement: version bumps AND the head value shrinks by the decommit. The reference's lovelace check
-- `adaOut + adaDelta ≡ adaIn` follows from `decrementValueOK` (headValue +ᵛ decommitValue ≡ headValueIn)
-- via the `adaOf` additivity law -- so a reference value-reject implies the spec rejects. The ada
-- fields carry: adaIn = head input, adaDelta = decommit value, adaOut = head output (the larger side
-- is the head INPUT, unlike increment).
decrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m
  → R.decRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (decommitValue ctx m)) (adaOf (headValue ctx))
          (nonAdaOf (headValueIn ctx)) (nonAdaOf (decommitValue ctx m)) (nonAdaOf (headValue ctx)))
     ≡ true
decrementValid→ref ctx cid hk n cp v η ada η′ ξ s m b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValue ctx) (decommitValue ctx m)))
                            (cong adaOf (DecrementValid.valueOK b))))
 (&&-intro (==-sound (trans (sym (nonAdaOf-+ᵛ (headValue ctx) (decommitValue ctx m)))
                            (cong nonAdaOf (DecrementValid.valueOK b))))
           refl))

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

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
-- `0 <ᵇ m` from `0 < m`; `burnedCount == n+1` from `burnAllTokensOK` (via `==-sound`); `tfinal < lo`
-- (posted after the deadline) from `afterDeadline` (via `<ᴮ-sound`). The accumulator-membership and
-- value-conservation conjuncts stay in the injected (mock) `fanoutCryptoOK`.
mockOpsFanout : R.OpsFanout
mockOpsFanout = record { fanoutCryptoOK = λ _ → true }

fanoutValid→ref : ∀ ctx cid hk n cp v s η C tfin ada outs m π crs
  → fanoutValid ctx (Closed cid hk n cp v s η C tfin ada) outs m π crs
  → R.fanoutRefᵇ mockOpsFanout
       (R.mkFanoutᶜ m (burnedCount ctx cid) n tfin (ValidityInterval.lo (Context.validity ctx))) ≡ true
fanoutValid→ref ctx cid hk n cp v s η C tfin ada outs m π crs b =
    &&-intro (<→<ᵇ (FanoutValid.outputsPositive b))
   (&&-intro (==-sound (FanoutValid.burnAllTokens b))
   (&&-intro (<ᴮ-sound (FanoutValid.afterDeadline b)) refl))

finalPartialFanoutValid→ref : ∀ ctx cid hk n tfin η ada outs m π crs
  → finalPartialFanoutValid ctx (FanoutProgress cid hk n tfin η ada) outs m π crs
  → R.fanoutRefᵇ mockOpsFanout
       (R.mkFanoutᶜ m (burnedCount ctx cid) n tfin (ValidityInterval.lo (Context.validity ctx))) ≡ true
finalPartialFanoutValid→ref ctx cid hk n tfin η ada outs m π crs b =
    &&-intro (<→<ᵇ (FinalPartialFanoutValid.outputsPositive b))
   (&&-intro (==-sound (FinalPartialFanoutValid.burnAllTokens b))
   (&&-intro (<ᴮ-sound (FinalPartialFanoutValid.afterDeadline b)) refl))

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
       (R.mkRecoverIOᶜ tRec (ValidityInterval.lo (Context.validity ctx))) ≡ true
recoverValid→ref ctx cid tRec C m b =
  &&-intro (<ᴮ-sound (RecoverValid.afterRecoverDeadline b)) refl

-- ── init (μHead minting policy: token count) ────────────────────────────────────────────────────
-- The reference's count check `mintedCount == suc n` holds from the bundle's `mintedCount ctx cid ≡
-- suc n` (the μHead `checkNumberOfTokens`, exactly n+1 tokens minted), discharged via `==-sound`. The
-- remaining μHead conjuncts (seed-spent, ST/PT placement, datum binding) are the injected (mock)
-- `initPlacementOK`. So a reference count-reject ⇒ the spec rejects ⇒ the μHead policy rejects
-- (`WrongNumberOfTokensMinted`).
mockOpsInit : R.OpsInit
mockOpsInit = record { initPlacementOK = λ _ → true }

initValid→ref : ∀ ctx seed cid hk n cp v η ada
  → initValid ctx seed (Open cid hk n cp v η ada)
  → R.initRefᵇ mockOpsInit (R.mkMintIOᶜ n (mintedCount ctx cid)) ≡ true
initValid→ref ctx seed cid hk n cp v η ada b =
  &&-intro (==-sound (InitValid.mintedCountOK b)) refl

-- ── deposit claim (νDeposit, Claim redeemer) ──────────────────────────────────────────────────
-- The reference's before-deadline check `validityHi ≤ᴮ tRecover` holds from the bundle's
-- `ValidityInterval.hi (validity ctx) ≤ DepositDatum.tRecover dd` (§5.2 deposit.ak `before_deadline`:
-- txValidityMax ≤ t_recover), discharged via `≤ᴮ-sound`. The own-head binding (`claimedByOwnHead`,
-- deposit.ak `expect_increment_redeemer`) is now ALSO checked: the deposit datum's head id `cid` equals
-- the spent head's id `hcid`. Head ids are hashes, which the hash-free reference layer cannot hold, so
-- the boundary represents each as the Integer `cidToNat ·` and the reference compares those. The bridge
-- only needs `cid ≡ hcid ⇒ cidToNat cid ≡ cidToNat hcid` — plain `cong` on the encoding, NO injectivity
-- assumption (that would only be needed for the converse, which the one-directional differential does
-- not assert). `cidToNat` is a typecheck-only encoding postulate (it is not part of the extracted
-- reference; the differential supplies a concrete deterministic encoding on the Haskell side).
-- The Increment-redeemer coupling stays injected. So a reference reject ⇒ the spec rejects ⇒ (by the
-- deposit.ak Claim arm) the validator rejects (`DepositPeriodSurpassed` / `expect_increment_redeemer`).
postulate cidToNat : ℍ → ℕ

mockOpsClaim : R.OpsClaim
mockOpsClaim = record { claimIncrementOK = λ _ → true }

claimValid→ref : ∀ ctx cid tRec C hcid hk n cp v η ada
  → claimValid ctx (mkDepositDatum cid tRec C) (Open hcid hk n cp v η ada)
  → R.claimRefᵇ mockOpsClaim
       (R.mkClaimIOᶜ tRec (ValidityInterval.hi (Context.validity ctx)) (cidToNat cid) (cidToNat hcid)) ≡ true
claimValid→ref ctx cid tRec C hcid hk n cp v η ada b =
  &&-intro (≤ᴮ-sound (ClaimValid.beforeRecoverDeadline b))
 (&&-intro (==-sound (cong cidToNat (ClaimValid.claimedByOwnHead b))) refl)
