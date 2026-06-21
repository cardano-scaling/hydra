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

open import Agda.Builtin.Nat using (_==_) renaming (_<_ to _<ᴮ_)
open import Data.Nat using (z≤n; s≤s)
open import Relation.Binary.PropositionalEquality using (trans; sym; cong)

-- Soundness of the BUILTIN Nat equality `_==_` w.r.t. propositional equality. `incRefᵇ` checks the
-- lovelace conjunct with `_==_` (native Integer equality at extraction) rather than the structural
-- `_==ᵇ_` (which is O(n) unary recursion, pathological on lovelace-scale values). The builtin does
-- not reduce on open terms, so this reflection lemma is postulated; it is trivially true and lives
-- in the same trust category as the builtin arithmetic (`_+_`) the bridge already relies on.
postulate
  ==-sound : ∀ {m n} → m ≡ n → (m == n) ≡ true

-- Soundness of the BUILTIN Nat strict-less-than `_<ᴮ_` w.r.t. the standard-library `_<_`. `recoverRefᵇ`
-- checks the after-deadline conjunct with the builtin `_<_` (native Integer `<` at extraction) rather
-- than the structural `_<ᵇ_` (O(n) unary recursion, pathological on POSIXTime-ms deadlines). Same trust
-- category as `==-sound`: the builtin does not reduce on open terms, but trivially agrees with `_<_`.
  <ᴮ-sound : ∀ {m n} → m < n → (m <ᴮ n) ≡ true

-- ── reflection lemmas: the Bool checks of Reference reflect the propositional relations ──────
==ᵇ-refl : ∀ n → (n R.==ᵇ n) ≡ true
==ᵇ-refl zero    = refl
==ᵇ-refl (suc n) = ==ᵇ-refl n

≡→==ᵇ : ∀ {m n} → m ≡ n → (m R.==ᵇ n) ≡ true
≡→==ᵇ {m} refl = ==ᵇ-refl m

≤→≤ᵇ : ∀ {m n} → m ≤ n → (m R.≤ᵇ n) ≡ true
≤→≤ᵇ z≤n     = refl
≤→≤ᵇ (s≤s p) = ≤→≤ᵇ p

<→<ᵇ : ∀ {m n} → m < n → (m R.<ᵇ n) ≡ true
<→<ᵇ p = ≤→≤ᵇ p

&&-intro : ∀ {a b} → a ≡ true → b ≡ true → (a R.&& b) ≡ true
&&-intro refl q = q

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
       (ValidityInterval.hi (Context.validity ctx)) ≡ true
-- Record-pattern destructuring: `step = close` matches the close rule's constructor, which refines
-- the produced contesters `C` to `[]` (the rule's output) — so the reference's `length C ==ᵇ zero`
-- discharges by `refl`. The named fields (`deadlineOK`, `initialOK`, `anyOK`) replace positional access.
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial
  record { step = close ; deadlineOK = dl ; initialOK = (v≡0 , s≡0 , _) } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro (&&-intro (≡→==ᵇ v≡0) (≡→==ᵇ s≡0))
   (&&-intro refl
   (&&-intro refl (==-sound dl))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeAny ξ η#)
  record { step = close ; deadlineOK = dl ; anyOK = anyOK } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro (<→<ᵇ anyOK)
   (&&-intro refl (==-sound dl))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUnused ξ η#)
  record { step = close ; deadlineOK = dl } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl (==-sound dl))))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUsed ξ η#)
  record { step = close ; deadlineOK = dl } =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl (==-sound dl))))))

-- ── increment / decrement ───────────────────────────────────────────────────────────────────
-- The produced Open datum carries `suc v`; the reference's `versionOut ==ᵇ suc versionIn` holds.
mockOpsInc : R.OpsInc
mockOpsInc = record { incCryptoOK = λ _ → true }

-- Increment: version bumps AND the head value grows by ALL deposits. The reference's lovelace check
-- `adaIn + adaDelta ≡ adaOut` follows from `incrementValueOK` (headValueIn +ᵛ depositsValue ≡ headValue)
-- via the `adaOf` additivity law — so a reference value-reject implies the spec rejects. `adaDelta` is
-- the lovelace of ALL spent deposits (`depositsValue`, Plutus `totalNonHeadInputValue`), which is what
-- makes the differential catch the multi-deposit siphon.
incrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref
  → R.incRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (depositsValue ctx)) (adaOf (headValue ctx)))
     ≡ true
incrementValid→ref ctx cid hk n cp v η ada η′ ξ s ref b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValueIn ctx) (depositsValue ctx)))
                            (cong adaOf (IncrementValid.valueOK b))))
           refl)

-- Decrement: version bumps AND the head value shrinks by the decommit. The reference's lovelace check
-- `adaOut + adaDelta ≡ adaIn` follows from `decrementValueOK` (headValue +ᵛ decommitValue ≡ headValueIn)
-- via the `adaOf` additivity law -- so a reference value-reject implies the spec rejects. The ada
-- fields carry: adaIn = head input, adaDelta = decommit value, adaOut = head output (the larger side
-- is the head INPUT, unlike increment).
decrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m
  → R.decRefᵇ mockOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (decommitValue ctx m)) (adaOf (headValue ctx)))
     ≡ true
decrementValid→ref ctx cid hk n cp v η ada η′ ξ s m b =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValue ctx) (decommitValue ctx m)))
                            (cong adaOf (DecrementValid.valueOK b))))
           refl)

-- ── contest ─────────────────────────────────────────────────────────────────────────────────
-- Version preserved (both v), snapshot strictly increases (s < s′ from the bundle), one contester
-- appended (output contesters ≡ kh ∷ C, so length ≡ suc (length C)).
mockOpsContest : R.OpsContest
mockOpsContest = record { contestCryptoOK = λ _ → true }

contestValid→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.contestRefᵇ mockOpsContest
       (R.mkContestIOᶜ v v s s′ (length C) (length (kh ∷ C))) ≡ true
contestValid→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct b =
    &&-intro (==ᵇ-refl v)
   (&&-intro (<→<ᵇ (ContestValid.snapIncreases b))
   (&&-intro (==ᵇ-refl (suc (length C))) refl))

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
-- The reference's `0 <ᵇ m` holds from the bundle's `0 < m` (the §5.8 m>0 guard).
mockOpsFanout : R.OpsFanout
mockOpsFanout = record { fanoutCryptoOK = λ _ → true }

fanoutValid→ref : ∀ ctx cid hk n cp v s η C tfin ada outs m π crs
  → fanoutValid ctx (Closed cid hk n cp v s η C tfin ada) outs m π crs
  → R.fanoutRefᵇ mockOpsFanout (R.mkFanoutᶜ m) ≡ true
fanoutValid→ref ctx cid hk n cp v s η C tfin ada outs m π crs b =
    &&-intro (<→<ᵇ (FanoutValid.outputsPositive b)) refl

finalPartialFanoutValid→ref : ∀ ctx cid hk n tfin η ada outs m π crs
  → finalPartialFanoutValid ctx (FanoutProgress cid hk n tfin η ada) outs m π crs
  → R.fanoutRefᵇ mockOpsFanout (R.mkFanoutᶜ m) ≡ true
finalPartialFanoutValid→ref ctx cid hk n tfin η ada outs m π crs b =
    &&-intro (<→<ᵇ (FinalPartialFanoutValid.outputsPositive b)) refl

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
