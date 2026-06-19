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

open import Data.Nat using (z≤n; s≤s)

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
⌊_⌋ᴋ : CloseType → R.CloseTagᶜ
⌊ closeInitial   ⌋ᴋ = R.closeInitialᶜ
⌊ closeAny _ _   ⌋ᴋ = R.closeAnyᶜ
⌊ closeUnused _ _ ⌋ᴋ = R.closeUnusedᶜ
⌊ closeUsed _ _  ⌋ᴋ = R.closeUsedᶜ

-- The injected boundary, mocked to `true` (the differential test supplies the same).
trueOps : R.Ops
trueOps = record { closeCryptoOK = λ _ _ _ → true }

-- ── the bridge ──────────────────────────────────────────────────────────────────────────────
-- For any spec-valid close (the produced Closed datum shares the preserved parameters, as the
-- `close` rule guarantees), the reference checker accepts.
closeValid→ref : ∀ ctx cid hk n cp v η ada s′ η′ C tfin ct
  → closeValid ctx (Open cid hk n cp v η ada) (Closed cid hk n cp v s′ η′ C tfin ada) ct
  → R.closeRefᵇ trueOps (R.mkOpenᶜ v cp) (R.mkClosedᶜ v cp s′ (length C)) ⌊ ct ⌋ᴋ ≡ true
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin closeInitial
  (close , _ , _ , ini , _ , _ , _ , _ , _ , _) =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro (&&-intro (≡→==ᵇ (proj₁ ini)) (≡→==ᵇ (proj₁ (proj₂ ini))))
   (&&-intro refl refl))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeAny ξ η#)
  (close , _ , _ , _ , _ , _ , any , _ , _ , _) =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro (<→<ᵇ any) refl))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUnused ξ η#)
  (close , _ , _ , _ , _ , _ , _ , _ , _ , _) =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl refl))))
closeValid→ref ctx cid hk n cp v η ada s′ η′ C tfin (closeUsed ξ η#)
  (close , _ , _ , _ , _ , _ , _ , _ , _ , _) =
    &&-intro (==ᵇ-refl v)
   (&&-intro (==ᵇ-refl cp)
   (&&-intro refl
   (&&-intro refl
   (&&-intro refl refl))))

-- ── increment / decrement ───────────────────────────────────────────────────────────────────
-- The produced Open datum carries `suc v`; the reference's `versionOut ==ᵇ suc versionIn` holds.
trueOpsInc : R.OpsInc
trueOpsInc = record { incCryptoOK = λ _ → true }

incrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref
  → R.incRefᵇ trueOpsInc (R.mkIncIOᶜ v (suc v)) ≡ true
incrementValid→ref ctx cid hk n cp v η ada η′ ξ s ref (increment , _) =
  &&-intro (==ᵇ-refl (suc v)) refl

decrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m
  → R.incRefᵇ trueOpsInc (R.mkIncIOᶜ v (suc v)) ≡ true
decrementValid→ref ctx cid hk n cp v η ada η′ ξ s m (decrement , _) =
  &&-intro (==ᵇ-refl (suc v)) refl

-- ── contest ─────────────────────────────────────────────────────────────────────────────────
-- Version preserved (both v), snapshot strictly increases (s < s′ from the bundle), one contester
-- appended (output contesters ≡ kh ∷ C, so length ≡ suc (length C)).
trueOpsContest : R.OpsContest
trueOpsContest = record { contestCryptoOK = λ _ → true }

contestValid→ref : ∀ ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  → contestValid ctx (Closed cid hk n cp v s η C tfin ada)
                     (Closed cid hk n cp v s′ η′ (kh ∷ C) tfin′ ada) ct
  → R.contestRefᵇ trueOpsContest
       (R.mkContestIOᶜ v v s s′ (length C) (length (kh ∷ C))) ≡ true
contestValid→ref ctx cid hk n cp v s η C tfin ada s′ η′ kh tfin′ ct
  (contest _ , _ , _ , _ , _ , s<s′ , _ , _ , _) =
    &&-intro (==ᵇ-refl v)
   (&&-intro (<→<ᵇ s<s′)
   (&&-intro (==ᵇ-refl (suc (length C))) refl))

-- ── fanout / finalPartialFanout ───────────────────────────────────────────────────────────────
-- The reference's `0 <ᵇ m` holds from the bundle's `0 < m` (the §5.8 m>0 guard).
trueOpsFanout : R.OpsFanout
trueOpsFanout = record { fanoutCryptoOK = λ _ → true }

fanoutValid→ref : ∀ ctx cid hk n cp v s η C tfin ada outs m π crs
  → fanoutValid ctx (Closed cid hk n cp v s η C tfin ada) outs m π crs
  → R.fanoutRefᵇ trueOpsFanout (R.mkFanoutᶜ m) ≡ true
fanoutValid→ref ctx cid hk n cp v s η C tfin ada outs m π crs
  (fanout , _ , _ , 0<m , _ , _) =
    &&-intro (<→<ᵇ 0<m) refl

finalPartialFanoutValid→ref : ∀ ctx cid hk n tfin η ada outs m π crs
  → finalPartialFanoutValid ctx (FanoutProgress cid hk n tfin η ada) outs m π crs
  → R.fanoutRefᵇ trueOpsFanout (R.mkFanoutᶜ m) ≡ true
finalPartialFanoutValid→ref ctx cid hk n tfin η ada outs m π crs
  (finalPartialFanout , _ , _ , 0<m , _ , _) =
    &&-intro (<→<ᵇ 0<m) refl
