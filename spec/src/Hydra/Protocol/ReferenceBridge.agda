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

open import Agda.Builtin.Nat using (_==_)
open import Data.Nat using (z≤n; s≤s)
open import Relation.Binary.PropositionalEquality using (trans; sym; cong)

-- Soundness of the BUILTIN Nat equality `_==_` w.r.t. propositional equality. `incRefᵇ` checks the
-- lovelace conjunct with `_==_` (native Integer equality at extraction) rather than the structural
-- `_==ᵇ_` (which is O(n) unary recursion, pathological on lovelace-scale values). The builtin does
-- not reduce on open terms, so this reflection lemma is postulated; it is trivially true and lives
-- in the same trust category as the builtin arithmetic (`_+_`) the bridge already relies on.
postulate
  ==-sound : ∀ {m n} → m ≡ n → (m == n) ≡ true

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

-- Increment: version bumps AND the head value grows by ALL deposits. The reference's lovelace check
-- `adaIn + adaDelta ≡ adaOut` follows from `incrementValueOK` (headValueIn +ᵛ depositsValue ≡ headValue)
-- via the `adaOf` additivity law — so a reference value-reject implies the spec rejects. `adaDelta` is
-- the lovelace of ALL spent deposits (`depositsValue`, Plutus `totalNonHeadInputValue`), which is what
-- makes the differential catch the multi-deposit siphon.
incrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s ref
  → incrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s ref
  → R.incRefᵇ trueOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (depositsValue ctx)) (adaOf (headValue ctx)))
     ≡ true
incrementValid→ref ctx cid hk n cp v η ada η′ ξ s ref (increment , _ , _ , valOK , _) =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValueIn ctx) (depositsValue ctx))) (cong adaOf valOK)))
           refl)

-- Decrement: version bumps AND the head value shrinks by the decommit. The reference's lovelace check
-- `adaOut + adaDelta ≡ adaIn` follows from `decrementValueOK` (headValue +ᵛ decommitValue ≡ headValueIn)
-- via the `adaOf` additivity law -- so a reference value-reject implies the spec rejects. The ada
-- fields carry: adaIn = head input, adaDelta = decommit value, adaOut = head output (the larger side
-- is the head INPUT, unlike increment).
decrementValid→ref : ∀ ctx cid hk n cp v η ada η′ ξ s m
  → decrementValid ctx (Open cid hk n cp v η ada) (Open cid hk n cp (suc v) η′ ada) ξ s m
  → R.decRefᵇ trueOpsInc
       (R.mkIncIOᶜ v (suc v) (adaOf (headValueIn ctx)) (adaOf (decommitValue ctx m)) (adaOf (headValue ctx)))
     ≡ true
decrementValid→ref ctx cid hk n cp v η ada η′ ξ s m (decrement , _ , _ , valOK , _) =
  &&-intro (==ᵇ-refl (suc v))
 (&&-intro (==-sound (trans (sym (adaOf-+ᵛ (headValue ctx) (decommitValue ctx m))) (cong adaOf valOK)))
           refl)

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
