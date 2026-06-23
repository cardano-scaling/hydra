-- | Off-chain differential test (Tier 2, off-chain side): runs the Agda-extracted decidable off-chain
-- HeadLogic reference ('Hydra.Agda.OffChainReference') and checks it against the §6 figure logic.
--
-- Exercises the whole off-chain pipeline end to end — Agda decidable function → MAlonzo extraction →
-- 'Hydra.Agda.OffChainReference' shim → this test — exactly as 'Hydra.Agda.Reference' does for the
-- on-chain validators. Two decisions are covered: the @tick@ deposit-status transition and the reqSn
-- signing-eligibility check. The comparison is against a faithful Haskell transcription of the figure
-- (it pins the extracted function to the spec); cross-checking against the REAL 'Hydra.HeadLogic' (its
-- @onOpenChainTick@ / @requireReqSn@) is later off-chain-mechanization work.
module Hydra.Tx.Contract.OffChainDifferential (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Agda.OffChainReference (HsDepositStatus (..), depositStatusRef, signEligibleRef)
import Test.QuickCheck (NonNegative (..), (===))

-- | The §6 @tick@ deposit-status transition, transcribed in Haskell (Nat truncated subtraction to
-- match the extracted Agda): @t > deadline − T_deposit@ ⇒ Expired; else @t > created + T_deposit@ ⇒
-- Active; else Inactive.
referenceStatus :: Integer -> Integer -> Integer -> Integer -> HsDepositStatus
referenceStatus created deadline tDeposit t
  | (deadline `monus` tDeposit) < t = ExpiredS
  | (created + tDeposit) < t = ActiveS
  | otherwise = InactiveS
 where
  monus :: Integer -> Integer -> Integer
  monus a b = max 0 (a - b)

-- | The §6 reqSn signing-eligibility check, transcribed: @v == v̂ ∧ s == ŝ + 1 ∧ leaderOk@.
referenceEligible :: Integer -> Integer -> Integer -> Integer -> Bool -> Bool
referenceEligible v vHat s sHat leaderOk = v == vHat && s == sHat + 1 && leaderOk

spec :: Spec
spec = do
  describe "Off-chain deposit-status reference (tick handler)" $ do
    it "Inactive before created + T_deposit" $
      depositStatusRef 0 100 10 5 `shouldBe` InactiveS
    it "Active once t > created + T_deposit" $
      depositStatusRef 0 100 10 15 `shouldBe` ActiveS
    it "still Active at the deadline − T_deposit boundary (not yet expired)" $
      depositStatusRef 0 100 10 90 `shouldBe` ActiveS
    it "Expired once t > deadline − T_deposit" $
      depositStatusRef 0 100 10 95 `shouldBe` ExpiredS
    prop "extracted depositStatusRef matches the figure transcription (non-negative times)" $
      \(NonNegative created) (NonNegative deadline) (NonNegative tDeposit) (NonNegative t) ->
        depositStatusRef created deadline tDeposit t === referenceStatus created deadline tDeposit t
  describe "Off-chain reqSn signing-eligibility reference" $ do
    it "eligible: matching version, s = ŝ+1, leader" $
      signEligibleRef 3 3 5 4 True `shouldBe` True
    it "ineligible: version mismatch" $
      signEligibleRef 2 3 5 4 True `shouldBe` False
    it "ineligible: s ≠ ŝ+1" $
      signEligibleRef 3 3 6 4 True `shouldBe` False
    it "ineligible: requested leader is not the sender" $
      signEligibleRef 3 3 5 4 False `shouldBe` False
    prop "extracted signEligibleRef matches the figure transcription (non-negative v/s)" $
      \(NonNegative v) (NonNegative vHat) (NonNegative s) (NonNegative sHat) leaderOk ->
        signEligibleRef v vHat s sHat leaderOk === referenceEligible v vHat s sHat leaderOk
