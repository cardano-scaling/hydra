-- | Off-chain differential test (Tier 2, off-chain side): runs the Agda-extracted decidable off-chain
-- HeadLogic reference ('Hydra.Agda.OffChainReference') and checks it against the §6 figure logic.
--
-- Exercises the whole off-chain pipeline end to end: Agda decidable function → MAlonzo extraction →
-- 'Hydra.Agda.OffChainReference' shim → this test, exactly as 'Hydra.Agda.Reference' does for the
-- on-chain validators. Two decisions are covered: the @tick@ deposit-status transition and the reqSn
-- signing-eligibility check. The comparison is against a faithful Haskell transcription of the figure
-- (it pins the extracted function to the spec); cross-checking against the REAL 'Hydra.HeadLogic' (its
-- @onOpenChainTick@ / @requireReqSn@) is later off-chain-mechanization work.
module Hydra.Tx.Contract.OffChainDifferential (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Agda.OffChainReference (
  HsDepositStatus (..),
  allSignedRef,
  contestEligibleRef,
  depositStatusRef,
  notAlreadySignedRef,
  reqDecEligibleRef,
  signEligibleRef,
 )
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

-- | Transcriptions of the remaining decidable handler guards.
refReqDecEligible :: Bool -> Bool -> Bool
refReqDecEligible commit decommit = not commit && not decommit

refNotAlreadySigned :: [Integer] -> Integer -> Bool
refNotAlreadySigned signers j = j `notElem` signers

refAllSigned :: Integer -> [Integer] -> Bool
refAllSigned n signers = all (`elem` signers) [0 .. n - 1]

refContestEligible :: Integer -> Integer -> Bool
refContestEligible sBar sc = sc < sBar

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
  describe "Off-chain reqDec eligibility reference (U_α = ∅ ∧ tx_ω = ⊥)" $ do
    it "eligible only when nothing is in flight" $
      reqDecEligibleRef False False `shouldBe` True
    it "ineligible with a commit in flight" $
      reqDecEligibleRef True False `shouldBe` False
    it "ineligible with a decommit in flight" $
      reqDecEligibleRef False True `shouldBe` False
    prop "extracted reqDecEligibleRef matches the figure transcription" $
      \commit decommit -> reqDecEligibleRef commit decommit === refReqDecEligible commit decommit
  describe "Off-chain ackSn-collect no-double-sign reference ((j,·) ∉ Σ̂)" $ do
    it "fresh signer" $ notAlreadySignedRef [0, 1, 2] 3 `shouldBe` True
    it "already signed" $ notAlreadySignedRef [0, 1, 2] 1 `shouldBe` False
    prop "extracted notAlreadySignedRef matches the figure transcription" $
      \signers (NonNegative j) -> notAlreadySignedRef signers j === refNotAlreadySigned signers j
  describe "Off-chain ackSn-confirm n-of-n reference (∀ k < n : (k,·) ∈ Σ̂)" $ do
    it "all signed" $ allSignedRef 3 [0, 1, 2] `shouldBe` True
    it "all signed, with extras" $ allSignedRef 3 [0, 1, 2, 5] `shouldBe` True
    it "missing a signer" $ allSignedRef 3 [0, 1] `shouldBe` False
    it "vacuous for n = 0" $ allSignedRef 0 [] `shouldBe` True
    prop "extracted allSignedRef matches the figure transcription" $
      \(NonNegative n) signers -> allSignedRef n signers === refAllSigned n signers
  describe "Off-chain contest re-post reference (S̄.s > s_c)" $ do
    it "newer confirmed snapshot re-posts" $ contestEligibleRef 5 3 `shouldBe` True
    it "equal does not re-post" $ contestEligibleRef 3 3 `shouldBe` False
    prop "extracted contestEligibleRef matches the figure transcription" $
      \(NonNegative sBar) (NonNegative sc) -> contestEligibleRef sBar sc === refContestEligible sBar sc
