-- | Pins each name the off-chain extraction surface exports to observable behaviour; see
-- "Hydra.Agda.ReferenceSpec" for why binding-level tests earn their place next to the differentials.
--
-- Two cases here also guard boundaries the oracle got wrong before: 'leaderRef' at @sn == 0@, where
-- the node's signed-'Int' arithmetic elects the LAST party and truncated ℕ subtraction would elect
-- the first, and 'depositStatusRef' with its two periods varied independently, which an oracle
-- taking a single period could not distinguish.
module Hydra.Agda.OffChainReferenceSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

import Hydra.Agda.OffChainReference (
  HsDepositStatus (..),
  allSignedRef,
  contestEligibleRef,
  depositStatusRef,
  leaderRef,
  notAlreadySignedRef,
  reqDecEligibleRef,
  reqSnDecommitOutputsRef,
  reqSnDepositSettledRef,
  reqSnNotBothRef,
  signEligibleRef,
 )

spec :: Spec
spec = do
  -- created deadline T_deposit T_activate t
  describe "depositStatusRef" $ do
    let status = depositStatusRef 1_000 100_000
    it "is Inactive before the activation period has elapsed" $
      status 10_000 500 1_500 `shouldBe` InactiveS
    it "is Active once it has" $
      status 10_000 500 1_501 `shouldBe` ActiveS
    it "is Expired past deadline - T_deposit" $
      status 10_000 500 90_001 `shouldBe` ExpiredS
    it "follows T_activate independently of T_deposit" $
      -- Same instant, same expiry period, longer activation period: still Inactive.
      status 10_000 5_000 2_000 `shouldBe` InactiveS
    it "follows T_deposit independently of T_activate" $
      -- Same instant, same activation period, longer expiry period: already Expired.
      status 99_000 500 2_000 `shouldBe` ExpiredS
    it "prefers Expired over Active when both hold" $
      status 10_000 500 100_000 `shouldBe` ExpiredS

  -- v v̂ s ŝ leaderOk
  describe "signEligibleRef" $ do
    it "signs the next snapshot from the leader at the seen version" $
      signEligibleRef 1 1 5 4 True `shouldBe` True
    it "declines a snapshot number that skips one" $
      signEligibleRef 1 1 6 4 True `shouldBe` False
    it "declines a request from a non-leader" $
      signEligibleRef 1 1 5 4 False `shouldBe` False
    it "declines a version it has not seen" $
      signEligibleRef 2 1 5 4 True `shouldBe` False

  describe "reqDecEligibleRef" $ do
    it "starts a decommit when nothing is in flight" $
      reqDecEligibleRef False False `shouldBe` True
    it "waits behind a commit in flight" $
      reqDecEligibleRef True False `shouldBe` False
    it "waits behind a decommit in flight" $
      reqDecEligibleRef False True `shouldBe` False

  describe "reqSnNotBothRef" $ do
    it "admits a commit alone" $ reqSnNotBothRef True False `shouldBe` True
    it "admits a decommit alone" $ reqSnNotBothRef False True `shouldBe` True
    it "admits neither" $ reqSnNotBothRef False False `shouldBe` True
    it "rejects both at once" $ reqSnNotBothRef True True `shouldBe` False

  describe "reqSnDecommitOutputsRef" $ do
    it "accepts a decommit that produces an output" $
      reqSnDecommitOutputsRef 1 `shouldBe` True
    it "rejects one that produces none" $
      reqSnDecommitOutputsRef 0 `shouldBe` False

  describe "reqSnDepositSettledRef" $ do
    it "settles the deposit bound into the confirmed snapshot" $
      reqSnDepositSettledRef True 7 7 `shouldBe` True
    it "rejects a look-alike deposit with matching content but another tx-id" $
      reqSnDepositSettledRef True 7 8 `shouldBe` False
    it "rejects when the content does not match either" $
      reqSnDepositSettledRef False 7 7 `shouldBe` False

  describe "notAlreadySignedRef" $ do
    it "accepts a party that has not signed this round" $
      notAlreadySignedRef [0, 1] 2 `shouldBe` True
    it "rejects a double signature" $
      notAlreadySignedRef [0, 1] 1 `shouldBe` False

  describe "allSignedRef" $ do
    it "confirms once every party has signed" $
      allSignedRef 3 [0, 1, 2] `shouldBe` True
    it "does not confirm while one is missing" $
      allSignedRef 3 [0, 1] `shouldBe` False
    it "ignores signatures from indices at or above n" $
      allSignedRef 2 [0, 1, 5] `shouldBe` True

  describe "contestEligibleRef" $ do
    it "contests with a newer confirmed snapshot" $
      contestEligibleRef 5 4 `shouldBe` True
    it "does not contest with an equal one" $
      contestEligibleRef 5 5 `shouldBe` False
    it "does not contest with an older one" $
      contestEligibleRef 4 5 `shouldBe` False

  -- m (where the head has suc m parties), sn, 0-based party index
  describe "leaderRef" $ do
    it "elects party (sn - 1) mod n for sn >= 1" $ do
      leaderRef 2 1 0 `shouldBe` True
      leaderRef 2 2 1 `shouldBe` True
      leaderRef 2 3 2 `shouldBe` True
      leaderRef 2 4 0 `shouldBe` True
    it "elects the LAST party at sn == 0, as the node's signed arithmetic does" $ do
      leaderRef 2 0 2 `shouldBe` True
      leaderRef 2 0 0 `shouldBe` False
    it "elects exactly one party in a single-party head" $
      leaderRef 0 7 0 `shouldBe` True
