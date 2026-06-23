-- | Off-chain differential (real-node binding): the Agda-extracted round-robin leader
-- 'Hydra.Agda.OffChainReference.leaderRef' (the §6 figure's @leader(s)@) checked against the REAL
-- 'Hydra.HeadLogic.isLeader'. This is the off-chain counterpart of the on-chain validator differentials:
-- the extracted decision is pinned not just to a Haskell transcription of the figure but to the function
-- the node actually runs, closing the figure↔Agda↔Haskell loop for leader selection.
--
-- Domain note: snapshot numbers in the protocol start at 1, and the extracted @leaderRef@ uses Nat
-- truncated subtraction whereas @isLeader@ subtracts over 'Int'; the two agree for every @sn ≥ 1@ and
-- diverge only at @sn = 0@ (Nat @0 ∸ 1 = 0@ vs Int @-1 `mod` n = n-1@), which is outside the protocol's
-- domain. The property therefore quantifies over @sn ≥ 1@.
module Hydra.OffChainLeaderSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Agda.OffChainReference (leaderRef)
import Hydra.HeadLogic (isLeader)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (SnapshotNumber)
import Test.Hydra.Tx.Fixture (alice, bob, carol)
import Test.QuickCheck (Positive (..), elements, forAll, (===))

spec :: Spec
spec =
  describe "Off-chain round-robin leader: extracted leaderRef vs real Hydra.HeadLogic.isLeader" $ do
    -- A 3-party head; @leaderRef@ takes @m@ where #parties = @suc m@, so @m = 2@ here.
    let parties = [alice, bob, carol]
        params = HeadParameters{contestationPeriod = 60, parties}
    it "sn 1 elects party 0 (alice)" $
      leaderRef 2 1 0 `shouldBe` True
    it "sn 2 elects party 1 (bob)" $
      leaderRef 2 2 1 `shouldBe` True
    it "sn 3 elects party 2 (carol)" $
      leaderRef 2 3 2 `shouldBe` True
    it "sn 4 wraps back to party 0 (alice)" $
      leaderRef 2 4 0 `shouldBe` True
    it "a non-leader index is rejected" $
      leaderRef 2 1 1 `shouldBe` False
    prop "leaderRef agrees with the real isLeader for every party index, sn ≥ 1" $
      \(Positive sn) -> forAll (elements (zip [0 :: Integer ..] parties)) $ \(i, party) ->
        leaderRef 2 sn i === isLeader params party (fromInteger sn :: SnapshotNumber)
