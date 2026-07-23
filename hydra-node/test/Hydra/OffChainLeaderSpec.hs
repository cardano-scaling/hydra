-- | Off-chain differential (real-node binding): the Agda-extracted round-robin leader
-- 'Hydra.Agda.OffChainReference.leaderRef' (the §6 figure's @leader(s)@) checked against the REAL
-- 'Hydra.HeadLogic.isLeader'. This is the off-chain counterpart of the on-chain validator differentials:
-- the extracted decision is pinned not just to a Haskell transcription of the figure but to the function
-- the node actually runs, closing the figure↔Agda↔Haskell loop for leader selection.
--
-- Domain note: snapshot numbers in the protocol start at 1, but the property covers @sn = 0@ too.
-- @leaderRef@ works over Nat, whose truncated subtraction would make @0 - 1@ zero where @isLeader@'s
-- 'Int' arithmetic gives @-1 `mod` n = n-1@; the extracted checker adds @m@ instead of subtracting 1
-- (the same residue for every @sn >= 1@, and @n-1@ at zero) precisely so the oracle cannot disagree
-- with the node anywhere - including on an @sn = 0@ a peer could put in a @ReqSn@.
module Hydra.OffChainLeaderSpec (spec) where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Agda.OffChainReference (leaderRef)
import Hydra.HeadLogic (isLeader)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Snapshot (SnapshotNumber)
import Test.Hydra.Tx.Fixture (alice, bob, carol)
import Test.QuickCheck (NonNegative (..), elements, forAll, (===))

spec :: Spec
spec =
  describe "Off-chain round-robin leader: extracted leaderRef vs real Hydra.HeadLogic.isLeader" $ do
    -- A 3-party head; @leaderRef@ takes @m@ where #parties = @suc m@, so @m = 2@ here.
    let parties = [alice, bob, carol]
        params = HeadParameters{contestationPeriod = 60, depositPeriod = 60, parties}
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
    -- The boundary the truncated-subtraction version got wrong: at sn 0 the node's signed
    -- arithmetic elects the LAST party, not the first.
    it "sn 0 elects the last party (n-1), as the node's signed arithmetic does" $ do
      leaderRef 2 0 2 `shouldBe` True
      leaderRef 2 0 0 `shouldBe` False
      isLeader params carol 0 `shouldBe` True
    prop "leaderRef agrees with the real isLeader for every party index, including sn = 0" $
      \(NonNegative sn) -> forAll (elements (zip [0 :: Integer ..] parties)) $ \(i, party) ->
        leaderRef 2 sn i === isLeader params party (fromInteger sn :: SnapshotNumber)
