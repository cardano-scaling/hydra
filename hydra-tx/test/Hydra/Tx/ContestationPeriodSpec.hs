module Hydra.Tx.ContestationPeriodSpec where

import Hydra.Prelude hiding (label)

import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromNominalDiffTime)
import Test.Hspec (Spec, describe, shouldThrow)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (getNonPositive, getPositive, (===))
import Test.QuickCheck.Instances.Time ()
import "time" Data.Time (secondsToNominalDiffTime)

spec :: Spec
spec = do
  describe "fromInteger" $ do
    prop "works for > 0" $
      (`seq` True) . fromInteger @ContestationPeriod . getPositive

    prop "fails for <= 0" $ \np -> do
      evaluate (fromInteger @ContestationPeriod $ getNonPositive np)
        `shouldThrow` \(SomeException _) -> True

  describe "toEnum" $ do
    prop "works for > 0" $
      (`seq` True) . toEnum @ContestationPeriod . getPositive

    prop "fails for <= 0" $ \np -> do
      evaluate (fromInteger @ContestationPeriod $ getNonPositive np)
        `shouldThrow` \(SomeException _) -> True

  describe "fromNominalDiffTime" $ do
    prop "works for diff times `> 0`" $
      isJust . fromNominalDiffTime . getPositive

    prop "fails for diff times `<= 0`" $
      isNothing . fromNominalDiffTime . getNonPositive

    prop "rounds to 1 second" $ \n ->
      let subSecond = getPositive n / 100 -- Definitely < 1 second
       in fromNominalDiffTime (secondsToNominalDiffTime subSecond)
            === (fromNominalDiffTime 1 :: Maybe ContestationPeriod)
