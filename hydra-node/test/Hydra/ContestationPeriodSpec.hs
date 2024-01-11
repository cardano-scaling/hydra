module Hydra.ContestationPeriodSpec where

import Hydra.Prelude

import Data.Time (picosecondsToDiffTime)
import Hydra.ContestationPeriod (ContestationPeriod, fromDiffTime)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (getNonPositive, getPositive, (===))
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "fromDiffTime" $ do
    prop "works for diff times > 0" $
      isJust . fromDiffTime . getPositive

    prop "fails for diff times <= 0" $
      isNothing . fromDiffTime . getNonPositive

    prop "rounds to 1 second" $ \n ->
      let subSecondPicos = getPositive n `mod` 1_000_000_000_000
       in fromDiffTime (picosecondsToDiffTime subSecondPicos)
            === (fromDiffTime 1 :: Maybe ContestationPeriod)
