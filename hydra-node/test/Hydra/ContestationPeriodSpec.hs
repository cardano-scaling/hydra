module Hydra.ContestationPeriodSpec where

import Hydra.Prelude hiding (label)

import Data.Time (secondsToNominalDiffTime)
import Hydra.Tx.ContestationPeriod (ContestationPeriod, fromNominalDiffTime)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (getNonPositive, getPositive, (===))
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "fromNominalDiffTime" $ do
    prop "works for diff times > 0" $
      isJust . fromNominalDiffTime . getPositive

    prop "fails for diff times <= 0" $
      isNothing . fromNominalDiffTime . getNonPositive

    prop "rounds to 1 second" $ \n ->
      let subSecond = getPositive n / 100 -- Definitely < 1 second
       in fromNominalDiffTime (secondsToNominalDiffTime subSecond)
            === (fromNominalDiffTime 1 :: Maybe ContestationPeriod)
