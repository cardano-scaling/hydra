module Hydra.ContestationPeriodSpec where

import Hydra.Prelude

import Hydra.ContestationPeriod (fromDiffTime)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (getNonPositive, getPositive)
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "fromDiffTime" $ do
    prop "works for diff times > 0" $
      isJust . fromDiffTime . getPositive

    prop "fails for diff times <= 0" $
      isNothing . fromDiffTime . getNonPositive
