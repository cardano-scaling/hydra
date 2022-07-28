module Test.Hydra.Cluster.FaucetSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

spec :: Spec
spec = do
    it "should be true" $ do
      True `shouldBe` True
