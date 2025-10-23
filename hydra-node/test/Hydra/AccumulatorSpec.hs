module Hydra.AccumulatorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

-- Import this to make compiler happy
import Accumulator ()

spec :: Spec
spec =
  parallel $
    describe "Haskell Accumulator" $
      it "is used" $
        True `shouldBe` True
