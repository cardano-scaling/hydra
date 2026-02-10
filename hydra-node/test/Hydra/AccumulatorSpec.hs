module Hydra.AccumulatorSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

-- Import this to make compiler happy
import "haskell-accumulator" Accumulator ()

spec :: Spec
spec =
  parallel $
    describe "Haskell Accumulator" $
      it "is used" $
        True `shouldBe` True
