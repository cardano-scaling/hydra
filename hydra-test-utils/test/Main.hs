module Main where

import Hydra.Prelude
import HydraTestUtilsSpec qualified
import Test.Hspec.Runner
import Test.Hydra.Prelude (combinedHspecFormatter)

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just (combinedHspecFormatter "hydra-test-utils")} HydraTestUtilsSpec.spec
