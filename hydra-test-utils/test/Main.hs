module Main where

import Hydra.Prelude
import qualified HydraTestUtilsSpec
import Test.Hspec.Runner
import Test.Hydra.Prelude (dualFormatter)

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just (dualFormatter "hydra-test-utils")} HydraTestUtilsSpec.spec
