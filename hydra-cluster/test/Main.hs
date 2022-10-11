module Main where

import Hydra.Prelude
import qualified Spec
import Test.Hspec.Runner
import Test.Hydra.Prelude (combinedHspecFormatter)

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just (combinedHspecFormatter "hydra-cluster")} Spec.spec
