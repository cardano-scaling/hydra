module Main where

import Hydra.Prelude
import qualified Spec
import Test.Hspec.Runner
import Test.Hydra.Prelude (dualFormatter)

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just (dualFormatter "merkle-patricia-tree")} Spec.spec
