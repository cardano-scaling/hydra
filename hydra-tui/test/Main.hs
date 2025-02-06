module Main where

import Hydra.Prelude
import Spec qualified
import Test.Hspec.Runner

main :: IO ()
main = hspec Spec.spec
