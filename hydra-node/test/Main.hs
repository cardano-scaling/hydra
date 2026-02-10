module Main where

import Spec qualified
import "hspec" Test.Hspec.Runner
import "hydra-prelude" Hydra.Prelude

main :: IO ()
main = hspec Spec.spec
