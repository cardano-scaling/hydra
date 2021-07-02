module Main where

import Hydra.Prelude
import qualified Spec
import Test.HSpec.JUnit
import Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig{configFormat = Just $ junitFormat "test-results.xml" "my-tests"} Spec.spec
