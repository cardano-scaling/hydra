module Main where

import "hspec" Test.Hspec.Runner
import "hydra-prelude" Hydra.Prelude

import Spec qualified

main :: IO ()
main = hspec Spec.spec
