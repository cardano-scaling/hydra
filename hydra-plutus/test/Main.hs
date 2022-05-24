module Main where

import Hydra.Prelude

import Test.Hspec.Runner (configFormat, defaultConfig, hspecWith)
import Test.Hydra.Prelude (dualFormatter)

import qualified Spec

main :: IO ()
main =
  hspecWith
    defaultConfig{configFormat = Just (dualFormatter "hydra-plutus")}
    Spec.spec
