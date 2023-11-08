module Main where

import Hydra.Prelude

import Test.Hspec.Runner (configFormat, defaultConfig, hspecWith)
import Test.Hydra.Prelude (combinedHspecFormatter)

import Spec qualified

main :: IO ()
main =
  hspecWith
    defaultConfig{configFormat = Just (combinedHspecFormatter "hydra-plutus")}
    Spec.spec
