module Main where

import Hydra.Prelude

import Test.Hspec.Runner (configFormat, defaultConfig, hspecWith)
import Test.Hydra.Prelude (combinedHspecFormatter)

import qualified Spec

main :: IO ()
main =
  hspecWith
    defaultConfig{configFormat = Just (combinedHspecFormatter "plutus-merkle-tree")}
    Spec.spec
