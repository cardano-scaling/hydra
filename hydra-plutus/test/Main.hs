module Main where

import "hydra-prelude" Hydra.Prelude

import "hspec" Test.Hspec.Runner (hspec)

import Spec qualified

main :: IO ()
main = hspec Spec.spec
