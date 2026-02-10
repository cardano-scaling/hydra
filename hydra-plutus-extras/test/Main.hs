module Main where

import "base" Prelude
import "hspec" Test.Hspec (hspec)

import Spec qualified

main :: IO ()
main = hspec Spec.spec
