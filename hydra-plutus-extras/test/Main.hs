module Main where

import "base" Prelude

import Spec qualified
import "hspec" Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec
