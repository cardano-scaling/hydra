module Main where

import Prelude

import Spec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec
