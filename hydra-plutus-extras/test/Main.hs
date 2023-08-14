module Main where

import Prelude

import qualified Spec
import Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec
