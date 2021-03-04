module Test.LocalClusterSpec where

import Cardano.Prelude

import Test.Hspec (Spec, describe, it, pendingWith)

spec :: Spec
spec = describe "Hydra local cluster" $ do
  it "should start" $ pendingWith "not implemented"
