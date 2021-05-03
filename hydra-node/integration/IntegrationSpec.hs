module IntegrationSpec where

import Cardano.Prelude

import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
 )

spec :: Spec
spec = describe "Integration tests" $ do
  it "should exist" $ True `shouldBe` True
