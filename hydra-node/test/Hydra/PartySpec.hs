{-# LANGUAGE TypeApplications #-}

module Hydra.PartySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Party (Party (..))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck ((==>))

spec :: Spec
spec = do
  it "can be constructed fromInteger" $ do
    (10 :: Party) `shouldBe` Party{alias = Nothing, vkey = 10}
  it "shows verification key as hex" $ do
    show (10 :: Party) `shouldContain` "000a"
  it "supports aliases" $ do
    show (Party (Just "Bob") 10) `shouldContain` "Bob"

  describe "Eq" $ do
    it "uses aliases" $
      Party{alias = Just "Alice", vkey = 10} /= Party{alias = Just "Bob", vkey = 10}
    it "not ignores aliases" $
      Party{alias = Nothing, vkey = 10} /= Party{alias = Just "Bob", vkey = 10}

  describe "Ord" $ do
    it "orders by alias first" $
      Party{alias = Just "Alice", vkey = 20} <= Party{alias = Just "Bob", vkey = 10}
    it "orders by vkey second" $
      Party{alias = Just "Alice", vkey = 20} <= Party{alias = Just "Alice", vkey = 30}
    it "prefers aliased keys" $
      Party{alias = Just "Alice", vkey = 20} <= Party{alias = Nothing, vkey = 10}
    it "orders by vkey otherwise" $
      Party{alias = Nothing, vkey = 10} <= Party{alias = Nothing, vkey = 20}
    it "orders by vkey otherwise 2" $
      Party{alias = Nothing, vkey = 20} >= Party{alias = Nothing, vkey = 10}

    prop "is transitive" $ \(x :: Party, y, z) ->
      x <= y && y <= z ==> x <= z
    prop "is reflexive" $ \(x :: Party) ->
      x <= x
    prop "is antisymmetric" $ \(x :: Party, y) ->
      (x <= y && y <= x) == (x == y)

  roundtripAndGoldenSpecs (Proxy @Party)
