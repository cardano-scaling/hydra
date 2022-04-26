{-# LANGUAGE TypeApplications #-}

module Hydra.PartySpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Party (Party (..))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck ((==>))

spec :: Spec
spec = do
  describe "Ord" $ do
    prop "is transitive" $ \(x :: Party, y, z) ->
      x <= y && y <= z ==> x <= z
    prop "is reflexive" $ \(x :: Party) ->
      x <= x
    prop "is antisymmetric" $ \(x :: Party, y) ->
      (x <= y && y <= x) == (x == y)

  prop "implements Eq and Ord correspondingly" $ \(x :: Party, y) ->
    (compare x y == EQ) == (x == y)

  roundtripAndGoldenSpecs (Proxy @Party)
