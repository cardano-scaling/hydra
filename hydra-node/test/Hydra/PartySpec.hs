module Hydra.PartySpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck ((==>))
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import "hydra-tx" Hydra.Tx.Party (Party (..))
import "hydra-tx" Test.Hydra.Tx.Fixture (alice, bob, carol)
import "hydra-tx" Test.Hydra.Tx.Gen ()

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

  it "has alice, bob, carol sorted" $
    sort [alice, bob, carol] `shouldBe` [alice, bob, carol]
