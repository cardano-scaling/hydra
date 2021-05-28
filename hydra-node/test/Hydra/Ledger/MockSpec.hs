{-# LANGUAGE TypeApplications #-}

module Hydra.Ledger.MockSpec where

import Cardano.Prelude
import Hydra.Ledger.Mock (MockTx (..))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, counterexample, forAll, frequency, property)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Property (Property)

spec :: Spec
spec = describe "Mock Ledger" $ do
  it "can readh/show MockTx" $ property $ prop_canReadAndShow genMockTx

genMockTx :: Gen MockTx
genMockTx = frequency [(10, ValidTx <$> arbitrary), (1, pure InvalidTx)]

prop_canReadAndShow :: (Read a, Show a, Eq a) => Gen a -> Property
prop_canReadAndShow genA =
  forAll genA $ \a ->
    let shown = show a
        msg = shown <> " is not readable"
     in counterexample msg $ readMaybe shown == Just a
