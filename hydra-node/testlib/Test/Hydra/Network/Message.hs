{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Message where

import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)
import "quickcheck-arbitrary-adt" Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

import Hydra.Network.Message (Message, NetworkEvent)
import Test.Hydra.Network ()

instance Arbitrary msg => Arbitrary (NetworkEvent msg) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => ToADTArbitrary (Message tx)
