{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Message where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Network.Message (Message, NetworkEvent)
import "hydra-node" Test.Hydra.Network ()
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)
import "quickcheck-arbitrary-adt" Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

instance Arbitrary msg => Arbitrary (NetworkEvent msg) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => ToADTArbitrary (Message tx)
