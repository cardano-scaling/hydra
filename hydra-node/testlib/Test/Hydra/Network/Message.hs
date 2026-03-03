{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Network.Message where

import Test.Hydra.Prelude

import Hydra.Chain.ChainState (ChainPointType)
import Hydra.Network.Message (Message, NetworkEvent)
import Test.Hydra.Network ()
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck.Arbitrary.ADT (ToADTArbitrary)

instance Arbitrary msg => Arbitrary (NetworkEvent msg) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainPointType tx)) => Arbitrary (Message tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainPointType tx)) => ToADTArbitrary (Message tx)
