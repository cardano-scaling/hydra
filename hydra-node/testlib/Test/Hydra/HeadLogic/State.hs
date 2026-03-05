{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.State where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.State (ClosedState (..), CoordinatedHeadState (..), HeadState (..), IdleState (..), InitialState (..), OpenState (..), SeenSnapshot (..))
import Test.Hydra.Tx.Gen (ArbitraryIsTx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

instance Arbitrary (ChainStateType tx) => Arbitrary (IdleState tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (InitialState tx) where
  arbitrary = do
    InitialState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (OpenState tx) where
  arbitrary =
    OpenState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ArbitraryIsTx tx => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ClosedState tx) where
  arbitrary =
    ClosedState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
