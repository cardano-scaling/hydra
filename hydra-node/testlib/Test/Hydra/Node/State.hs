{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.State where

import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (recursivelyShrink)
import "hydra-node" Hydra.Node.State (Deposit, DepositStatus, NodeState, SyncedStatus)
import "hydra-node" Test.Hydra.HeadLogic.State ()
import "hydra-tx" Hydra.Chain.ChainState (IsChainState (..))
import "hydra-tx" Test.Hydra.Tx.Gen (ArbitraryIsTx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (NodeState tx) where
  arbitrary = genericArbitrary

instance Arbitrary SyncedStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

instance ArbitraryIsTx tx => Arbitrary (Deposit tx) where
  arbitrary = genericArbitrary
  shrink = recursivelyShrink

instance Arbitrary DepositStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink
