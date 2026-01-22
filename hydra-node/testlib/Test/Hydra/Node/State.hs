{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.State where

import Test.Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.Node.State (Deposit, DepositStatus, NodeState, SyncedStatus)
import Test.Hydra.HeadLogic.State ()
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck (recursivelyShrink)

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
