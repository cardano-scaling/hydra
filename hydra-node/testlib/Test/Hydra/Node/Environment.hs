{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.Environment where

import Test.Hydra.Prelude

import Hydra.Node.Environment (Environment)
import Test.Hydra.Node.DepositPeriod ()
import Test.Hydra.Node.UnsyncedPeriod ()
import Test.Hydra.Tx.Gen ()
import Test.QuickCheck.Instances.Text ()

instance Arbitrary Environment where
  arbitrary = genericArbitrary
  shrink = genericShrink
