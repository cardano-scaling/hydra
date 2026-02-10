{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.Environment where

import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Test.Hydra.Tx.Gen ()
import "quickcheck-instances" Test.QuickCheck.Instances.Text ()

import Hydra.Node.Environment (Environment)
import Test.Hydra.Node.DepositPeriod ()
import Test.Hydra.Node.UnsyncedPeriod ()

instance Arbitrary Environment where
  arbitrary = genericArbitrary
  shrink = genericShrink
