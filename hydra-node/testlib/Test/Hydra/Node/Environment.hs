{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.Environment where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Node.Environment (Environment)
import "hydra-node" Test.Hydra.Node.DepositPeriod ()
import "hydra-node" Test.Hydra.Node.UnsyncedPeriod ()
import "hydra-tx" Test.Hydra.Tx.Gen ()
import "quickcheck-instances" Test.QuickCheck.Instances.Text ()

instance Arbitrary Environment where
  arbitrary = genericArbitrary
  shrink = genericShrink
