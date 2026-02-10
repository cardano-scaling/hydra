{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.DepositPeriod where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (choose)
import "hydra-node" Hydra.Node.DepositPeriod

instance Arbitrary DepositPeriod where
  arbitrary = DepositPeriod . fromInteger <$> choose (1, 86400)
