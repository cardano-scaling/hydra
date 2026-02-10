{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.UnsyncedPeriod where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "QuickCheck" Test.QuickCheck (choose)
import "hydra-node" Hydra.Node.UnsyncedPeriod

instance Arbitrary UnsyncedPeriod where
  arbitrary = UnsyncedPeriod . fromInteger <$> choose (1, 86400)
