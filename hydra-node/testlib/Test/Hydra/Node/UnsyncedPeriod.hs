{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.UnsyncedPeriod where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.UnsyncedPeriod
import Test.QuickCheck (choose)

instance Arbitrary UnsyncedPeriod where
  arbitrary = UnsyncedPeriod . fromInteger <$> choose (1, 86400)
