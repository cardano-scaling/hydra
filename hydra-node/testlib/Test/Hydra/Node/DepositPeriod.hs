{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.DepositPeriod where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.DepositPeriod
import Test.QuickCheck (choose)

instance Arbitrary DepositPeriod where
  arbitrary = DepositPeriod . fromInteger <$> choose (1, 86400)
