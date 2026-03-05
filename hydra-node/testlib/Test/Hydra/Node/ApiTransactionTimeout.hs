{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.ApiTransactionTimeout where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Node.ApiTransactionTimeout

import Test.QuickCheck (choose)

instance Arbitrary ApiTransactionTimeout where
  arbitrary = ApiTransactionTimeout . fromInteger <$> choose (0, 86400)
