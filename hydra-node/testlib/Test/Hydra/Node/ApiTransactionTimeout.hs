{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Node.ApiTransactionTimeout where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Node.ApiTransactionTimeout

import "QuickCheck" Test.QuickCheck (choose)

instance Arbitrary ApiTransactionTimeout where
  arbitrary = ApiTransactionTimeout . fromInteger <$> choose (0, 86400)
