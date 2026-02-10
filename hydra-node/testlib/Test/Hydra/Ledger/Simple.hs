{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Simple where

import "hydra-test-utils" Test.Hydra.Prelude

import "hydra-node" Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx, SimpleTxOut (..))

import "hydra-tx" Test.Hydra.Tx.Gen ()

instance Arbitrary SimpleTx where
  arbitrary = genericArbitrary

deriving newtype instance Arbitrary SimpleTxOut

deriving newtype instance Arbitrary SimpleChainState
