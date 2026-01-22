{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Simple where

import Test.Hydra.Prelude

import Hydra.Ledger.Simple (SimpleChainPoint (..), SimpleChainState (..), SimpleTx, SimpleTxOut (..))

import Test.Hydra.Tx.Gen ()

instance Arbitrary SimpleTx where
  arbitrary = genericArbitrary

deriving newtype instance Arbitrary SimpleTxOut

deriving newtype instance Arbitrary SimpleChainState

instance Arbitrary SimpleChainPoint where
  arbitrary = genericArbitrary
