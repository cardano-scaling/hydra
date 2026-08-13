{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.Ledger.Simple where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), SimpleTxOut (..))
import Hydra.Tx.IsTx (UTxOType)

import Data.Set qualified as Set
import Test.Hydra.Tx.Gen ()

instance Arbitrary SimpleTx where
  arbitrary = genericArbitrary

deriving newtype instance Arbitrary SimpleTxOut

deriving newtype instance Arbitrary SimpleChainState

-- * Builders

utxoRef :: Integer -> UTxOType SimpleTx
utxoRef = Set.singleton . SimpleTxOut

utxoRefs :: [Integer] -> UTxOType SimpleTx
utxoRefs = Set.fromList . fmap SimpleTxOut

aValidTx :: Integer -> SimpleTx
aValidTx n = SimpleTx n mempty (utxoRef n)
