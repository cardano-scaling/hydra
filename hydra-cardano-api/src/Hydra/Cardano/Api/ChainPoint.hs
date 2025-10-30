{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Test.Gen.Cardano.Api.Typed (genChainPoint)
import Test.QuickCheck.Hedgehog (hedgehog)

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- * Orphans

instance Arbitrary ChainPoint where
  arbitrary = hedgehog genChainPoint
