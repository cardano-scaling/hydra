{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

import Cardano.Api (BlockHeader (..), ChainPoint (..), SlotNo (..))
import Hydra.Cardano.Api.BlockHeader (genBlockHeaderHash)
import Test.QuickCheck (frequency)

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- * Generators

-- | Generate a chain point with a likely invalid block header hash.
genChainPoint :: Gen ChainPoint
genChainPoint =
  frequency
    [ (1, pure ChainPointAtGenesis)
    , (5, arbitrary >>= genChainPointAt . SlotNo)
    ]

-- | Generate a chain point at given slot with a likely invalid block header hash.
genChainPointAt :: SlotNo -> Gen ChainPoint
genChainPointAt s =
  ChainPoint s <$> genBlockHeaderHash

-- * Orphans

instance Arbitrary ChainPoint where
  arbitrary = genChainPoint
