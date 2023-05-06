{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Cardano.Api.ChainPoint where

import Hydra.Cardano.Api.Prelude

-- | Get the chain point corresponding to a given 'BlockHeader'.
getChainPoint :: BlockHeader -> ChainPoint
getChainPoint header =
  ChainPoint slotNo headerHash
 where
  (BlockHeader slotNo headerHash _) = header

-- | Get the chain SlotNo corresponding to a given 'BlockHeader'.
getChainSlotNo :: BlockHeader -> SlotNo
getChainSlotNo header = slotNo
 where
  (BlockHeader slotNo _ _) = header

-- * Orphans

-- XXX: Incomplete arbitrary instance
instance Arbitrary ChainPoint where
  arbitrary =
    pure ChainPointAtGenesis
