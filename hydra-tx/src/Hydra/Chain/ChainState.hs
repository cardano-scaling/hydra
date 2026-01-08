{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Chain.ChainState where

import Hydra.Cardano.Api (ChainPoint (..), SlotNo (..))
import Hydra.Prelude

import Hydra.Tx (IsTx (..))

-- | A generic description for a chain slot all implementations need to use.
newtype ChainSlot = ChainSlot Natural
  deriving stock (Ord, Eq, Show, Generic)
  deriving newtype (Num, ToJSON, FromJSON)

-- | Types that can be used on-chain by the Hydra protocol.
-- XXX: Find a better name for this. Maybe IsChainTx or IsL1Tx?
class
  ( IsTx tx
  , Eq (ChainStateType tx)
  , Show (ChainStateType tx)
  , FromJSON (ChainStateType tx)
  , ToJSON (ChainStateType tx)
  ) =>
  IsChainState tx
  where
  -- | Type of what to keep as L1 chain state.
  -- XXX: Why is this not always UTxOType?
  type ChainStateType tx = c | c -> tx

  -- | Get the chain point for a chain state.
  chainStatePoint :: ChainStateType tx -> ChainPoint

-- | Get the chain slot for a chain point.
chainPointSlot :: ChainPoint -> ChainSlot
chainPointSlot = \case
  ChainPointAtGenesis -> ChainSlot 0
  ChainPoint SlotNo{unSlotNo} _ -> ChainSlot $ fromIntegral unSlotNo

-- encountered, we assume monotonically increasing slots.
chainStateSlot :: IsChainState tx => ChainStateType tx -> ChainSlot
chainStateSlot = chainPointSlot . chainStatePoint
