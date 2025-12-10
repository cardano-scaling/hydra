{-# LANGUAGE TypeFamilyDependencies #-}

module Hydra.Chain.ChainState where

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
  , Arbitrary (ChainStateType tx)
  , Eq (ChainPointType tx)
  , Show (ChainPointType tx)
  , FromJSON (ChainPointType tx)
  , ToJSON (ChainPointType tx)
  , Arbitrary (ChainPointType tx)
  ) =>
  IsChainState tx
  where
  -- | Type of what to keep as L1 chain state.
  -- XXX: Why is this not always UTxOType?
  type ChainStateType tx = c | c -> tx

  -- | Type of what to keep as L1 chain point.
  type ChainPointType tx = c | c -> tx

  -- | Get the chain point for a chain state.
  chainStatePoint :: ChainStateType tx -> ChainPointType tx

  -- | Get the chain slot for a chain point.
  chainPointSlot :: ChainPointType tx -> ChainSlot

  -- | The chain point genesis.
  genesisPoint :: ChainPointType tx

-- | Get the chain slot for a chain state. NOTE: For any sequence of 'a'
-- encountered, we assume monotonically increasing slots.
chainStateSlot :: IsChainState tx => ChainStateType tx -> ChainSlot
chainStateSlot = chainPointSlot . chainStatePoint
