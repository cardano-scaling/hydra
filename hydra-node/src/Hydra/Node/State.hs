{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State where

import Hydra.Prelude

import Data.Map qualified as Map
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.HeadLogic.State (HeadState (Idle), IdleState (..))
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )
import Hydra.Tx.Accumulator (HasAccumulatorElement)
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Test.QuickCheck (recursivelyShrink)

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

data NodeState tx = NodeState
  { headState :: HeadState tx
  , pendingDeposits :: PendingDeposits tx
  -- ^ Pending deposits as observed on chain.
  -- TODO: could even move the chain state here (also see todo below)
  -- , chainState :: ChainStateType tx
  , currentSlot :: ChainSlot
  }
  deriving stock (Generic)

instance (ArbitraryIsTx tx, HasAccumulatorElement tx, Arbitrary (ChainStateType tx)) => Arbitrary (NodeState tx) where
  arbitrary = genericArbitrary

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx)

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeState
    { headState = Idle IdleState{chainState}
    , pendingDeposits = mempty
    , currentSlot = chainStateSlot chainState
    }

-- | A deposit tracked by the protocol. The 'DepositStatus' determines whether
-- it may be used for an incremental commit or not.
data Deposit tx = Deposit
  { headId :: HeadId
  , deposited :: UTxOType tx
  , created :: UTCTime
  , deadline :: UTCTime
  , status :: DepositStatus
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (Deposit tx)
deriving stock instance IsTx tx => Show (Deposit tx)
deriving anyclass instance IsTx tx => ToJSON (Deposit tx)
deriving anyclass instance IsTx tx => FromJSON (Deposit tx)

instance ArbitraryIsTx tx => Arbitrary (Deposit tx) where
  arbitrary = genericArbitrary
  shrink = recursivelyShrink

data DepositStatus = Inactive | Active | Expired
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

instance Arbitrary DepositStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

depositsForHead :: HeadId -> PendingDeposits tx -> PendingDeposits tx
depositsForHead targetHeadId =
  Map.filter (\Deposit{headId} -> headId == targetHeadId)
