{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State where

import Hydra.Prelude

import Data.Map qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.State (HeadState (Idle), IdleState (..))
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

data NodeState tx
  = -- | Normal operation of the node where it is connected and has a recent
    -- view of the chain.
    NodeInSync
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      -- ^ Pending deposits as observed on chain.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , currentSlot :: ChainSlot
      -- ^ Latest chain slot as observed on chain.
      , currentChainTime :: UTCTime
      -- ^ Latest chain slot time representation as observed on chain.
      }
  | -- | Node is catching up on its view of the chain and should behave
    -- differently.
    NodeCatchingUp
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      , currentSlot :: ChainSlot
      , currentChainTime :: UTCTime
      }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx)

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeCatchingUp
    { headState = Idle IdleState{chainState}
    , pendingDeposits = mempty
    , currentSlot = chainStateSlot chainState
    , currentChainTime = initialChainTime
    }

initialChainTime :: UTCTime
initialChainTime = posixSecondsToUTCTime 0

data SyncedStatus = InSync | CatchingUp
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

syncedStatus :: NodeState tx -> SyncedStatus
syncedStatus NodeInSync{} = InSync
syncedStatus NodeCatchingUp{} = CatchingUp

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

data DepositStatus = Inactive | Active | Expired
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

depositsForHead :: HeadId -> PendingDeposits tx -> PendingDeposits tx
depositsForHead targetHeadId =
  Map.filter (\Deposit{headId} -> headId == targetHeadId)
