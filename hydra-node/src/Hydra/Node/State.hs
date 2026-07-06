{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.State (HeadState (Idle), IdleState (..))
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

data ChainPointTime = ChainPointTime
  { currentSlot :: ChainSlot
  -- ^ Latest chain slot as observed on chain.
  , currentChainTime :: UTCTime
  -- ^ Time corresponding to `currentSlot`.
  , drift :: NominalDiffTime
  -- ^ Time difference with current system wall-clock measured in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ChainPointTime where
  toCBOR ChainPointTime{currentSlot, currentChainTime, drift} =
    toCBOR currentSlot <> toCBOR currentChainTime <> toCBOR drift

instance FromCBOR ChainPointTime where
  fromCBOR = ChainPointTime <$> fromCBOR <*> fromCBOR <*> fromCBOR

data NodeState tx
  = -- | Normal operation of the node where it is connected and has a recent
    -- view of the chain.
    NodeInSync
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      -- ^ Pending deposits as observed on chain.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  | -- | Node is catching up on its view of the chain and should behave
    -- differently.
    NodeCatchingUp
      { headState :: HeadState tx
      , pendingDeposits :: PendingDeposits tx
      -- ^ Pending deposits as observed on chain.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx)

instance IsChainState tx => ToCBOR (NodeState tx) where
  toCBOR = \case
    NodeInSync{headState, pendingDeposits, chainPointTime} ->
      toCBOR ("NodeInSync" :: Text)
        <> toCBOR headState
        <> toCBOR pendingDeposits
        <> toCBOR chainPointTime
    NodeCatchingUp{headState, pendingDeposits, chainPointTime} ->
      toCBOR ("NodeCatchingUp" :: Text)
        <> toCBOR headState
        <> toCBOR pendingDeposits
        <> toCBOR chainPointTime

instance IsChainState tx => FromCBOR (NodeState tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NodeInSync" :: Text) -> NodeInSync <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "NodeCatchingUp" -> NodeCatchingUp <$> fromCBOR <*> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded NodeState"

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeCatchingUp
    { headState = Idle IdleState{chainState}
    , pendingDeposits = mempty
    , chainPointTime = initialChainPointTime chainState
    }

initialChainPointTime :: IsChainState tx => ChainStateType tx -> ChainPointTime
initialChainPointTime chainState =
  ChainPointTime
    { currentSlot = chainStateSlot chainState
    , currentChainTime = initialChainTime
    , drift = 0
    }

initialChainTime :: UTCTime
initialChainTime = posixSecondsToUTCTime 0

data SyncedStatus = InSync | CatchingUp
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR SyncedStatus where
  toCBOR = \case
    InSync -> toCBOR ("InSync" :: Text)
    CatchingUp -> toCBOR ("CatchingUp" :: Text)

instance FromCBOR SyncedStatus where
  fromCBOR =
    fromCBOR >>= \case
      ("InSync" :: Text) -> pure InSync
      "CatchingUp" -> pure CatchingUp
      tag -> fail $ show tag <> " is not a proper CBOR-encoded SyncedStatus"

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
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Deposit tx)
deriving stock instance IsTx tx => Show (Deposit tx)
deriving anyclass instance IsTx tx => ToJSON (Deposit tx)
deriving anyclass instance IsTx tx => FromJSON (Deposit tx)

instance IsTx tx => ToCBOR (Deposit tx) where
  toCBOR Deposit{headId, deposited, created, deadline, status} =
    toCBOR headId
      <> toCBOR deposited
      <> toCBOR created
      <> toCBOR deadline
      <> toCBOR status

instance IsTx tx => FromCBOR (Deposit tx) where
  fromCBOR = Deposit <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

data DepositStatus = Inactive | Active | Expired
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR DepositStatus where
  toCBOR = \case
    Inactive -> toCBOR ("Inactive" :: Text)
    Active -> toCBOR ("Active" :: Text)
    Expired -> toCBOR ("Expired" :: Text)

instance FromCBOR DepositStatus where
  fromCBOR =
    fromCBOR >>= \case
      ("Inactive" :: Text) -> pure Inactive
      "Active" -> pure Active
      "Expired" -> pure Expired
      tag -> fail $ show tag <> " is not a proper CBOR-encoded DepositStatus"

depositsForHead :: HeadId -> PendingDeposits tx -> PendingDeposits tx
depositsForHead targetHeadId =
  Map.filter (\Deposit{headId} -> headId == targetHeadId)
