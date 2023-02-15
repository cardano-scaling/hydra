{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Chain (
  module API,
  OnChainTx (..),
  Chain (..),
  ChainEvent (..),
  ChainCallback,
  ChainComponent,
) where

import Hydra.Prelude

import Hydra.API.Chain as API
import Hydra.API.ContestationPeriod (ContestationPeriod)
import Hydra.API.Ledger (IsTx, TxIdType, UTxOType)
import Hydra.API.Party (Party)
import Hydra.API.Snapshot (SnapshotNumber)
import Test.QuickCheck.Instances.Time ()

-- | Describes transactions as seen on chain. Holds as minimal information as
-- possible to simplify observing the chain.
data OnChainTx tx
  = OnInitTx {headId :: HeadId, contestationPeriod :: ContestationPeriod, parties :: [Party]}
  | OnCommitTx {party :: Party, committed :: UTxOType tx}
  | OnAbortTx
  | OnCollectComTx
  | OnCloseTx
      { snapshotNumber :: SnapshotNumber
      , contestationDeadline :: UTCTime
      }
  | OnContestTx {snapshotNumber :: SnapshotNumber}
  | OnFanoutTx
  deriving (Generic)

deriving instance IsTx tx => Eq (OnChainTx tx)
deriving instance IsTx tx => Show (OnChainTx tx)
deriving instance IsTx tx => ToJSON (OnChainTx tx)
deriving instance IsTx tx => FromJSON (OnChainTx tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

-- | Handle to interface with the main chain network
newtype Chain tx m = Chain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'PostChainTx' description and the current 'ChainState'. This
    -- function is not expected to block, so it is only responsible for
    -- submitting, but it should validate the created transaction against a
    -- reasonable local view of the chain and throw an exception when invalid.
    --
    -- Does at least throw 'PostTxError'.
    postTx :: (IsChainState tx, MonadThrow m) => ChainStateType tx -> PostChainTx tx -> m ()
  }

data ChainEvent tx
  = Observation
      { observedTx :: OnChainTx tx
      , newChainState :: ChainStateType tx
      }
  | Rollback ChainSlot
  | Tick UTCTime
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ChainEvent tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (ChainEvent tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ChainEvent tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ChainEvent tx)

instance
  ( Arbitrary tx
  , Arbitrary (UTxOType tx)
  , Arbitrary (TxIdType tx)
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ChainEvent tx)
  where
  arbitrary = genericArbitrary

-- | A callback indicating receival of a potential Hydra transaction which is Maybe
-- observing a relevant 'ChainEvent tx'.
type ChainCallback tx m = (ChainStateType tx -> Maybe (ChainEvent tx)) -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
