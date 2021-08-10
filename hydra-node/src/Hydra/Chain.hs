{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Chain where

import Cardano.Prelude
import Control.Monad.Class.MonadThrow (MonadThrow)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time (DiffTime)
import Hydra.Ledger (Party, Tx, UTxO)
import Hydra.Prelude (Arbitrary (arbitrary), genericArbitrary)
import Hydra.Snapshot (Snapshot)

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: DiffTime
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Read, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary HeadParameters where
  arbitrary = genericArbitrary

type ContestationPeriod = DiffTime

-- NOTE(SN): Might not be symmetric in a real chain client, i.e. posting
-- transactions could be parameterized using such data types, but they are not
-- fully recoverable from transactions observed on chain
-- REVIEW(SN): There is a similarly named type in plutus-ledger, so we might
-- want to rename this
data OnChainTx tx
  = InitTx HeadParameters
  | CommitTx Party (UTxO tx)
  | AbortTx (UTxO tx)
  | CollectComTx (UTxO tx)
  | CloseTx (Snapshot tx)
  | ContestTx (Snapshot tx)
  | FanoutTx (UTxO tx)
  deriving stock (Generic)

deriving instance Tx tx => Eq (OnChainTx tx)
deriving instance Tx tx => Show (OnChainTx tx)
deriving instance Tx tx => Read (OnChainTx tx)
deriving instance Tx tx => ToJSON (OnChainTx tx)
deriving instance Tx tx => FromJSON (OnChainTx tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype Chain tx m = Chain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx tx -> m ()
  }

-- | Handle to interface observed transactions.
type ChainCallback tx m = OnChainTx tx -> m ()

-- | A type tying both posting and observing transactions into a single /Component/.
type ChainComponent tx m a = ChainCallback tx m -> (Chain tx m -> m a) -> m a
