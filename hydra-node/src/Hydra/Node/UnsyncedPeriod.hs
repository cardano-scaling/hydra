module Hydra.Node.UnsyncedPeriod where

import Hydra.Prelude

import Hydra.Tx.ContestationPeriod (ContestationPeriod, toNominalDiffTime)

-- | Period of time after which we consider the node becoming unsynced with the chain.
-- Beyond this period the node will refuse to process new transactions and signing snapshots.
newtype UnsyncedPeriod = UnsyncedPeriod {unsyncedPeriodToNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Read, Num, Real, ToJSON, FromJSON)

instance ToCBOR UnsyncedPeriod where
  toCBOR = genericToCBOR

instance FromCBOR UnsyncedPeriod where
  fromCBOR = genericFromCBOR

-- | Compute a default 'UnsyncedPeriod' based on the 'ContestationPeriod'.
-- This is the legacy behavior: half of the contestation period.
defaultUnsyncedPeriodFor :: ContestationPeriod -> UnsyncedPeriod
defaultUnsyncedPeriodFor cp = UnsyncedPeriod $ toNominalDiffTime cp * 0.5
