{-# LANGUAGE TemplateHaskell #-}

module Hydra.Data.ContestationPeriod where

import Hydra.Prelude

import PlutusTx.Prelude qualified as Plutus

import Data.Ratio ((%))
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds, fromMilliSeconds)
import PlutusLedgerApi.V3 (POSIXTime (..))
import PlutusTx qualified
import Test.QuickCheck (Arbitrary (..))

newtype ContestationPeriod = UnsafeContestationPeriod {milliseconds :: DiffMilliSeconds}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, Plutus.Eq)

PlutusTx.unstableMakeIsData ''ContestationPeriod

instance Arbitrary ContestationPeriod where
  arbitrary = fromInteger <$> arbitrary

contestationPeriodFromDiffTime :: NominalDiffTime -> ContestationPeriod
contestationPeriodFromDiffTime = UnsafeContestationPeriod . truncate . (* 1000) . nominalDiffTimeToSeconds

contestationPeriodToDiffTime :: ContestationPeriod -> NominalDiffTime
contestationPeriodToDiffTime cp =
  secondsToNominalDiffTime $ fromRational (toInteger (milliseconds cp) % 1000)

-- | Compute the (on-chain) contestation deadline from a given current time and
-- the 'ContestationPeriod'.
addContestationPeriod :: POSIXTime -> ContestationPeriod -> POSIXTime
addContestationPeriod time UnsafeContestationPeriod{milliseconds} = time Plutus.+ fromMilliSeconds milliseconds
{-# INLINEABLE addContestationPeriod #-}
