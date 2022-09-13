{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Data.ContestationPeriod where

import Hydra.Prelude

import qualified PlutusTx.Prelude as Plutus

import Data.Fixed (Pico)
import Data.Ratio ((%))
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, fromMilliSeconds)
import Plutus.V2.Ledger.Api (POSIXTime (..))
import qualified PlutusTx

newtype ContestationPeriod = UnsafeContestationPeriod {milliseconds :: DiffMilliSeconds}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num)

PlutusTx.unstableMakeIsData ''ContestationPeriod

instance Arbitrary ContestationPeriod where
  arbitrary = fromInteger <$> arbitrary

instance FromJSON ContestationPeriod where
  parseJSON =
    fmap (UnsafeContestationPeriod . fromInteger) . parseJSON

instance ToJSON ContestationPeriod where
  toJSON =
    toJSON . toInteger . milliseconds

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

-- * Converting to/from time on-chain

-- | Convert given on-chain 'POSIXTime' to a 'UTCTime'.
posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime (POSIXTime ms) =
  -- NOTE: POSIXTime records the number of milliseconds since epoch
  posixSecondsToUTCTime (fromRational $ ms % 1000)

-- | Compute on-chain 'POSIXTime' from a given 'UTCTime'.
posixFromUTCTime :: UTCTime -> POSIXTime
posixFromUTCTime utcTime =
  -- NOTE: POSIXTime records the number of milliseconds since epoch
  POSIXTime . truncate $ posixSeconds * 1000
 where
  -- NOTE: 'Pico' is a 'Fixed' precision integer and denotes here the seconds
  -- since epoch with picosecond precision.
  posixSeconds :: Pico
  posixSeconds = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utcTime
