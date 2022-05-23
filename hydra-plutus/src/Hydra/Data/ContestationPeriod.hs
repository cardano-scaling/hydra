{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Data.ContestationPeriod where

import Hydra.Prelude

import qualified PlutusTx.Prelude as Plutus

import Data.Ratio ((%))
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import Plutus.V1.Ledger.Time (DiffMilliSeconds, fromMilliSeconds)
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

posixToUTCTime :: POSIXTime -> UTCTime
posixToUTCTime (POSIXTime ms) =
  posixSecondsToUTCTime (fromInteger $ 1000 * ms)

millisInPico :: Integer
millisInPico = 10 ^ (9 :: Integer)

addContestationPeriod :: POSIXTime -> ContestationPeriod -> POSIXTime
addContestationPeriod time UnsafeContestationPeriod{milliseconds} = time Plutus.+ fromMilliSeconds milliseconds
{-# INLINEABLE addContestationPeriod #-}
