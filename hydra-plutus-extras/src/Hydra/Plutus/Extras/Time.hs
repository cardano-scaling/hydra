-- |  Converting to/from time on-chain
--
-- XXX: Ideally, these functions would be upstreamed into plutus directly.
module Hydra.Plutus.Extras.Time where

import "hydra-prelude" Hydra.Prelude

import "base" Data.Fixed (Pico)
import "base" Data.Ratio ((%))
import "plutus-ledger-api" PlutusLedgerApi.V1.Time qualified as Plutus
import "time" Data.Time (nominalDiffTimeToSeconds)
import "time" Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | Convert given on-chain 'POSIXTime' to a 'UTCTime'.
posixToUTCTime :: Plutus.POSIXTime -> UTCTime
posixToUTCTime (Plutus.POSIXTime ms) =
  -- NOTE: POSIXTime records the number of milliseconds since epoch
  posixSecondsToUTCTime (fromRational $ ms % 1000)

-- | Compute on-chain 'POSIXTime' from a given 'UTCTime'.
posixFromUTCTime :: UTCTime -> Plutus.POSIXTime
posixFromUTCTime utcTime =
  -- NOTE: POSIXTime records the number of milliseconds since epoch
  Plutus.POSIXTime . truncate $ posixSeconds * 1000
 where
  -- NOTE: 'Pico' is a 'Fixed' precision integer and denotes here the seconds
  -- since epoch with picosecond precision.
  posixSeconds :: Pico
  posixSeconds = nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds utcTime
