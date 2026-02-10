{-# LANGUAGE TemplateHaskell #-}

module Hydra.Data.ContestationPeriod where

import "hydra-prelude" Hydra.Prelude
import "plutus-tx" PlutusTx.Prelude qualified as Plutus
import "base" Data.Ratio ((%))
import "plutus-ledger-api" PlutusLedgerApi.V1.Time (DiffMilliSeconds, fromMilliSeconds)
import "plutus-ledger-api" PlutusLedgerApi.V3 (POSIXTime (..))
import "plutus-tx" PlutusTx qualified
import "time" Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)

newtype ContestationPeriod = UnsafeContestationPeriod {milliseconds :: DiffMilliSeconds}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, Plutus.Eq)

PlutusTx.unstableMakeIsData ''ContestationPeriod

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
