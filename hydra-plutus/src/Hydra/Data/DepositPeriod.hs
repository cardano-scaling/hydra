{-# LANGUAGE TemplateHaskell #-}

module Hydra.Data.DepositPeriod where

import Hydra.Prelude

import PlutusTx.Prelude qualified as Plutus

import Data.Ratio ((%))
import Data.Time (nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds)
import PlutusTx qualified

newtype DepositPeriod = UnsafeDepositPeriod {milliseconds :: DiffMilliSeconds}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, Plutus.Eq)

PlutusTx.unstableMakeIsData ''DepositPeriod

depositPeriodFromDiffTime :: NominalDiffTime -> DepositPeriod
depositPeriodFromDiffTime = UnsafeDepositPeriod . truncate . (* 1000) . nominalDiffTimeToSeconds

depositPeriodToDiffTime :: DepositPeriod -> NominalDiffTime
depositPeriodToDiffTime dp =
  secondsToNominalDiffTime $ fromRational (toInteger (milliseconds dp) % 1000)
