{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hydra.Contract.ContestationPeriod where

import Hydra.Prelude

import Data.Time.Clock (
  diffTimeToPicoseconds,
  picosecondsToDiffTime,
 )

newtype ContestationPeriod = ContestationPeriod
  {unsafeContestationPeriod :: Integer}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, FromJSON, ToJSON)

fromDiffTime :: DiffTime -> ContestationPeriod
fromDiffTime = ContestationPeriod . diffTimeToPicoseconds

toDiffTime :: ContestationPeriod -> DiffTime
toDiffTime = picosecondsToDiffTime . unsafeContestationPeriod
