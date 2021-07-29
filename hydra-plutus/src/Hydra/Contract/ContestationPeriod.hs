{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Contract.ContestationPeriod where

import Hydra.Prelude

import Data.Time.Clock (
  diffTimeToPicoseconds,
  picosecondsToDiffTime,
 )
import qualified PlutusTx

newtype ContestationPeriod = ContestationPeriod
  {unsafeContestationPeriod :: Integer}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''ContestationPeriod

fromDiffTime :: DiffTime -> ContestationPeriod
fromDiffTime = ContestationPeriod . diffTimeToPicoseconds

toDiffTime :: ContestationPeriod -> DiffTime
toDiffTime = picosecondsToDiffTime . unsafeContestationPeriod
