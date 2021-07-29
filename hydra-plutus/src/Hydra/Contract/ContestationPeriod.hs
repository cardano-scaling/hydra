{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Contract.ContestationPeriod where

import Hydra.Prelude

import Data.Time (diffTimeToPicoseconds, picosecondsToDiffTime)
import qualified PlutusTx

newtype ContestationPeriod = UnsafeContestationPeriod {picoseconds :: Integer}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num)

PlutusTx.unstableMakeIsData ''ContestationPeriod

instance FromJSON ContestationPeriod where
  parseJSON =
    fmap (UnsafeContestationPeriod . diffTimeToPicoseconds) . parseJSON

instance ToJSON ContestationPeriod where
  toJSON =
    toJSON . picosecondsToDiffTime . picoseconds
