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

instance Arbitrary ContestationPeriod where
  shrink = genericShrink
  arbitrary = genericArbitrary

instance FromJSON ContestationPeriod where
  parseJSON =
    fmap (UnsafeContestationPeriod . diffTimeToPicoseconds) . parseJSON

instance ToJSON ContestationPeriod where
  toJSON =
    toJSON . picosecondsToDiffTime . picoseconds

contestationPeriodFromDiffTime :: DiffTime -> ContestationPeriod
contestationPeriodFromDiffTime = UnsafeContestationPeriod . diffTimeToPicoseconds
