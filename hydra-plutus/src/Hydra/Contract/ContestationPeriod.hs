{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydra.Contract.ContestationPeriod where

import Hydra.Prelude

import qualified PlutusTx

newtype ContestationPeriod = UnsafeContestationPeriod {seconds :: Integer}
  deriving stock (Generic, Eq, Ord, Show)
  deriving newtype (Num, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''ContestationPeriod
