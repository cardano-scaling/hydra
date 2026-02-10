module Hydra.Tx.HeadParameters where

import "hydra-prelude" Hydra.Prelude

import "hydra-tx" Hydra.Tx.ContestationPeriod (ContestationPeriod)
import "hydra-tx" Hydra.Tx.Party (Party (..))

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
