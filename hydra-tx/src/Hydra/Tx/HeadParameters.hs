module Hydra.Tx.HeadParameters where

import Hydra.Prelude

import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Party (Party (..))

-- | Contains the head's parameters as established in the initial transaction.
data HeadParameters = HeadParameters
  { contestationPeriod :: ContestationPeriod
  , parties :: [Party] -- NOTE(SN): The order of this list is important for leader selection.
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
