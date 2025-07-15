-- | Structured errors related to configuration mismatch.
--
-- When we start a `Hydra.Node` we need to do sanity checks between what's
-- provided as parameters to the node and what's persisted.
module Hydra.Node.ParameterMismatch where

import Hydra.Prelude

import Hydra.Tx (Party)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)

-- | Exception used to indicate command line options not matching the persisted
-- state.
newtype ParameterMismatch = ParameterMismatch [ParamMismatch]
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

data ParamMismatch
  = ContestationPeriodMismatch {loadedCp :: ContestationPeriod, configuredCp :: ContestationPeriod}
  | PartiesMismatch {loadedParties :: [Party], configuredParties :: [Party]}
  | SavedNetworkPartiesInconsistent {numberOfParties :: Int}
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)
