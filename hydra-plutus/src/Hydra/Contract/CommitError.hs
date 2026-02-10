module Hydra.Contract.CommitError (
  errorCode,
  module Hydra.Contract.CommitError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import "base" Text.Show (Show)

data CommitError
  = STNotBurnedError
  | STIsMissingInTheOutput
  deriving stock (Show)

instance ToErrorCode CommitError where
  toErrorCode = \case
    STNotBurnedError -> "C01"
    STIsMissingInTheOutput -> "C02"
