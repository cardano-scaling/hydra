module Hydra.Contract.CommitError (
  errorCode,
  module Hydra.Contract.CommitError,
) where

import "base" Text.Show (Show)

import Hydra.Contract.Error (ToErrorCode (..), errorCode)

data CommitError
  = STNotBurnedError
  | STIsMissingInTheOutput
  deriving stock (Show)

instance ToErrorCode CommitError where
  toErrorCode = \case
    STNotBurnedError -> "C01"
    STIsMissingInTheOutput -> "C02"
