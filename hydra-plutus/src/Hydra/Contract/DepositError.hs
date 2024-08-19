module Hydra.Contract.DepositError (
  errorCode,
  module Hydra.Contract.DepositError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Text.Show (Show)

data DepositError
  = DepositDeadlineSurpassed
  | DepositNoUpperBoundDefined
  deriving stock (Show)

instance ToErrorCode DepositError where
  toErrorCode = \case
    DepositDeadlineSurpassed -> "D01"
    DepositNoUpperBoundDefined -> "D02"
