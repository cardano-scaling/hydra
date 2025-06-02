module Hydra.Contract.DepositError (
  errorCode,
  module Hydra.Contract.DepositError,
) where

import Hydra.Contract.Error (ToErrorCode (..), errorCode)
import Text.Show (Show)

data DepositError
  = DepositPeriodSurpassed
  | DepositNoUpperBoundDefined
  | DepositNoLowerBoundDefined
  | DepositPeriodNotReached
  | IncorrectDepositHash
  | WrongHeadIdInDepositDatum
  deriving stock (Show)

instance ToErrorCode DepositError where
  toErrorCode = \case
    DepositPeriodSurpassed -> "D01"
    DepositNoUpperBoundDefined -> "D02"
    DepositNoLowerBoundDefined -> "D03"
    DepositPeriodNotReached -> "D04"
    IncorrectDepositHash -> "D05"
    WrongHeadIdInDepositDatum -> "D06"
