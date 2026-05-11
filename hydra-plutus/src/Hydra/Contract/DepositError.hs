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
  | DepositHeadInputNotFound
  | HeadInputRedeemerNotFound
  | HeadRedeemerNotIncrement
  deriving stock (Show)

instance ToErrorCode DepositError where
  toErrorCode = \case
    DepositPeriodSurpassed -> "D01"
    DepositNoUpperBoundDefined -> "D02"
    DepositNoLowerBoundDefined -> "D03"
    DepositPeriodNotReached -> "D04"
    IncorrectDepositHash -> "D05"
    DepositHeadInputNotFound -> "D06"
    HeadInputRedeemerNotFound -> "D07"
    HeadRedeemerNotIncrement -> "D08"
