module Hydra.Node.DepositPeriod where

import "hydra-prelude" Hydra.Prelude

-- | A new type wrapped period of time to be used in deposit validity.
newtype DepositPeriod = DepositPeriod {toNominalDiffTime :: NominalDiffTime}
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num, Enum, Real, ToJSON, FromJSON)

-- | Truncates to whole seconds.
instance Integral DepositPeriod where
  quotRem (DepositPeriod a) (DepositPeriod b) = (DepositPeriod $ fromInteger q, DepositPeriod r)
   where
    (q, r) = properFraction (a / b)

  toInteger (DepositPeriod a) = round a
