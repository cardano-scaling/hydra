module Hydra.Node.ApiTransactionTimeout where

import "hydra-prelude" Hydra.Prelude

-- | A new type wrapped period of time to be used in API transaction timeout.
newtype ApiTransactionTimeout = ApiTransactionTimeout
  { apiTransactionTimeoutNominalDiffTime :: NominalDiffTime
  }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, Num, Enum, Real, ToJSON, FromJSON)

-- | Truncates to whole seconds.
instance Integral ApiTransactionTimeout where
  quotRem (ApiTransactionTimeout a) (ApiTransactionTimeout b) = (ApiTransactionTimeout $ fromInteger q, ApiTransactionTimeout r)
   where
    (q, r) = properFraction (a / b)

  toInteger (ApiTransactionTimeout a) = round a
