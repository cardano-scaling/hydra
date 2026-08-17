module Hydra.Node.ApiTransactionTimeout where

import Hydra.Prelude

-- | A new type wrapped period of time to be used in API transaction timeout.
newtype ApiTransactionTimeout = ApiTransactionTimeout
  { apiTransactionTimeoutNominalDiffTime :: NominalDiffTime
  }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Read, Num, Enum, Real, ToJSON, FromJSON)

instance ToCBOR ApiTransactionTimeout where
  toCBOR = genericToCBOR

instance FromCBOR ApiTransactionTimeout where
  fromCBOR = genericFromCBOR

-- | Truncates to whole seconds.
instance Integral ApiTransactionTimeout where
  quotRem (ApiTransactionTimeout a) (ApiTransactionTimeout b) = (ApiTransactionTimeout $ fromInteger q, ApiTransactionTimeout r)
   where
    (q, r) = properFraction (a / b)

  toInteger (ApiTransactionTimeout a) = round a
