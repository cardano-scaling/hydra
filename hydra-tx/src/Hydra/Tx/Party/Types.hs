module Hydra.Tx.Party.Types where

import Data.Aeson (FromJSONKey, ToJSONKey)
import Hydra.Prelude
import Hydra.Tx.Crypto (HydraKey, VerificationKey, verificationKeyHash)

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party = Party {vkey :: VerificationKey HydraKey}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (ToJSONKey, FromJSONKey)

-- REVIEW: Do we really want to define Ord or also use unordered-containers
-- based on Hashable?
instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    verificationKeyHash a <= verificationKeyHash b

instance FromCBOR Party where
  fromCBOR = genericFromCBOR

instance ToCBOR Party where
  toCBOR = genericToCBOR
