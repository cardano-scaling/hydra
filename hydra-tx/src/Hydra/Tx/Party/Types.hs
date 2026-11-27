module Hydra.Tx.Party.Types where

import Hydra.Prelude

import Data.Aeson (FromJSONKey, ToJSONKey)
import Hydra.Tx.Crypto (HydraKey, VerificationKey, verificationKeyHash)

-- | Identifies a party in a Hydra head by its 'VerificationKey'.
newtype Party = Party {vkey :: VerificationKey HydraKey}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (ToJSONKey, FromJSONKey)

instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    verificationKeyHash a <= verificationKeyHash b

instance FromCBOR Party where
  fromCBOR = genericFromCBOR

instance ToCBOR Party where
  toCBOR = genericToCBOR
