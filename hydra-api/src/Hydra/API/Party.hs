-- | A `Party` uniquely identifies a participant in a Hydra Head protocol.
module Hydra.API.Party where

import Prelude

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Data.Aeson (FromJSON, ToJSON, ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import GHC.Generics (Generic)
import Hydra.API.Crypto (HydraKey)
import Hydra.Cardano.Api (
  VerificationKey,
  verificationKeyHash,
 )
import Test.QuickCheck (Arbitrary (..))

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party where
  Party :: {vkey :: VerificationKey HydraKey} -> Party
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromJSONKey, ToJSONKey)

-- REVIEW: Do we really want to define Ord or also use unordered-containers
-- based on Hashable?
instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    verificationKeyHash a <= verificationKeyHash b

instance Arbitrary Party where
  arbitrary = Party <$> arbitrary

instance FromCBOR Party where
  fromCBOR = Party <$> fromCBOR

instance ToCBOR Party where
  toCBOR Party{vkey} = toCBOR vkey
