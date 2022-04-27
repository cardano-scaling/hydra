-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol.
module Hydra.Party where

import Hydra.Prelude hiding (show)

import Data.Aeson (ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import Hydra.Crypto (hashVerificationKey)
import qualified Hydra.Crypto as Hydra
import qualified Hydra.Data.Party as OnChain

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party = Party {vkey :: Hydra.VerificationKey}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromJSONKey, ToJSONKey)

-- REVIEW: Do we really want to define Ord or also use unordered-containers
-- based on Hashable?
instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    hashVerificationKey a <= hashVerificationKey b

instance Arbitrary Party where
  arbitrary = Party <$> arbitrary

instance FromCBOR Party where
  fromCBOR = Party <$> fromCBOR

instance ToCBOR Party where
  toCBOR Party{vkey} = toCBOR vkey

-- | Get the 'Party' given some Hydra 'SigningKey'.
deriveParty :: Hydra.SigningKey -> Party
deriveParty = Party . Hydra.deriveVerificationKey

convertPartyFromChain :: MonadFail m => OnChain.Party -> m Party
convertPartyFromChain =
  fmap Party . Hydra.deserialiseVerificationKeyFromRawBytes . OnChain.partyToVerficationKeyBytes

convertPartyToChain :: Party -> OnChain.Party
convertPartyToChain Party{vkey} =
  OnChain.partyFromVerificationKeyBytes $ Hydra.serialiseVerificationKeyToRawBytes vkey
