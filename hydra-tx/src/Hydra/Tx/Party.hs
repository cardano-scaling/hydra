-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol.
module Hydra.Tx.Party where

import Hydra.Prelude

import Hydra.Cardano.Api (
  AsType (AsVerificationKey),
  SerialiseAsRawBytes (..),
  SigningKey,
  VerificationKey,
  deserialiseFromRawBytesHex,
  getVerificationKey,
  serialiseToRawBytesHexText,
  verificationKeyHash,
 )
import Hydra.Data.Party qualified as OnChain
import Hydra.Tx.Crypto (AsType (AsHydraKey), HydraKey)
import "aeson" Data.Aeson (FromJSONKeyFunction (FromJSONKeyTextParser), ToJSONKey (..))
import "aeson" Data.Aeson.Types (FromJSONKey (..), toJSONKeyText)

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party = Party {vkey :: VerificationKey HydraKey}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSONKey Party where
  toJSONKey = toJSONKeyText (serialiseToRawBytesHexText . vkey)

instance FromJSONKey Party where
  fromJSONKey = FromJSONKeyTextParser partyFromHexText
   where
    partyFromHexText :: MonadFail m => Text -> m Party
    partyFromHexText t =
      case deserialiseFromRawBytesHex (encodeUtf8 t) of
        Left err -> fail $ "failed to decode Party: " <> show err
        Right vkey -> pure $ Party{vkey}

-- REVIEW: Do we really want to define Ord or also use unordered-containers
-- based on Hashable?
instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    verificationKeyHash a <= verificationKeyHash b

instance FromCBOR Party where
  fromCBOR = Party <$> fromCBOR

instance ToCBOR Party where
  toCBOR Party{vkey} = toCBOR vkey

-- | Get the 'Party' given some Hydra 'SigningKey'.
deriveParty :: SigningKey HydraKey -> Party
deriveParty = Party . getVerificationKey

-- | Convert "high-level" 'Party' to the "low-level" representation as used
-- on-chain. See 'Hydra.Data.Party.Party' for an explanation why this is a
-- distinct type.
partyToChain :: Party -> OnChain.Party
partyToChain Party{vkey} =
  OnChain.partyFromVerificationKeyBytes $ serialiseToRawBytes vkey

-- | Retrieve the "high-level" 'Party from the "low-level" on-chain
-- representation. This can fail because of the lower type-safety used on-chain
-- and a non-guaranteed verification key length. See 'Hydra.Data.Party.Party'
-- for an explanation why this is a distinct type.
partyFromChain :: MonadFail m => OnChain.Party -> m Party
partyFromChain =
  either (\e -> fail $ "partyFromChain failed: " <> show e) (pure . Party)
    . deserialiseFromRawBytes (AsVerificationKey AsHydraKey)
    . OnChain.partyToVerificationKeyBytes

-- | Type class to retrieve the 'Party' from some type.
class HasParty a where
  getParty :: a -> Party
