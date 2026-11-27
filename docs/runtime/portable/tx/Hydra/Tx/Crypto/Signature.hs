{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.Crypto.Signature (
  module Cardano.Crypto.Util,
  HydraKey,
  SigningKey,
  VerificationKey,
  Signature,
  generateSigningKey,
  getVerificationKey,
  verificationKeyHash,
  sign,
  verifyBytes,
) where

import Cardano.Crypto.Util
import Data.Aeson (FromJSONKey, ToJSONKey)
import Hydra.Prelude
import Hydra.Prelude.Core (HexBytes (..))

data HydraKey
newtype SigningKey key = SigningKey ByteString
newtype VerificationKey key = VerificationKey ByteString
  deriving stock (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via HexBytes

data Signature a = Witness (VerificationKey HydraKey) Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

generateSigningKey :: ByteString -> SigningKey HydraKey
generateSigningKey = SigningKey

getVerificationKey :: SigningKey key -> VerificationKey key
getVerificationKey (SigningKey actor) = VerificationKey actor

verificationKeyHash :: VerificationKey key -> ByteString
verificationKeyHash (VerificationKey bytes) = bytes

sign :: SignableRepresentation a => SigningKey HydraKey -> a -> Signature a
sign key value = Witness (getVerificationKey key) (encodeBase16 $ getSignableRepresentation value)

verifyBytes :: VerificationKey HydraKey -> Signature a -> ByteString -> Bool
verifyBytes actor (Witness author payload) bytes = actor == author && payload == encodeBase16 bytes
