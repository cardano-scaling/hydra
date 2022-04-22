{-# LANGUAGE TypeApplications #-}

-- | Hydra multi-signature credentials and cryptographic primitives used to sign
-- and verify snapshots (or any messages) within the Hydra protocol.
--
-- Currently this interface is only supporting naiive, concatenated
-- multi-signatures and will change when we adopt aggregated multi-signatures
-- including aggregate keys.
--
-- It is recommended to import this module qualified to avoid confusion with
-- Cardano keys & signatures.
module Hydra.Crypto where

-- TODO: explicit exports and hide 'HydraSigningeKey' and 'HydraVerificationKey'?

import Hydra.Prelude hiding (show)

import Cardano.Crypto.DSIGN (
  ContextDSIGN,
  Ed25519DSIGN,
  SigDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  hashVerKeyDSIGN,
  rawDeserialiseSigDSIGN,
  rawDeserialiseVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  seedSizeDSIGN,
  signDSIGN,
  verifyDSIGN,
 )
import Cardano.Crypto.Hash (Blake2b_256, Hash, castHash)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Text.Show (Show (show))

-- | The used signature algorithm
type SignAlg = Ed25519DSIGN

-- * Hydra keys

-- | Hydra signing key which can be used to 'sign' messages and 'aggregate'
-- multi-signatures or 'deriveVerificationKey'.
--
-- FIXME: rewrite Show instance to /not/ expose secret, eg. 8 bytes from the hash of
-- the key
newtype SigningKey = HydraSigningKey (SignKeyDSIGN SignAlg)
  deriving (Eq, Show)

instance Arbitrary SigningKey where
  arbitrary = generateSigningKey <$> arbitrary

-- | Create a new 'SigningKey' from a 'ByteString' seed. The created keys are
-- not random and insecure, so don't use this is production code!
generateSigningKey :: ByteString -> SigningKey
generateSigningKey seed =
  HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes padded
 where
  needed = fromIntegral $ seedSizeDSIGN (Proxy :: Proxy SignAlg)
  provided = BS.length seed
  padded = seed <> BS.pack (replicate (needed - provided) 0)

-- | Get the 'VerificationKey' for a given 'SigningKey'.
deriveVerificationKey :: SigningKey -> VerificationKey
deriveVerificationKey (HydraSigningKey sk) = HydraVerificationKey (deriveVerKeyDSIGN sk)

-- | Hydra verification key, which can be used to 'verify' signed messages.
newtype VerificationKey = HydraVerificationKey (VerKeyDSIGN SignAlg)
  deriving (Eq, Show)
  deriving newtype (ToCBOR, FromCBOR)

instance ToJSON VerificationKey where
  toJSON (HydraVerificationKey vk) =
    toJSON (decodeUtf8 @Text $ Base16.encode $ rawSerialiseVerKeyDSIGN vk)

-- TODO: It would be nice(r) to have a bech32 representation for verification
-- keys BUT cardano-api decided to not expose the class internals which makes it
-- impossible to define new instances for that class :upside-down-smiling-face:
--
-- instance SerialiseAsBech32 VerificationKey where
--  bech32PrefixFor = const "hydra_vk"
--  bech32PrefixesPermitted _ = ["hydra_vk"]

instance FromJSON VerificationKey where
  parseJSON = Aeson.withText "VerificationKey" $ decodeBase16 >=> deserialiseKey
   where
    deserialiseKey =
      maybe
        (fail "Unable to deserialize VerificationKey")
        (pure . HydraVerificationKey)
        . rawDeserialiseVerKeyDSIGN

instance Arbitrary VerificationKey where
  arbitrary = generateVerificationKey <$> arbitrary

-- | Generate a 'VerificationKey' from a 'ByteString' seed. Use
-- 'generateSigningKey' and 'deriveVerificationKey' if you also need the
-- 'SigningKey'.
generateVerificationKey :: ByteString -> VerificationKey
generateVerificationKey =
  deriveVerificationKey . generateSigningKey

hashVerificationKey :: VerificationKey -> Hash Blake2b_256 VerificationKey
hashVerificationKey (HydraVerificationKey vk) =
  castHash $ hashVerKeyDSIGN vk
-- * Signatures

-- | Signature of 'a', not containing the actual payload.
newtype Signature a = HydraSignature (SigDSIGN SignAlg)
  deriving (Eq)
  deriving newtype (ToCBOR, FromCBOR)

instance Show (Signature a) where
  show (HydraSignature sig) =
    "HydraSignature " <> show hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSigDSIGN sig

instance Hashable (Signature a) where
  hashWithSalt salt (HydraSignature sig) =
    hashWithSalt salt (rawSerialiseSigDSIGN sig)

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (Signature a) where
  arbitrary = sign <$> arbitrary <*> arbitrary

instance ToJSON a => ToJSON (Signature a) where
  toJSON (HydraSignature sig) = Aeson.String $ decodeUtf8 hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSigDSIGN sig

instance FromJSON a => FromJSON (Signature a) where
  parseJSON = Aeson.withText "Signed" $ \t -> do
    bs <- decodeBase16 t
    maybe
      (fail "deserialise signature from bytes failed")
      (pure . HydraSignature)
      $ rawDeserialiseSigDSIGN bs

-- | Sign some value 'a' with the provided 'SigningKey'.
sign :: SignableRepresentation a => SigningKey -> a -> Signature a
sign (HydraSigningKey sk) a =
  HydraSignature $ signDSIGN ctx a sk
 where
  ctx = () :: ContextDSIGN SignAlg

-- | Verify a given 'Signature a' and value 'a' using provided 'VerificationKey'.
verify :: SignableRepresentation a => VerificationKey -> Signature a -> a -> Bool
verify (HydraVerificationKey vk) (HydraSignature sig) a =
  case verifyDSIGN ctx vk a sig of
    Right () -> True
    -- NOTE: Current implementation does not yield multiple Left cases, so no need
    -- to distinguish in our interface
    Left _ -> False
 where
  ctx = () :: ContextDSIGN SignAlg

-- * Multi-signatures

-- | Naiively aggregated multi-signatures.
newtype MultiSignature a = HydraMultiSignature [Signature a]
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Combine multiple signatures of 'a' into a 'MultiSignature a'.
aggregate :: [Signature a] -> MultiSignature a
aggregate = HydraMultiSignature
