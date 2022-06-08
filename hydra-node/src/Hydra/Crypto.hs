-- | Hydra multi-signature credentials and cryptographic primitives used to sign
-- and verify snapshots (or any messages) within the Hydra protocol.
--
-- We are re-using the 'Key' interface of 'cardano-api' for a consistent
-- representation. For example: Cardano credentials are 'VerificationKey
-- PaymentKey', Hydra credentials are 'VerificationKey HydraKey'.
--
-- Currently this interface is only supporting naiive, concatenated
-- multi-signatures and will change when we adopt aggregated multi-signatures
-- including aggregate keys.
module Hydra.Crypto where

import Hydra.Prelude hiding (Key, show)

import Cardano.Crypto.DSIGN (
  ContextDSIGN,
  Ed25519DSIGN,
  SigDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
  algorithmNameDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  hashVerKeyDSIGN,
  rawDeserialiseSigDSIGN,
  rawDeserialiseSignKeyDSIGN,
  rawDeserialiseVerKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseSignKeyDSIGN,
  rawSerialiseVerKeyDSIGN,
  seedSizeDSIGN,
  signDSIGN,
  verifyDSIGN,
 )
import qualified Cardano.Crypto.DSIGN as Crypto
import Cardano.Crypto.Hash (Blake2b_256, castHash, hashFromBytes, hashToBytes)
import qualified Cardano.Crypto.Hash as Crypto
import Cardano.Crypto.Seed (getSeedBytes, mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Map as Map
import Hydra.Cardano.Api (
  AsType (AsHash, AsVerificationKey),
  HasTextEnvelope (..),
  HasTypeProxy (..),
  Hash,
  Key (..),
  SerialiseAsCBOR,
  SerialiseAsRawBytes (..),
  serialiseToRawBytesHexText,
 )
import qualified Hydra.Contract.HeadState as OnChain
import qualified Plutus.V2.Ledger.Api as Plutus
import Test.QuickCheck.Instances.ByteString ()
import Text.Show (Show (..))

-- | The used hash algorithm for 'HydraKey' verification key hashes.
type HashAlg = Blake2b_256

-- | The used signature algorithm for 'HydraKey' signatures.
type SignAlg = Ed25519DSIGN

-- * Hydra keys

-- | Hydra keys (keyrole) which can be used to 'sign' and 'verify' messages, as
-- well as 'aggregate' multi-signatures.
data HydraKey

instance HasTypeProxy HydraKey where
  data AsType HydraKey = AsHydraKey
  proxyToAsType _ = AsHydraKey

-- | Hashes of Hydra keys
newtype instance Hash HydraKey = HydraKeyHash (Crypto.Hash HashAlg (VerificationKey HydraKey))
  deriving stock (Ord, Eq, Show)

instance SerialiseAsRawBytes (Hash HydraKey) where
  serialiseToRawBytes (HydraKeyHash vkh) = hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsHydraKey) bs =
    HydraKeyHash <$> hashFromBytes bs

instance Key HydraKey where
  -- Hydra verification key, which can be used to 'verify' signed messages.
  newtype VerificationKey HydraKey = HydraVerificationKey (VerKeyDSIGN SignAlg)
    deriving (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  -- Hydra signing key which can be used to 'sign' messages and 'aggregate'
  -- multi-signatures or 'deriveVerificationKey'.
  --
  -- REVIEW: Maybe rewrite Show instance to /not/ expose secret, eg. 8 bytes from
  -- the hash of the key? Although both, cardano-api and cardano-crypto-class are
  -- both deriving this and thus showing secret key material as well.
  newtype SigningKey HydraKey = HydraSigningKey (SignKeyDSIGN SignAlg)
    deriving (Eq, Show)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  getVerificationKey = deriveVerificationKey

  deterministicSigningKey AsHydraKey =
    generateSigningKey . getSeedBytes

  deterministicSigningKeySeedSize AsHydraKey =
    seedSizeDSIGN (Proxy :: Proxy SignAlg)

  verificationKeyHash = hashVerificationKey

instance Arbitrary (SigningKey HydraKey) where
  arbitrary = generateSigningKey <$> arbitrary

instance HasTextEnvelope (VerificationKey HydraKey) where
  textEnvelopeType _ =
    "HydraVerificationKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy SignAlg))

instance HasTextEnvelope (SigningKey HydraKey) where
  textEnvelopeType _ =
    "HydraSigningKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy SignAlg))

-- | Serialise the signing key material as raw bytes.
serialiseSigningKeyToRawBytes :: SigningKey HydraKey -> ByteString
serialiseSigningKeyToRawBytes (HydraSigningKey sk) = rawSerialiseSignKeyDSIGN sk
{-# DEPRECATED serialiseSigningKeyToRawBytes "use Key interface instead" #-}

-- | Deserialise a signing key from raw bytes.
deserialiseSigningKeyFromRawBytes :: MonadFail m => ByteString -> m (SigningKey HydraKey)
deserialiseSigningKeyFromRawBytes bytes =
  case rawDeserialiseSignKeyDSIGN bytes of
    Nothing -> fail "failed to deserialise signing key"
    Just key -> pure $ HydraSigningKey key
{-# DEPRECATED deserialiseSigningKeyFromRawBytes "use Key interface instead" #-}

-- | Get the 'VerificationKey' for a given 'SigningKey'.
deriveVerificationKey :: SigningKey HydraKey -> VerificationKey HydraKey
deriveVerificationKey (HydraSigningKey sk) = HydraVerificationKey (deriveVerKeyDSIGN sk)
{-# DEPRECATED deriveVerificationKey "use Key interface instead" #-}

-- | Create a new 'SigningKey' from a 'ByteString' seed. The created keys are
-- not random and insecure, so don't use this in production code!
generateSigningKey :: ByteString -> SigningKey HydraKey
generateSigningKey seed =
  HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes padded
 where
  needed = fromIntegral $ seedSizeDSIGN (Proxy :: Proxy SignAlg)
  provided = BS.length seed
  padded = seed <> BS.pack (replicate (needed - provided) 0)
{-# DEPRECATED generateSigningKey "use Key interface instead" #-}

instance SerialiseAsRawBytes (VerificationKey HydraKey) where
  serialiseToRawBytes (HydraVerificationKey vk) =
    rawSerialiseVerKeyDSIGN vk

  deserialiseFromRawBytes (AsVerificationKey AsHydraKey) bs =
    HydraVerificationKey <$> rawDeserialiseVerKeyDSIGN bs

instance ToJSON (VerificationKey HydraKey) where
  toJSON = toJSON . serialiseToRawBytesHexText

-- TODO: It would be nice(r) to have a bech32 representation for verification
-- keys BUT cardano-api decided to not expose the class internals which makes it
-- impossible to define new instances for that class :upside-down-smiling-face:
--
-- instance SerialiseAsBech32 VerificationKey where
--  bech32PrefixFor = const "hydra_vk"
--  bech32PrefixesPermitted _ = ["hydra_vk"]

instance FromJSON (VerificationKey HydraKey) where
  parseJSON = Aeson.withText "VerificationKey" $ decodeBase16 >=> deserialiseKey
   where
    deserialiseKey =
      maybe
        (fail "unable to deserialize VerificationKey, wrong length")
        (pure . HydraVerificationKey)
        . rawDeserialiseVerKeyDSIGN

instance Arbitrary (VerificationKey HydraKey) where
  arbitrary = deriveVerificationKey . generateSigningKey <$> arbitrary

-- | Serialise the verification key material as raw bytes.
serialiseVerificationKeyToRawBytes :: VerificationKey HydraKey -> ByteString
serialiseVerificationKeyToRawBytes (HydraVerificationKey vk) = rawSerialiseVerKeyDSIGN vk
{-# DEPRECATED serialiseVerificationKeyToRawBytes "use Key interface instead" #-}

-- | Deserialise a verirfication key from raw bytes.
deserialiseVerificationKeyFromRawBytes :: MonadFail m => ByteString -> m (VerificationKey HydraKey)
deserialiseVerificationKeyFromRawBytes bytes =
  case rawDeserialiseVerKeyDSIGN bytes of
    Nothing -> fail "failed to deserialise verification key"
    Just key -> pure $ HydraVerificationKey key
{-# DEPRECATED deserialiseVerificationKeyFromRawBytes "use Key interface instead" #-}

-- | Get the verification key hash of a 'VerificationKey'. See 'HashAlg' for
-- info on the used hashing algorithm.
hashVerificationKey :: VerificationKey HydraKey -> Hash HydraKey
hashVerificationKey (HydraVerificationKey vk) =
  HydraKeyHash . castHash $ hashVerKeyDSIGN vk
{-# DEPRECATED hashVerificationKey "use Key interface instead" #-}

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
sign :: SignableRepresentation a => SigningKey HydraKey -> a -> Signature a
sign (HydraSigningKey sk) a =
  HydraSignature $ signDSIGN ctx a sk
 where
  ctx = () :: ContextDSIGN SignAlg

-- | Verify a given 'Signature a' and value 'a' using provided 'VerificationKey'.
verify :: SignableRepresentation a => VerificationKey HydraKey -> Signature a -> a -> Bool
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
newtype MultiSignature a = HydraMultiSignature {multiSignature :: [Signature a]}
  deriving (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (ToJSON, FromJSON)

-- | Combine multiple signatures of 'a' into a 'MultiSignature a'.
aggregate :: [Signature a] -> MultiSignature a
aggregate = HydraMultiSignature

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (MultiSignature a) where
  arbitrary = HydraMultiSignature <$> arbitrary

-- FIXME(AB): This function exists solely because the order of signatures
-- matters on-chain, and it should match the order of parties as declared in the
-- initTx. This should disappear once we use a proper multisignature scheme
aggregateInOrder :: Ord k => Map k (Signature a) -> [k] -> MultiSignature a
aggregateInOrder signatures = HydraMultiSignature . foldr appendSignature []
 where
  appendSignature k sigs =
    case Map.lookup k signatures of
      Nothing -> sigs
      Just sig -> sig : sigs

toPlutusSignatures :: MultiSignature a -> [OnChain.Signature]
toPlutusSignatures (HydraMultiSignature sigs) =
  toPlutusSignature <$> sigs
 where
  toPlutusSignature :: Signature a -> OnChain.Signature
  toPlutusSignature (HydraSignature sig) =
    Plutus.toBuiltin $ rawSerialiseSigDSIGN sig
