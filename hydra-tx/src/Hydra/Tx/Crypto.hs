{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Hydra multi-signature credentials and cryptographic primitives used to sign
-- and verify snapshots (or any messages) within the Hydra protocol.
--
-- We are re-using the 'Key' interface of 'cardano-api' for a consistent
-- representation. For example: Cardano credentials are 'VerificationKey
-- PaymentKey', Hydra credentials are 'VerificationKey HydraKey'.
--
-- Currently 'MultiSignature' interface is only supporting naiive, concatenated
-- multi-signatures and will change when we adopt aggregated multi-signatures
-- including aggregate keys.
module Hydra.Tx.Crypto (
  -- * Cardano Key interface
  Key (..),

  -- * Hydra specifics
  Hash (HydraKeyHash),
  AsType (AsHydraKey),
  SigningKey (HydraSigningKey),
  VerificationKey (HydraVerificationKey),
  module Hydra.Tx.Crypto,
) where

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
import Cardano.Crypto.Hash (Blake2b_256, SHA256, castHash, hashFromBytes, hashToBytes)
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Hash.Class (HashAlgorithm (digest))
import Cardano.Crypto.Seed (getSeedBytes, mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.Map qualified as Map
import Hydra.Cardano.Api (
  AsType (..),
  BlockHeader,
  HasTextEnvelope (..),
  HasTypeProxy (..),
  Hash,
  Key (..),
  ScriptData (..),
  SerialiseAsCBOR,
  SerialiseAsRawBytes (..),
  SerialiseAsRawBytesError (..),
  TxId (..),
  UsingRawBytesHex (..),
  serialiseToRawBytesHexText,
 )
import Hydra.Contract.HeadState qualified as OnChain
import PlutusLedgerApi.V3 qualified as Plutus
import Test.QuickCheck (vectorOf)
import Test.QuickCheck.Instances.ByteString ()
import Text.Show (Show (..))

-- * Hydra keys

-- | Hydra keys (keyrole) which can be used to 'sign' and 'verify' messages, as
-- well as 'aggregate' multi-signatures.
data HydraKey

instance HasTypeProxy HydraKey where
  data AsType HydraKey = AsHydraKey
  proxyToAsType _ = AsHydraKey

-- | Hashes of Hydra keys
newtype instance Hash HydraKey
  = HydraKeyHash (Crypto.Hash Blake2b_256 (VerificationKey HydraKey))
  deriving stock (Ord, Eq, Show)

instance SerialiseAsRawBytes (Hash HydraKey) where
  serialiseToRawBytes (HydraKeyHash vkh) = hashToBytes vkh

  deserialiseFromRawBytes (AsHash AsHydraKey) bs =
    maybe
      (Left $ SerialiseAsRawBytesError "invalid length when deserializing Hash HydraKey")
      (Right . HydraKeyHash)
      (hashFromBytes bs)

instance SerialiseAsRawBytes a => IsString (UsingRawBytesHex a) where
  fromString = either (error . toText) id . deserialiseFromRawBytesBase16 . BSC.pack

deriving via UsingRawBytesHex (Hash BlockHeader) instance IsString (Hash BlockHeader)
deriving via UsingRawBytesHex TxId instance IsString TxId
deriving via UsingRawBytesHex (Hash ScriptData) instance IsString (Hash ScriptData)

deserialiseFromRawBytesBase16 ::
  SerialiseAsRawBytes a => ByteString -> Either String (UsingRawBytesHex a)
deserialiseFromRawBytesBase16 str =
  case Base16.decode str of
    Right raw -> case deserialiseFromRawBytes ttoken raw of
      Right x -> Right (UsingRawBytesHex x)
      Left (SerialiseAsRawBytesError msg) -> Left ("cannot deserialise " ++ show str ++ ".  The error was: " <> msg)
    Left msg -> Left ("invalid hex " ++ show str ++ ", " ++ msg)
 where
  ttoken = proxyToAsType (Proxy :: Proxy a)

instance Key HydraKey where
  -- Hydra verification key, which can be used to 'verify' signed messages.
  newtype VerificationKey HydraKey
    = HydraVerificationKey (VerKeyDSIGN Ed25519DSIGN)
    deriving stock (Eq, Ord)
    deriving (Show, IsString) via UsingRawBytesHex (VerificationKey HydraKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  -- Hydra signing key which can be used to 'sign' messages and 'aggregate'
  -- multi-signatures or 'deriveVerificationKey'.
  newtype SigningKey HydraKey
    = HydraSigningKey (SignKeyDSIGN Ed25519DSIGN)
    deriving stock (Eq, Ord)
    deriving (Show, IsString) via UsingRawBytesHex (SigningKey HydraKey)
    deriving newtype (ToCBOR, FromCBOR)
    deriving anyclass (SerialiseAsCBOR)

  -- Get the 'VerificationKey' for a given 'SigningKey'.
  getVerificationKey (HydraSigningKey sk) =
    HydraVerificationKey $ deriveVerKeyDSIGN sk

  -- Create a new 'SigningKey' from a 'Seed'. See 'generateSigningKey'
  deterministicSigningKey AsHydraKey =
    generateSigningKey . getSeedBytes

  -- Get the number of bytes required to seed a signing key with
  -- 'deterministicSigningKey'.
  deterministicSigningKeySeedSize AsHydraKey =
    seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)

  -- Get the verification key hash of a 'VerificationKey'. See 'Blake2b_256' for
  -- info on the used hashing algorithm.
  verificationKeyHash (HydraVerificationKey vk) =
    HydraKeyHash . castHash $ hashVerKeyDSIGN vk

instance Arbitrary (SigningKey HydraKey) where
  arbitrary = generateSigningKey . BS.pack <$> vectorOf 32 arbitrary

instance HasTextEnvelope (SigningKey HydraKey) where
  textEnvelopeType _ =
    "HydraSigningKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy Ed25519DSIGN))

instance SerialiseAsRawBytes (SigningKey HydraKey) where
  serialiseToRawBytes (HydraSigningKey sk) =
    rawSerialiseSignKeyDSIGN sk

  deserialiseFromRawBytes (AsSigningKey AsHydraKey) bs =
    maybe
      (Left (SerialiseAsRawBytesError "invalid length when deserializing SigningKey HydraKey"))
      (Right . HydraSigningKey)
      (rawDeserialiseSignKeyDSIGN bs)

instance Arbitrary (VerificationKey HydraKey) where
  arbitrary = getVerificationKey <$> arbitrary

instance SerialiseAsRawBytes (VerificationKey HydraKey) where
  serialiseToRawBytes (HydraVerificationKey vk) =
    rawSerialiseVerKeyDSIGN vk

  deserialiseFromRawBytes (AsVerificationKey AsHydraKey) bs =
    maybe
      (Left $ SerialiseAsRawBytesError "invalid length when deserializing VerificationKey HydraKey")
      (Right . HydraVerificationKey)
      (rawDeserialiseVerKeyDSIGN bs)

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

instance HasTextEnvelope (VerificationKey HydraKey) where
  textEnvelopeType _ =
    "HydraVerificationKey_"
      <> fromString (algorithmNameDSIGN (Proxy :: Proxy Ed25519DSIGN))

instance ToJSON (SigningKey HydraKey) where
  toJSON = toJSON . serialiseToRawBytesHexText

instance FromJSON (SigningKey HydraKey) where
  parseJSON = Aeson.withText "SigningKey" $ decodeBase16 >=> deserialiseKey
   where
    deserialiseKey =
      maybe
        (fail "unable to deserialize SigningKey, wrong length")
        (pure . HydraSigningKey)
        . rawDeserialiseSignKeyDSIGN

-- | Create a new 'SigningKey' from a 'ByteString' seed. The created keys are
-- not random and insecure, so don't use this in production code!
generateSigningKey :: ByteString -> SigningKey HydraKey
generateSigningKey seed =
  HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes hashOfSeed
 where
  hashOfSeed = digest (Proxy :: Proxy SHA256) seed

-- * Signatures

-- | Signature of 'a', not containing the actual payload.
newtype Signature a = HydraSignature (SigDSIGN Ed25519DSIGN)
  deriving stock (Eq)
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
  ctx = () :: ContextDSIGN Ed25519DSIGN

-- | Verify a given 'Signature a' and value 'a' using provided 'VerificationKey'.
verify ::
  SignableRepresentation a =>
  VerificationKey HydraKey ->
  Signature a ->
  a ->
  Bool
verify (HydraVerificationKey vk) (HydraSignature sig) a =
  case verifyDSIGN ctx vk a sig of
    Right () -> True
    -- NOTE: Current implementation does not yield multiple Left cases, so no need
    -- to distinguish in our interface
    Left _ -> False
 where
  ctx = () :: ContextDSIGN Ed25519DSIGN

-- * Multi-signatures

-- | Naiively aggregated multi-signatures.
newtype MultiSignature a = HydraMultiSignature {multiSignature :: [Signature a]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Semigroup, Monoid)

deriving anyclass instance ToJSON a => ToJSON (MultiSignature a)
deriving anyclass instance FromJSON a => FromJSON (MultiSignature a)

instance (Arbitrary a, SignableRepresentation a) => Arbitrary (MultiSignature a) where
  arbitrary = HydraMultiSignature <$> arbitrary

-- | Combine multiple signatures of 'a' into a 'MultiSignature a'.
aggregate :: [Signature a] -> MultiSignature a
aggregate = HydraMultiSignature

-- | Like aggregate, but use order of given list of keys instead.
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

-- | A result type for multisigs verification providing some information
-- in case of failure.
--
-- This type is of course structurally equivalent to `Maybe [VerificationKey HydraKey]` but it's much more explicit.
data Verified
  = Verified
  | FailedKeys {failedKeys :: [VerificationKey HydraKey]}
  | KeyNumberMismatch
  deriving stock (Eq, Show, Generic)

-- | Verify a given 'MultiSignature a' and value 'a' provided a list of
-- 'VerificationKey'.
--
-- Note that order of keys is relevant and that length of signature and
-- multisignature list needs to be the same.
verifyMultiSignature ::
  SignableRepresentation a =>
  [VerificationKey HydraKey] ->
  MultiSignature a ->
  a ->
  Verified
verifyMultiSignature vks HydraMultiSignature{multiSignature} a
  | length vks == length multiSignature =
      let verifications = zipWith (\vk s -> (vk, verify vk s a)) vks multiSignature
          failures = fst <$> filter (not . snd) verifications
       in if null failures
            then Verified
            else FailedKeys failures
  | otherwise = KeyNumberMismatch

toPlutusSignatures :: MultiSignature a -> [OnChain.Signature]
toPlutusSignatures (HydraMultiSignature sigs) =
  toPlutusSignature <$> sigs
 where
  toPlutusSignature :: Signature a -> OnChain.Signature
  toPlutusSignature (HydraSignature sig) =
    Plutus.toBuiltin $ rawSerialiseSigDSIGN sig
