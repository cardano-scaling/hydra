{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
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

  --

  -- | NOTE: We re-export 'Key' but deliberately *omit* its
  -- 'getVerificationKey' method. The polymorphic 'getVerificationKey'
  -- exported below (via 'HasVerificationKey') subsumes it and additionally
  -- accepts 'Secret'-wrapped signing keys.
  Key (deterministicSigningKey, deterministicSigningKeySeedSize, verificationKeyHash),

  -- * Hydra specifics
  Hash (HydraKeyHash),
  AsType (AsHydraKey),
  SigningKey (HydraSigningKey),
  VerificationKey (HydraVerificationKey),
  HasVerificationKey (getVerificationKey),
  CanSignTx (signTx),
  module Hydra.Tx.Crypto,

  -- * Re-exports
  getSignableRepresentation,
) where

import Hydra.Prelude hiding (Key, show)

import Cardano.Binary (decodeFull', serialize')
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
import Cardano.Crypto.Util (SignableRepresentation, getSignableRepresentation)
import Control.Exception (TypeError (..), throw)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as BSC
import Data.Map.Strict qualified as Map
import Hydra.Cardano.Api (
  AsType (..),
  BlockHeader,
  HasTextEnvelope (..),
  HasTypeProxy (..),
  Hash,
  Key (deterministicSigningKey, deterministicSigningKeySeedSize, verificationKeyHash),
  ScriptData (..),
  SerialiseAsCBOR (..),
  SerialiseAsRawBytes (..),
  SerialiseAsRawBytesError (..),
  SigningKey,
  TxId (..),
  UsingRawBytesHex (..),
  VerificationKey,
  serialiseToRawBytesHexText,
 )
import Hydra.Cardano.Api qualified as Cardano
import Hydra.Contract.HeadState qualified as OnChain
import Hydra.Tx.Secret (Forbid, Secret, mkSecret, withSecret)
import PlutusLedgerApi.V3 qualified as Plutus
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
  --
  -- No 'Show', 'ToJSON', 'FromJSON', 'ToCBOR' or 'FromCBOR' is provided:
  -- those are the channels through which a key could accidentally leak
  -- (logs, API responses, on-disk persistence). They are forbidden via
  -- 'Forbid'-bearing 'TypeError' instances further down, so any caller
  -- that tries to use one gets a precise compile error.
  --
  -- Wrap in 'Hydra.Tx.Secret.Secret' at the field level to extend the
  -- ban to enclosing records (their @deriving stock (Show)@ /
  -- @deriving anyclass (ToJSON)@ then propagates to the same custom
  -- error).
  newtype SigningKey HydraKey
    = HydraSigningKey (SignKeyDSIGN Ed25519DSIGN)
    deriving stock (Eq, Ord)
    deriving (IsString) via UsingRawBytesHex (SigningKey HydraKey)

  -- Get the 'VerificationKey' for a given 'SigningKey'.
  getVerificationKey (HydraSigningKey sk) =
    HydraVerificationKey $ deriveVerKeyDSIGN sk

  -- Create a new 'SigningKey' from a 'Seed'. The 'cardano-api' 'Key'
  -- class returns a raw 'SigningKey'; callers (in tests / setup paths)
  -- should immediately wrap the result with 'mkSecret'. The
  -- public-facing 'generateSigningKey' below already does this.
  deterministicSigningKey AsHydraKey seed =
    HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes (getSeedBytes seed)

  -- Get the number of bytes required to seed a signing key with
  -- 'deterministicSigningKey'.
  deterministicSigningKeySeedSize AsHydraKey =
    seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN)

  -- Get the verification key hash of a 'VerificationKey'. See 'Blake2b_256' for
  -- info on the used hashing algorithm.
  verificationKeyHash (HydraVerificationKey vk) =
    HydraKeyHash . castHash $ hashVerKeyDSIGN vk

-- | Polymorphic 'getVerificationKey' that works on either a raw
-- 'SigningKey k' or a 'Secret'-wrapped 'Secret (SigningKey k)'. This
-- subsumes the cardano-api 'Key' class method of the same name and is
-- what 'Hydra.Tx.Crypto' re-exports. Callers can write
-- @getVerificationKey sk@ regardless of whether @sk@ is wrapped.
class HasVerificationKey s k | s -> k where
  getVerificationKey :: s -> VerificationKey k

instance (Key k, HasTypeProxy k) => HasVerificationKey (SigningKey k) k where
  getVerificationKey = Cardano.getVerificationKey

instance (Key k, HasTypeProxy k) => HasVerificationKey (Secret (SigningKey k)) k where
  getVerificationKey s = withSecret s Cardano.getVerificationKey

-- | Polymorphic 'signTx' that accepts either a raw 'SigningKey PaymentKey'
-- or a 'Secret'-wrapped one. Production paths thread 'Secret' here and
-- benefit from the wrap-free call shape; tests can still pass raw keys.
class CanSignTx s where
  signTx :: s -> Cardano.Tx -> Cardano.Tx

instance CanSignTx (SigningKey Cardano.PaymentKey) where
  signTx = Cardano.signTx

instance CanSignTx (Secret (SigningKey Cardano.PaymentKey)) where
  signTx s tx = withSecret s (`Cardano.signTx` tx)

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

-- | CBOR encode by delegating to the inner 'SignKeyDSIGN' (which has its own
-- 'ToCBOR' / 'FromCBOR'). This keeps the on-disk text-envelope format
-- identical to the previous newtype-derived encoding, while leaving
-- 'ToCBOR' / 'FromCBOR' on 'SigningKey HydraKey' itself forbidden.
instance SerialiseAsCBOR (SigningKey HydraKey) where
  serialiseToCBOR (HydraSigningKey sk) = serialize' sk
  deserialiseFromCBOR _ bs = HydraSigningKey <$> decodeFull' bs

-- Refuse 'ToJSON', 'FromJSON', 'ToCBOR' and 'FromCBOR' on raw signing
-- keys. No 'Show' instance is provided either: every holder of a
-- signing key in this codebase wraps it in 'Hydra.Tx.Secret.Secret', so
-- the rendering / serialisation goes through Secret's instances (Secret
-- has a redacting 'Show' and 'TypeError'-bearing JSON/CBOR instances).
-- Any code that tries to Show / JSON / CBOR a raw 'SigningKey HydraKey'
-- gets either a "no instance" or a 'Forbid' custom error from GHC.

-- The bodies below mirror the pattern in 'Hydra.Tx.Secret': the 'Forbid'
-- constraint fires at normal compile time, so the bodies are unreachable.
-- Under '-fdefer-type-errors' they get reached: throw a
-- 'Control.Exception.TypeError' so the runtime exception type matches
-- what the negative-spec helpers look for.
instance Forbid "encode to JSON" => ToJSON (SigningKey HydraKey) where
  toJSON _ = throw (TypeError "Refusing to encode SigningKey HydraKey to JSON")

instance Forbid "decode from JSON" => FromJSON (SigningKey HydraKey) where
  parseJSON _ = throw (TypeError "Refusing to decode SigningKey HydraKey from JSON")

instance Forbid "CBOR-encode" => ToCBOR (SigningKey HydraKey) where
  toCBOR _ = throw (TypeError "Refusing to CBOR-encode SigningKey HydraKey")

instance Forbid "CBOR-decode" => FromCBOR (SigningKey HydraKey) where
  fromCBOR = throw (TypeError "Refusing to CBOR-decode SigningKey HydraKey")

-- | Create a new 'SigningKey' from a 'ByteString' seed, wrapped in
-- 'Secret'. The public API never returns a raw 'SigningKey HydraKey'.
-- The created keys are not random and insecure, so don't use this in
-- production code!
generateSigningKey :: ByteString -> Secret (SigningKey HydraKey)
generateSigningKey seed =
  mkSecret . HydraSigningKey . genKeyDSIGN $ mkSeedFromBytes hashOfSeed
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

-- | Sign some value 'a' with the provided 'Secret'-wrapped 'SigningKey'.
-- The unwrap is internal: callers don't need a 'withSecret' at the call
-- site, so the noisy continuation pattern is hidden where it matters.
sign :: SignableRepresentation a => Secret (SigningKey HydraKey) -> a -> Signature a
sign secret a =
  withSecret secret $ \(HydraSigningKey sk) ->
    HydraSignature (signDSIGN ctx a sk)
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
verifyMultiSignature vks multisig a =
  verifyMultiSignatureBytes vks multisig (getSignableRepresentation a)

-- | Like 'verifyMultiSignature' but operates on pre-computed signable bytes,
-- avoiding repeated calls to 'getSignableRepresentation'. Use this when the
-- same value would be verified multiple times (e.g. per-party in a round).
verifyMultiSignatureBytes ::
  [VerificationKey HydraKey] ->
  MultiSignature a ->
  ByteString ->
  Verified
verifyMultiSignatureBytes vks HydraMultiSignature{multiSignature} bs
  | length vks == length multiSignature =
      let ctx = () :: ContextDSIGN Ed25519DSIGN
          failures =
            mapMaybe
              ( \(vk@(HydraVerificationKey rawVk), HydraSignature sig) ->
                  case verifyDSIGN ctx rawVk bs sig of
                    Right () -> Nothing
                    Left _ -> Just vk
              )
              (zip vks multiSignature)
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
