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
  rawSerialiseSigDSIGN,
  rawSerialiseSignKeyDSIGN,
  rawSerialiseVerKeyDSIGN,
  seedSizeDSIGN,
  signDSIGN,
  verifyDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Text.Show (Show (show))

-- | The used signature algorithm
type SignAlg = Ed25519DSIGN

-- * Hydra keys

-- | Hydra signing key which can be used to 'sign' messages and 'aggregate'
-- multi-signatures or 'deriveVerificationKey'.
newtype SigningKey = HydraSigningKey (SignKeyDSIGN SignAlg)
  deriving (Eq)

-- NOTE: Contains full key material (or seed).
-- REVIEW: Should we truncate to keep logs short?
instance Show SigningKey where
  show (HydraSigningKey sk) =
    "HydraSigningKey " <> show hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSignKeyDSIGN sk

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
  deriving (Eq)

instance Show VerificationKey where
  show (HydraVerificationKey vk) =
    "HydraVerificationKey " <> show hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseVerKeyDSIGN vk

-- | Generate a 'VerificationKey' from a 'ByteString' seed. Use
-- 'generateSigningKey' and 'deriveVerificationKey' if you also need the
-- 'SigningKey'.
generateVerificationKey :: ByteString -> VerificationKey
generateVerificationKey =
  deriveVerificationKey . generateSigningKey

-- * Signatures

-- | Signature of 'a', not containing the actual payload.
newtype Signature a = HydraSignature (SigDSIGN SignAlg)
  deriving (Eq)

instance Show (Signature a) where
  show (HydraSignature sig) =
    "HydraSignature " <> show hexBytes
   where
    hexBytes = Base16.encode $ rawSerialiseSigDSIGN sig

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
newtype MultiSignature a = UnsafeMultiSignature [Signature a]
