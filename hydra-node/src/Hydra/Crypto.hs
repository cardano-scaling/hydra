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
  DSIGNAlgorithm (rawSerialiseSignKeyDSIGN, rawSerialiseVerKeyDSIGN),
  Ed25519DSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
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
newtype Signature a = UnsafeSignature ByteString
  deriving (Eq)

-- * Multi-signatures

-- | Naiively aggregated multi-signatures.
newtype MultiSignature a = UnsafeMultiSignature [Signature a]
