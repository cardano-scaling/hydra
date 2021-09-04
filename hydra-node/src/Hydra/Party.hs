{-# LANGUAGE TypeApplications #-}

-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol. Hence, this module also includes functions to sign and verify
-- data given a 'Party'.
module Hydra.Party where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  MockDSIGN,
  SigDSIGN (..),
  SignKeyDSIGN,
  VerKeyDSIGN (VerKeyMockDSIGN),
  signDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Data.Aeson (FromJSONKey (..), ToJSONKey (..), Value (String), withText)
import qualified Data.ByteString.Base16 as Base16
import Test.QuickCheck (vectorOf)

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party = UnsafeParty
  { vkey :: VerificationKey
  }
  deriving stock (Eq, Generic)
  deriving newtype (Show, Num)

instance Ord Party where
  (UnsafeParty a) <= (UnsafeParty b) =
    rawSerialiseVerKeyDSIGN a <= rawSerialiseVerKeyDSIGN b

instance Arbitrary Party where
  arbitrary = deriveParty . generateKey <$> arbitrary

instance ToJSONKey Party
instance ToJSON Party where
  toJSON (UnsafeParty (VerKeyMockDSIGN i)) = toJSON i

instance FromJSONKey Party
instance FromJSON Party where
  parseJSON = fmap fromInteger . parseJSON

instance FromCBOR Party where
  fromCBOR = UnsafeParty <$> fromCBOR

instance ToCBOR Party where
  toCBOR (UnsafeParty vk) = toCBOR vk

type VerificationKey = VerKeyDSIGN MockDSIGN

type SigningKey = SignKeyDSIGN MockDSIGN

deriveParty :: SigningKey -> Party
deriveParty = coerce . deriveVerKeyDSIGN

generateKey :: Integer -> SigningKey
generateKey = fromInteger

sign :: SignableRepresentation a => SigningKey -> a -> Signed a
sign signingKey signable = UnsafeSigned $ signDSIGN () signable signingKey

verify :: SignableRepresentation a => Signed a -> Party -> a -> Bool
verify (UnsafeSigned sig) (UnsafeParty vk) msg =
  isRight (verifyDSIGN () vk msg sig)

-- | Signature of 'a'
newtype Signed a = UnsafeSigned (SigDSIGN MockDSIGN)
  deriving (Eq, Show)

instance Arbitrary (Signed a) where
  arbitrary = do
    key <- genKeyDSIGN . mkSeedFromBytes . fromList <$> vectorOf 8 arbitrary
    a <- arbitrary @ByteString
    pure . UnsafeSigned $ signDSIGN () a key

instance ToJSON a => ToJSON (Signed a) where
  toJSON (UnsafeSigned sig) = String . decodeUtf8 . Base16.encode . rawSerialiseSigDSIGN $ sig

instance FromJSON a => FromJSON (Signed a) where
  parseJSON = withText "Signed" $ decodeBase16' >=> deserialiseSigned
   where
    decodeBase16' :: MonadFail f => Text -> f ByteString
    decodeBase16' =
      either (fail . show) pure . Base16.decode . encodeUtf8

    deserialiseSigned :: MonadFail f => ByteString -> f (Signed a)
    deserialiseSigned =
      let err = "Unable to decode signature"
       in maybe (fail err) (pure . UnsafeSigned) . rawDeserialiseSigDSIGN

instance Typeable a => FromCBOR (Signed a) where
  fromCBOR = UnsafeSigned <$> fromCBOR

instance Typeable a => ToCBOR (Signed a) where
  toCBOR (UnsafeSigned sig) = toCBOR sig
