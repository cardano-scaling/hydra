{-# LANGUAGE TypeApplications #-}
-- ToJSON VerificationKey
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol. Hence, this module also includes functions to sign and verify
-- data given a 'Party'.
module Hydra.Party where

import Hydra.Prelude hiding (show)

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  MockDSIGN,
  SigDSIGN (..),
  SignKeyDSIGN,
  VerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Util (SignableRepresentation)
import Data.Aeson (ToJSONKey, Value (String), withText)
import Data.Aeson.Types (FromJSONKey)
import qualified Data.ByteString.Base16 as Base16
import Test.QuickCheck (vectorOf)
import Text.Show (Show (..))

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
data Party = Party
  { alias :: Maybe Text
  , vkey :: VerificationKey
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

instance Show Party where
  show Party{alias, vkey} =
    toString $ prefix <> showVerificationKey vkey
   where
    prefix = case alias of
      Nothing -> ""
      Just a -> a <> "@"

instance Ord Party where
  a <= b =
    alias a <= alias b
      || rawSerialiseVerKeyDSIGN (vkey a) <= rawSerialiseVerKeyDSIGN (vkey b)

instance Arbitrary Party where
  arbitrary = deriveParty . generateKey <$> arbitrary

instance FromCBOR Party where
  fromCBOR = Party <$> fromCBOR <*> fromCBOR

instance ToCBOR Party where
  toCBOR Party{alias, vkey} =
    toCBOR alias <> toCBOR vkey

-- REVIEW(SN): are default instances using 'Show' or 'ToJSON'?
instance FromJSONKey Party
instance ToJSONKey Party

-- NOTE(SN): Convenience type class to be able to quickly create parties from
-- integer literals using 'fromInteger', e.g. `let alice = 10`. This will be
-- removed at latest when we don't have MockDSIGN VerificationKeys anymore
-- TODO(SN): at least replace with a `mkParty :: Integer -> Party`
instance Num Party where
  fromInteger = deriveParty . generateKey
  (+) = error "Party is not a proper Num"
  (*) = error "Party is not a proper Num"
  abs = error "Party is not a proper Num"
  signum = error "Party is not a proper Num"
  (-) = error "Party is not a proper Num"

type VerificationKey = VerKeyDSIGN MockDSIGN

instance ToJSON VerificationKey where
  toJSON = String . showVerificationKey

instance FromJSON VerificationKey where
  parseJSON = withText "VerificationKey" $ decodeBase16' >=> deserialiseKey
   where
    deserialiseKey =
      maybe (fail "Unable to deserialize VerificationKey") pure . rawDeserialiseVerKeyDSIGN

showVerificationKey :: VerificationKey -> Text
showVerificationKey = decodeUtf8 . Base16.encode . rawSerialiseVerKeyDSIGN

type SigningKey = SignKeyDSIGN MockDSIGN

deriveParty :: SigningKey -> Party
deriveParty = Party Nothing . deriveVerKeyDSIGN

generateKey :: Integer -> SigningKey
generateKey = fromInteger

sign :: SignableRepresentation a => SigningKey -> a -> Signed a
sign signingKey signable = UnsafeSigned $ signDSIGN () signable signingKey

verify :: SignableRepresentation a => Signed a -> Party -> a -> Bool
verify (UnsafeSigned sig) Party{vkey} msg =
  isRight (verifyDSIGN () vkey msg sig)

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
    deserialiseSigned :: MonadFail f => ByteString -> f (Signed a)
    deserialiseSigned =
      let err = "Unable to decode signature"
       in maybe (fail err) (pure . UnsafeSigned) . rawDeserialiseSigDSIGN

instance Typeable a => FromCBOR (Signed a) where
  fromCBOR = UnsafeSigned <$> fromCBOR

instance Typeable a => ToCBOR (Signed a) where
  toCBOR (UnsafeSigned sig) = toCBOR sig

decodeBase16' :: MonadFail f => Text -> f ByteString
decodeBase16' =
  either fail pure . Base16.decode . encodeUtf8
