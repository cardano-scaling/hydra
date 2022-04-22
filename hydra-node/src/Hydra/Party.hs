-- | Types and functions revolving around a Hydra 'Party'. That is, a
-- participant in a Hydra Head, which signs transactions or snapshots in the
-- Hydra protocol.
module Hydra.Party where

import Hydra.Prelude hiding (show)

import Data.Aeson (ToJSONKey)
import Data.Aeson.Types (FromJSONKey)
import Hydra.Crypto (hashVerificationKey)
import qualified Hydra.Crypto as Hydra

-- | Identifies a party in a Hydra head by it's 'VerificationKey'.
newtype Party = Party {vkey :: Hydra.VerificationKey}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, FromJSONKey, ToJSONKey)

-- REVIEW: Do we really want to define Ord or also use unordered-containers
-- based on Hashable?
instance Ord Party where
  Party{vkey = a} <= Party{vkey = b} =
    hashVerificationKey a <= hashVerificationKey b

instance Arbitrary Party where
  arbitrary = Party <$> arbitrary

instance FromCBOR Party where
  fromCBOR = Party <$> fromCBOR

instance ToCBOR Party where
  toCBOR Party{vkey} = toCBOR vkey

-- | Generate a 'Party' from a 'ByteString' seed.
generateParty :: ByteString -> Party
generateParty =
  Party . Hydra.generateVerificationKey

deriveParty :: Hydra.SigningKey -> Party
deriveParty = Party . Hydra.deriveVerificationKey

-- * Hydra keys

-- TODO(SN): move into a Hydra.Crypto module or so

-- type VerificationKey = VerKeyDSIGN MockDSIGN

-- instance ToJSON VerificationKey where
--   toJSON = String . showVerificationKey

-- instance FromJSON VerificationKey where
--   parseJSON = withText "VerificationKey" $ decodeBase16' >=> deserialiseKey
--    where
--     deserialiseKey =
--       maybe (fail "Unable to deserialize VerificationKey") pure . rawDeserialiseVerKeyDSIGN

-- showVerificationKey :: VerificationKey -> Text
-- showVerificationKey = decodeUtf8 . Base16.encode . rawSerialiseVerKeyDSIGN

-- type SigningKey = SignKeyDSIGN MockDSIGN

-- generateKey :: Integer -> SigningKey
-- generateKey = fromInteger

-- verify :: SignableRepresentation a => Signed a -> Party -> a -> Bool
-- verify signed Party{vkey = (VerKeyMockDSIGN wo)} msg =
--   sign (SignKeyMockDSIGN wo) msg == signed

-- -- | Naiive mult-signatures.
-- newtype MultiSigned a = MultiSigned {multiSignature :: [Signed a]}
--   deriving stock (Show, Generic, Eq)
--   deriving newtype (ToCBOR, Semigroup, Monoid)
--   deriving anyclass (FromJSON, ToJSON)

-- instance Arbitrary (MultiSigned a) where
--   arbitrary = genericArbitrary

-- aggregate :: [Signed a] -> MultiSigned a
-- aggregate = MultiSigned

-- -- | Signature of 'a'
-- newtype Signed a = UnsafeSigned ByteString
--   deriving (Eq, Show)

-- instance Arbitrary (Signed a) where
--   arbitrary = do
--     key <- genKeyDSIGN . mkSeedFromBytes . fromList <$> vectorOf 8 arbitrary
--     a <- arbitrary @ByteString
--     pure . UnsafeSigned $ coerce $ sign key a

-- instance ToJSON a => ToJSON (Signed a) where
--   toJSON (UnsafeSigned sig) = String . decodeUtf8 . Base16.encode $ sig

-- instance FromJSON a => FromJSON (Signed a) where
--   parseJSON = withText "Signed" $ decodeBase16' >=> pure . UnsafeSigned

-- instance Typeable a => FromCBOR (Signed a) where
--   fromCBOR = UnsafeSigned <$> fromCBOR

-- instance Typeable a => ToCBOR (Signed a) where
--   toCBOR (UnsafeSigned sig) = toCBOR sig

-- -- * Helpers

-- decodeBase16' :: MonadFail f => Text -> f ByteString
-- decodeBase16' =
--   either fail pure . Base16.decode . encodeUtf8
