{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Ledger where

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
import Data.Aeson (FromJSONKey (..), ToJSONKey (..), withText)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Test.QuickCheck (vectorOf)

-- NOTE(MB): We probably want to move these common types somewhere else. Putting
-- here to avoid circular dependencies with Hydra.Logic

-- | Identifies a party in a Hydra head.
newtype Party = UnsafeParty (VerKeyDSIGN MockDSIGN)
  deriving stock (Eq, Generic)
  deriving newtype (Show, Read, Num)

deriving instance Read (VerKeyDSIGN MockDSIGN)

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

newtype Signed a = UnsafeSigned (SigDSIGN MockDSIGN)
  deriving (Eq, Show)

instance Arbitrary (Signed a) where
  arbitrary = do
    key <- genKeyDSIGN . mkSeedFromBytes . fromList <$> vectorOf 8 arbitrary
    a <- arbitrary @ByteString
    pure . UnsafeSigned $ signDSIGN () a key

instance ToJSON a => ToJSON (Signed a) where
  toJSON (UnsafeSigned sig) = toJSON . encodeBase16 . rawSerialiseSigDSIGN $ sig

instance FromJSON a => FromJSON (Signed a) where
  parseJSON = withText "Signed" $ decodeBase16' >=> deserialiseSigned
   where
    decodeBase16' :: MonadFail f => Text -> f ByteString
    decodeBase16' =
      either (fail . show) pure . decodeBase16 . encodeUtf8

    deserialiseSigned :: MonadFail f => ByteString -> f (Signed a)
    deserialiseSigned =
      let err = "Unable to decode signature"
       in maybe (fail err) (pure . UnsafeSigned) . rawDeserialiseSigDSIGN

instance Typeable a => FromCBOR (Signed a) where
  fromCBOR = UnsafeSigned <$> fromCBOR

instance Typeable a => ToCBOR (Signed a) where
  toCBOR (UnsafeSigned sig) = toCBOR sig

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

-- TODO:
-- deriving instance Read (SigDSIGN MockDSIGN)

type Committed tx = Map Party (UTxO tx)

-- * Ledger interface

class
  ( Eq tx
  , Eq (UTxO tx)
  , Eq (TxId tx)
  , Show tx
  , Show (UTxO tx)
  , Show (TxId tx)
  , Read tx
  , Read (UTxO tx)
  , Read (TxId tx)
  , Monoid (UTxO tx)
  , Typeable tx
  , Typeable (TxId tx)
  , FromJSON tx
  , FromJSON (UTxO tx)
  , FromJSON (TxId tx)
  , ToJSON tx
  , ToJSON (UTxO tx)
  , ToJSON (TxId tx)
  , Ord (TxId tx)
  ) =>
  Tx tx
  where
  type UTxO tx
  type TxId tx

  txId :: tx -> TxId tx

data Ledger tx = Ledger
  { applyTransactions :: UTxO tx -> [tx] -> Either ValidationError (UTxO tx)
  , initUTxO :: UTxO tx
  }

canApply :: Ledger tx -> UTxO tx -> tx -> ValidationResult
canApply ledger utxo tx =
  either Invalid (const Valid) $ applyTransactions ledger utxo (pure tx)

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ValidationError
  = ValidationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ValidationError where
  arbitrary = pure ValidationError
