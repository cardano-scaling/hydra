{-# LANGUAGE AllowAmbiguousTypes #-}
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
import Data.Aeson (FromJSONKey (..), ToJSONKey (..), Value (String), withText)
import qualified Data.ByteString.Base16 as Base16
import Test.QuickCheck (vectorOf)

-- NOTE(MB): We probably want to move these common types somewhere else. Putting
-- here to avoid circular dependencies with Hydra.Logic

-- | Identifies a party in a Hydra head.
newtype Party = UnsafeParty (VerKeyDSIGN MockDSIGN)
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

type Committed tx = Map Party (Utxo tx)

data Balance tx = Balance
  { lovelace :: Natural
  , assets :: Map (AssetId tx) Integer
  }

-- * Ledger interface

class
  ( Eq tx
  , Eq (TxId tx)
  , Eq (Utxo tx)
  , FromCBOR tx
  , FromJSON (TxId tx)
  , FromJSON (Utxo tx)
  , FromJSON tx
  , Monoid (Utxo tx)
  , Ord (TxId tx)
  , Show (TxId tx)
  , Show (Utxo tx)
  , Show tx
  , ToCBOR tx
  , ToJSON (TxId tx)
  , ToJSON (Utxo tx)
  , ToJSON tx
  , Typeable (TxId tx)
  , Typeable tx
  ) =>
  Tx tx
  where
  type Utxo tx
  type TxId tx
  type AssetId tx

  txId :: tx -> TxId tx

  balance :: Utxo tx -> Balance tx

data Ledger tx = Ledger
  { applyTransactions :: Utxo tx -> [tx] -> Either ValidationError (Utxo tx)
  , initUtxo :: Utxo tx
  }

canApply :: Ledger tx -> Utxo tx -> tx -> ValidationResult
canApply ledger utxo tx =
  either Invalid (const Valid) $ applyTransactions ledger utxo (pure tx)

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype ValidationError = ValidationError {reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary ValidationError where
  arbitrary = genericArbitrary
