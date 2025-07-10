{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.HeadId where

import Hydra.Prelude

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Hydra.Cardano.Api (
  HasTypeProxy (..),
  PolicyId,
  SerialiseAsRawBytes (..),
  TxIn,
  UsingRawBytesHex (..),
  fromPlutusCurrencySymbol,
 )
import PlutusLedgerApi.V3 (CurrencySymbol (..), toBuiltin)
import Test.QuickCheck (vectorOf)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

-- * HeadId

-- | Uniquely identifies a Hydra Head.
newtype HeadId = UnsafeHeadId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (UnsafeHeadId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadId

instance ToCBOR HeadId where
  toCBOR = toCBOR . serialiseToRawBytes

instance FromCBOR HeadId where
  fromCBOR = do
    bs <- fromCBOR
    case deserialiseFromRawBytes AsHeadId bs of
      Left err -> fail (show err)
      Right v -> pure v

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

instance Arbitrary HeadId where
  arbitrary = UnsafeHeadId . BS.pack <$> vectorOf 16 arbitrary

currencySymbolToHeadId :: MonadFail m => CurrencySymbol -> m HeadId
currencySymbolToHeadId = fmap mkHeadId . fromPlutusCurrencySymbol

headIdToPolicyId :: MonadFail m => HeadId -> m PolicyId
headIdToPolicyId = fromPlutusCurrencySymbol . headIdToCurrencySymbol

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (UnsafeHeadId headId) = CurrencySymbol (toBuiltin headId)

mkHeadId :: PolicyId -> HeadId
mkHeadId = UnsafeHeadId . serialiseToRawBytes

-- * HeadSeed

-- | Unique seed to create a 'HeadId'
--
-- XXX: This might actually be the 'HeadId' to the protocol and users? Then the
-- policy id of the cardano-specific implementation (being the result of minting
-- policy + seed) stays internal. A drawback is, that the seed is not such a
-- good "key" to find things about this head on explorers and indexers.
newtype HeadSeed = UnsafeHeadSeed ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadSeed)
  deriving newtype (ToCBOR, FromCBOR)

instance IsString HeadSeed where
  fromString = UnsafeHeadSeed . fromString

instance SerialiseAsRawBytes HeadSeed where
  serialiseToRawBytes (UnsafeHeadSeed bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadSeed

instance HasTypeProxy HeadSeed where
  data AsType HeadSeed = AsHeadSeed
  proxyToAsType _ = AsHeadSeed

instance Arbitrary HeadSeed where
  arbitrary = UnsafeHeadSeed . BS.pack <$> vectorOf 16 arbitrary

headSeedToTxIn :: MonadFail m => HeadSeed -> m TxIn
headSeedToTxIn (UnsafeHeadSeed bytes) =
  case Aeson.decodeStrict bytes of
    Nothing -> fail $ "Failed to decode HeadSeed " <> show bytes
    Just txIn -> pure txIn

txInToHeadSeed :: TxIn -> HeadSeed
txInToHeadSeed txin = UnsafeHeadSeed $ toStrict $ Aeson.encode txin
