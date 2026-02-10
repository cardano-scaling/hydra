{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.HeadId where

import "hydra-prelude" Hydra.Prelude

import "aeson" Data.Aeson qualified as Aeson
import "hydra-cardano-api" Hydra.Cardano.Api (
  HasTypeProxy (..),
  PolicyId,
  SerialiseAsRawBytes (..),
  TxIn,
  UsingRawBytesHex (..),
  fromPlutusCurrencySymbol,
 )
import "plutus-ledger-api" PlutusLedgerApi.V3 (CurrencySymbol (..), toBuiltin)

-- * HeadId

-- | Uniquely identifies a Hydra Head.
newtype HeadId = UnsafeHeadId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (UnsafeHeadId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadId

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

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

instance IsString HeadSeed where
  fromString = UnsafeHeadSeed . fromString

instance SerialiseAsRawBytes HeadSeed where
  serialiseToRawBytes (UnsafeHeadSeed bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadSeed

instance HasTypeProxy HeadSeed where
  data AsType HeadSeed = AsHeadSeed
  proxyToAsType _ = AsHeadSeed

headSeedToTxIn :: MonadFail m => HeadSeed -> m TxIn
headSeedToTxIn (UnsafeHeadSeed bytes) =
  case Aeson.decodeStrict bytes of
    Nothing -> fail $ "Failed to decode HeadSeed " <> show bytes
    Just txIn -> pure txIn

txInToHeadSeed :: TxIn -> HeadSeed
txInToHeadSeed txin = UnsafeHeadSeed $ toStrict $ Aeson.encode txin
