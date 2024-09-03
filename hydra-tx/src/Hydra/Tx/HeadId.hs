{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.HeadId where

import Hydra.Prelude

import Hydra.Cardano.Api (
  HasTypeProxy (..),
  PolicyId,
  SerialiseAsRawBytes (..),
  UsingRawBytesHex (..),
 )
import PlutusLedgerApi.V2 (CurrencySymbol (..), toBuiltin)

-- | Uniquely identifies a Hydra Head.
newtype HeadId = UnsafeHeadId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadId)
  deriving newtype (FromCBOR, ToCBOR)

instance SerialiseAsRawBytes HeadId where
  serialiseToRawBytes (UnsafeHeadId bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadId

instance HasTypeProxy HeadId where
  data AsType HeadId = AsHeadId
  proxyToAsType _ = AsHeadId

-- | Unique seed to create a 'HeadId'
--
-- XXX: This might actually be the 'HeadId' to the protocol and users? Then the
-- policy id of the cardano-specific implementation (being the result of minting
-- policy + seed) stays internal. A drawback is, that the seed is not such a
-- good "key" to find things about this head on explorers and indexers.
newtype HeadSeed = UnsafeHeadSeed ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via (UsingRawBytesHex HeadSeed)

instance SerialiseAsRawBytes HeadSeed where
  serialiseToRawBytes (UnsafeHeadSeed bytes) = bytes
  deserialiseFromRawBytes _ = Right . UnsafeHeadSeed

instance HasTypeProxy HeadSeed where
  data AsType HeadSeed = AsHeadSeed
  proxyToAsType _ = AsHeadSeed

headIdToCurrencySymbol :: HeadId -> CurrencySymbol
headIdToCurrencySymbol (UnsafeHeadId headId) = CurrencySymbol (toBuiltin headId)

mkHeadId :: PolicyId -> HeadId
mkHeadId = UnsafeHeadId . serialiseToRawBytes
