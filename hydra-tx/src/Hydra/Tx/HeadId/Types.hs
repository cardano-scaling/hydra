{-# LANGUAGE DerivingVia #-}

module Hydra.Tx.HeadId.Types where

import Hydra.Prelude
import Hydra.Prelude.Core (HexBytes (..))

-- * HeadId

-- | Uniquely identifies a Hydra Head.
newtype HeadId = UnsafeHeadId ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via HexBytes

instance ToCBOR HeadId where
  toCBOR = genericToCBOR

instance FromCBOR HeadId where
  fromCBOR = genericFromCBOR

-- * HeadSeed

-- | Unique seed to create a 'HeadId'
--
-- XXX: This might actually be the 'HeadId' to the protocol and users? Then the
-- policy id of the cardano-specific implementation (being the result of minting
-- policy + seed) stays internal. A drawback is, that the seed is not such a
-- good "key" to find things about this head on explorers and indexers.
newtype HeadSeed = UnsafeHeadSeed ByteString
  deriving stock (Show, Eq, Ord, Generic)
  deriving (ToJSON, FromJSON) via HexBytes

instance ToCBOR HeadSeed where
  toCBOR = genericToCBOR

instance FromCBOR HeadSeed where
  fromCBOR = genericFromCBOR

instance IsString HeadSeed where
  fromString = UnsafeHeadSeed . fromString

headIdBytes :: HeadId -> ByteString
headIdBytes (UnsafeHeadId bytes) = bytes
