{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan 'ToCBOR' / 'FromCBOR' instances for third-party types that appear
-- on the CBOR-encoded API surface.
module Hydra.CBOR.Orphans () where

import Hydra.Prelude

import Codec.CBOR.JSON (decodeValue, encodeValue)
import Data.Aeson qualified as Aeson
import Network.Socket (PortNumber)

-- | 'Aeson.Value' is CBOR-encoded using the standard JSON-in-CBOR mapping
-- from cborg-json. This is used to serve the pre-rendered configuration
-- document at GET /config; 'FromCBOR' is its client-side decode counterpart.
--
-- NOTE: Non-integral JSON numbers round-trip through 'Double', which may lose
-- precision; integers are exact.
instance ToCBOR Aeson.Value where
  toCBOR = encodeValue

instance FromCBOR Aeson.Value where
  fromCBOR = decodeValue False

instance ToCBOR PortNumber where
  toCBOR = toCBOR . (fromIntegral :: PortNumber -> Word16)

instance FromCBOR PortNumber where
  fromCBOR = (fromIntegral :: Word16 -> PortNumber) <$> fromCBOR
