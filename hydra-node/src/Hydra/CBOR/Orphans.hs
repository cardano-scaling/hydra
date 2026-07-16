{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan 'ToCBOR' / 'FromCBOR' instances for third-party types that appear
-- in CBOR-encoded hydra-node data (logs and API messages).
module Hydra.CBOR.Orphans () where

import Hydra.Prelude

import Codec.CBOR.JSON (decodeValue, encodeValue)
import Data.Aeson qualified as Aeson
import Data.IP (IP)
import Hydra.Cardano.Api (File (..))
import Network.Socket (PortNumber)

-- | 'Aeson.Value' is CBOR-encoded using the standard JSON-in-CBOR mapping
-- from cborg-json. This is used for the few log types which carry raw JSON
-- payloads (e.g. 'Hydra.API.APIServerLog.APIServerLog').
--
-- NOTE: Non-integral JSON numbers round-trip through 'Double', which may lose
-- precision; integers are exact.
instance ToCBOR Aeson.Value where
  toCBOR = encodeValue

instance FromCBOR Aeson.Value where
  fromCBOR = decodeValue False

-- | Encoded as its textual representation (e.g. @127.0.0.1@).
instance ToCBOR IP where
  toCBOR = toCBOR . (show :: IP -> Text)

instance FromCBOR IP where
  fromCBOR = do
    t :: Text <- fromCBOR
    case readMaybe (toString t) of
      Nothing -> fail $ "failed to parse IP address from " <> show t
      Just ip -> pure ip

instance ToCBOR PortNumber where
  toCBOR = toCBOR . (fromIntegral :: PortNumber -> Word16)

instance FromCBOR PortNumber where
  fromCBOR = (fromIntegral :: Word16 -> PortNumber) <$> fromCBOR

-- | Encoded as the file path text (e.g. for 'Hydra.Cardano.Api.SocketPath').
instance (Typeable content, Typeable direction) => ToCBOR (File content direction) where
  toCBOR (File fp) = toCBOR (toText fp)

instance (Typeable content, Typeable direction) => FromCBOR (File content direction) where
  fromCBOR = File . toString <$> fromCBOR @Text
