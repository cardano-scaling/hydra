-- | Wire encodings of the client API: JSON (the default) or native CBOR.
--
-- This module is the single home for the encode/decode primitives shared by
-- the WebSocket server, the HTTP server and the clients; call sites resolve
-- the negotiated 'ApiEncoding' once and stay dispatch-free afterwards.
module Hydra.API.WireFormat where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS

-- | Which wire encoding a client negotiated: JSON (the default) or native
-- CBOR. WebSocket clients opt in via the @?encoding=cbor@ query parameter,
-- HTTP clients via @Accept@ / @Content-Type: application/cbor@ headers.
data ApiEncoding = JsonEncoding | CborEncoding
  deriving stock (Eq, Show)

-- | Encode a value in the given wire encoding.
encodeWire :: (ToJSON a, ToCBOR a) => ApiEncoding -> a -> LBS.ByteString
encodeWire = \case
  JsonEncoding -> Aeson.encode
  CborEncoding -> fromStrict . serialize'

-- | Decode a value in the given wire encoding.
decodeWire :: (FromJSON a, FromCBOR a) => ApiEncoding -> LBS.ByteString -> Either String a
decodeWire = \case
  JsonEncoding -> Aeson.eitherDecode'
  CborEncoding -> first show . decodeFull' . toStrict

-- | Echo received bytes in a readable form: raw text for JSON, base16 for
-- CBOR (which is not valid UTF-8).
describeWire :: ApiEncoding -> LBS.ByteString -> Text
describeWire = \case
  JsonEncoding -> decodeUtf8With lenientDecode . toStrict
  CborEncoding -> encodeBase16 . toStrict
