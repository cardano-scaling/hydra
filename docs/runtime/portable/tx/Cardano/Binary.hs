{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Cardano.Binary where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Data.ByteString (ByteString)
import Prelude

class ToCBOR a where
  toCBOR :: a -> Encoding

class FromCBOR a where
  fromCBOR :: Decoder s a

instance {-# OVERLAPPABLE #-} ToCBOR a where
  toCBOR = genericToCBOR

instance {-# OVERLAPPABLE #-} FromCBOR a where
  fromCBOR = genericFromCBOR

genericToCBOR :: a -> Encoding
genericToCBOR _ = error "CBOR persistence is unavailable in the playground"

genericFromCBOR :: Decoder s a
genericFromCBOR = fail "CBOR persistence is unavailable in the playground"

serialize' :: ToCBOR a => a -> ByteString
serialize' _ = error "Network wire encoding is unavailable in the playground"
