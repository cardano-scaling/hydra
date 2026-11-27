module Hydra.Tx.Snapshot.Encoding where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as LBS
import Hydra.Prelude

encodeBytes :: ByteString -> LBS.ByteString
encodeBytes bytes = encode [encodeBase16 bytes]

encodeNumber :: Integer -> LBS.ByteString
encodeNumber number = encode [number]

hashBytes :: ByteString -> ByteString
hashBytes = id
