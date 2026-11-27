module Hydra.Tx.Snapshot.Encoding where

import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Hydra.Prelude
import PlutusLedgerApi.V3 (fromBuiltin, toBuiltin, toData)
import PlutusTx.Builtins (sha2_256)

encodeBytes :: ByteString -> LBS.ByteString
encodeBytes = serialise . toData . toBuiltin

encodeNumber :: Integer -> LBS.ByteString
encodeNumber = serialise . toData . toBuiltin

hashBytes :: ByteString -> ByteString
hashBytes = fromBuiltin . sha2_256 . toBuiltin
