-- | Module for the on-chain representation of Utxo.
module Hydra.Data.Utxo where

import Hydra.Prelude

import PlutusTx (FromData (..), ToData (..), UnsafeFromData (..))
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

newtype Utxo = Utxo BuiltinByteString

instance FromData Utxo where
  fromBuiltinData = fmap Utxo . fromBuiltinData

instance UnsafeFromData Utxo where
  unsafeFromBuiltinData = Utxo . unsafeFromBuiltinData

instance ToData Utxo where
  toBuiltinData (Utxo bs) = toBuiltinData bs

fromByteString :: ByteString -> Utxo
fromByteString = Utxo . BuiltinByteString

toByteString :: Utxo -> ByteString
toByteString (Utxo (BuiltinByteString bytes)) = bytes
