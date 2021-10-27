-- | Module for the on-chain representation of Utxo.
module Hydra.Data.Utxo where

import Hydra.Prelude

import PlutusTx (FromData (..))
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))

newtype Utxo = Utxo BuiltinByteString

instance FromData Utxo where
  fromBuiltinData = fmap Utxo . fromBuiltinData

toByteString :: Utxo -> ByteString
toByteString (Utxo (BuiltinByteString bytes)) = bytes
