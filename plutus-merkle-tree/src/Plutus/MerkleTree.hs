module Plutus.MerkleTree where

import PlutusTx.Prelude

data MerkleTree

fromList :: [BuiltinByteString] -> MerkleTree
fromList = traceError "not implemented"

toList :: MerkleTree -> [BuiltinByteString]
toList = traceError "not implemented"
