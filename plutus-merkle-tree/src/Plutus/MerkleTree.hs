module Plutus.MerkleTree where

import PlutusTx.Prelude
import qualified Prelude as Haskell

type Hash = BuiltinByteString

data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash MerkleTree MerkleTree
  | MerkleLeaf Hash BuiltinByteString
  deriving (Haskell.Eq, Haskell.Show)

instance Eq MerkleTree where
  MerkleEmpty == MerkleEmpty = True
  (MerkleLeaf h0 _) == (MerkleLeaf h1 _) = h0 == h1
  (MerkleNode h0 _ _) == (MerkleNode h1 _ _) = h0 == h1
  _ == _ = False

fromList :: [BuiltinByteString] -> MerkleTree
fromList =
  foldr insert MerkleEmpty

toList :: MerkleTree -> [BuiltinByteString]
toList = go []
 where
  go xs = \case
    MerkleEmpty -> xs
    _ -> xs

insert :: BuiltinByteString -> MerkleTree -> MerkleTree
insert e = \case
  MerkleEmpty -> MerkleLeaf (sha2_256 e) e
  leaf@(MerkleLeaf h' _) ->
    let h = sha2_256 e
        hNode = sha2_256 (appendByteString h' h)
     in MerkleNode hNode leaf (MerkleLeaf h e)
  _ -> MerkleEmpty
