module Plutus.MerkleTree where

import Data.ByteString.Base16 (encodeBase16)
import qualified Data.Text as Text
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq)

instance Eq Hash where
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) = Text.unpack $ encodeBase16 $ fromBuiltin $ takeByteString 4 bs

hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256

combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')

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

null :: MerkleTree -> Bool
null = \case
  MerkleEmpty -> True
  _ -> False

type Proof = [Hash]

mkProof :: BuiltinByteString -> MerkleTree -> Proof
mkProof _e _tree = []

{-# INLINEABLE member #-}
member :: BuiltinByteString -> Hash -> Proof -> Bool
member _e _root _proof = False

rootHash :: MerkleTree -> Hash
rootHash = \case
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h

fromList :: [BuiltinByteString] -> MerkleTree
fromList =
  foldr insert MerkleEmpty

toList :: MerkleTree -> [BuiltinByteString]
toList = go []
 where
  go es = \case
    MerkleEmpty -> reverse es
    MerkleLeaf _ e -> e : es
    MerkleNode _ n1 n2 -> toList n2 <> toList n1

insert :: BuiltinByteString -> MerkleTree -> MerkleTree
insert e = \case
  MerkleEmpty -> MerkleLeaf (hash e) e
  leaf@(MerkleLeaf h' _) ->
    let h = hash e
        hNode = combineHash h' h
     in MerkleNode hNode leaf (MerkleLeaf h e)
  MerkleNode _ l r ->
    let r' = insert e r
        hNode' = combineHash (rootHash l) (rootHash r')
     in MerkleNode hNode' l r'
