{-# LANGUAGE TemplateHaskell #-}

module Plutus.MerkleTree where

import Data.ByteString.Base16 (encodeBase16)
import qualified Data.Text as Text
import PlutusPrelude ((<|>))
import qualified PlutusTx
import PlutusTx.Builtins (divideInteger, subtractInteger)
import qualified PlutusTx.List as List
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq)

PlutusTx.unstableMakeIsData ''Hash

instance Eq Hash where
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) = Text.unpack $ encodeBase16 $ fromBuiltin $ takeByteString 4 bs

hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256
{-# INLINEABLE hash #-}

combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash Integer MerkleTree MerkleTree
  | MerkleLeaf Hash BuiltinByteString
  deriving (Haskell.Eq, Haskell.Show)

instance Eq MerkleTree where
  MerkleEmpty == MerkleEmpty = True
  (MerkleLeaf h0 _) == (MerkleLeaf h1 _) = h0 == h1
  (MerkleNode h0 _ _ _) == (MerkleNode h1 _ _ _) = h0 == h1
  _ == _ = False

size :: MerkleTree -> Integer
size = \case
  MerkleEmpty -> 0
  MerkleNode _ sz _ _ -> sz
  MerkleLeaf{} -> 1
{-# INLINEABLE size #-}

null :: MerkleTree -> Bool
null = \case
  MerkleEmpty -> True
  _ -> False
{-# INLINEABLE null #-}

type Proof = [Either Hash Hash]

mkProof :: BuiltinByteString -> MerkleTree -> Maybe Proof
mkProof e = go []
 where
  he = hash e
  go es = \case
    MerkleEmpty -> Nothing
    MerkleLeaf h _ ->
      if h == he
        then Just es
        else Nothing
    MerkleNode _ _ l r ->
      go (Right (rootHash r) : es) l <|> go (Left (rootHash l) : es) r
{-# INLINEABLE mkProof #-}

member :: BuiltinByteString -> Hash -> Proof -> Bool
member e root = go (hash e)
 where
  go root' = \case
    [] -> root' == root
    Left l : q -> go (combineHash l root') q
    Right r : q -> go (combineHash root' r) q
{-# INLINEABLE member #-}

rootHash :: MerkleTree -> Hash
rootHash = \case
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ _ -> h
{-# INLINEABLE rootHash #-}

fromList :: [BuiltinByteString] -> MerkleTree
fromList =
  \case
    [] -> MerkleEmpty
    [e] -> MerkleLeaf (hash e) e
    es ->
      let len = length es
          cutoff = len `divideInteger` 2
          (l, r) = (List.take cutoff es, drop cutoff es)
          lnode = fromList l
          rnode = fromList r
       in MerkleNode (combineHash (rootHash lnode) (rootHash rnode)) len lnode rnode
{-# INLINEABLE fromList #-}

-- | Plutus Tx version of 'Data.List.drop'.
--
-- TODO: move into plutus
drop :: Integer -> [a] -> [a]
drop n rs | n <= 0 = rs
drop n (_ : xs) = drop (subtractInteger n 1) xs
drop _ [] = []
{-# INLINEABLE drop #-}

toList :: MerkleTree -> [BuiltinByteString]
toList = go
 where
  go = \case
    MerkleEmpty -> []
    MerkleLeaf _ e -> [e]
    MerkleNode _ _ n1 n2 -> toList n1 <> toList n2
