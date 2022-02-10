{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A purely functional implementation of MerkleTrees that is suitable for
-- usage on-chain. Note however that the construction of 'MerkleTree' and
-- membership proofs are still expected to happen *off-chain* while only the
-- proof verification should be done on-chain.
--
-- Note that this module is meant to used as a qualified import, for example:
--
-- @
-- import qualified Plutus.MerkleTree as MT
-- @
module Plutus.MerkleTree where

import PlutusPrelude hiding (toList)

import qualified PlutusTx
import PlutusTx.Builtins (divideInteger, subtractInteger)
import qualified PlutusTx.List as List
import PlutusTx.Prelude hiding (toList)

import qualified Data.ByteString.Base16 as Haskell.Base16
import qualified Data.Text as Haskell.Text
import qualified Prelude as Haskell

-- * MerkleTree

-- | A MerkleTree representation, suitable for on-chain manipulation.
-- Construction of the merkle tree shouldn't be done by hand, but via
-- 'fromList'.
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

-- | Construct a 'MerkleTree' from a list of serialized data as
-- 'BuiltinByteString'.
--
-- Note that, while this operation is doable on-chain, it is expensive and
-- preferably done off-chain.
fromList :: [BuiltinByteString] -> MerkleTree
fromList es0 = recursively (length es0) es0
 where
  recursively len =
    \case
      [] ->
        MerkleEmpty
      [e] ->
        MerkleLeaf (hash e) e
      es ->
        let cutoff = len `divideInteger` 2
            (l, r) = (List.take cutoff es, drop cutoff es)
            lnode = recursively cutoff l
            rnode = recursively (len - cutoff) r
         in MerkleNode (combineHash (rootHash lnode) (rootHash rnode)) lnode rnode
{-# INLINEABLE fromList #-}

-- | Deconstruct a 'MerkleTree' back to a list of elements.
--
-- >>> toList (fromList xs) == xs
-- True
toList :: MerkleTree -> [BuiltinByteString]
toList = go
 where
  go = \case
    MerkleEmpty -> []
    MerkleLeaf _ e -> [e]
    MerkleNode _ n1 n2 -> toList n1 <> toList n2
{-# INLINEABLE toList #-}

-- | Obtain the root hash of a 'MerkleTree'. In particular we have:
--
-- >>> (mt == mt') == (rootHash mt == rootHash mt')
-- True
rootHash :: MerkleTree -> Hash
rootHash = \case
  MerkleEmpty -> hash ""
  MerkleLeaf h _ -> h
  MerkleNode h _ _ -> h
{-# INLINEABLE rootHash #-}

-- | Return true if the 'MerkleTree' is empty.
--
-- >>> null mt == (size mt == 0)
-- True
null :: MerkleTree -> Bool
null = \case
  MerkleEmpty -> True
  _ -> False
{-# INLINEABLE null #-}

-- | Total numbers of leaves in the tree.
size :: MerkleTree -> Integer
size = \case
  MerkleEmpty -> 0
  MerkleNode _ l r -> size l + size r
  MerkleLeaf{} -> 1
{-# INLINEABLE size #-}

-- * Proof

-- | A membership 'Proof'. The type is meant to be opaque.
type Proof = [Either Hash Hash]

-- | Construct a membership 'Proof' from an element and a 'MerkleTree'. Returns
-- 'Nothing' if the element isn't a member of the tree to begin with.
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
    MerkleNode _ l r ->
      go (Right (rootHash r) : es) l <|> go (Left (rootHash l) : es) r
{-# INLINEABLE mkProof #-}

-- | Check whether a element is part of a 'MerkleTree' using only its root hash
-- and a 'Proof'. The proof is guaranteed to be in log(n) of the size of the
-- tree, which is why we are interested in such data-structure in the first
-- place.
member :: BuiltinByteString -> Hash -> Proof -> Bool
member e root = go (hash e)
 where
  go root' = \case
    [] -> root' == root
    Left l : q -> go (combineHash l root') q
    Right r : q -> go (combineHash root' r) q
{-# INLINEABLE member #-}

-- * Hash

-- | A type for representing hash digests.
newtype Hash = Hash BuiltinByteString
  deriving (Haskell.Eq)

instance Eq Hash where
  Hash h == Hash h' = h == h'

instance Haskell.Show Hash where
  show (Hash bs) =
    Haskell.Text.unpack $
      Haskell.Base16.encodeBase16 $
        fromBuiltin $
          takeByteString 4 bs

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
hash :: BuiltinByteString -> Hash
hash = Hash . sha2_256
{-# INLINEABLE hash #-}

-- | Combines two hashes digest into a new one. This is effectively a new hash
-- digest of the same length.
combineHash :: Hash -> Hash -> Hash
combineHash (Hash h) (Hash h') = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

-- Internal

-- Plutus Tx version of 'Data.List.drop'.
--
-- TODO: move into plutus
drop :: Integer -> [a] -> [a]
drop n rs | n <= 0 = rs
drop n (_ : xs) = drop (subtractInteger n 1) xs
drop _ [] = []
{-# INLINEABLE drop #-}

-- Template Haskell

PlutusTx.unstableMakeIsData ''Hash
