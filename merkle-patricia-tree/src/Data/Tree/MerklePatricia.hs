{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK prune #-}

-- | An implementation of a Merkle-Patricia tree, with additions and deletions
-- of elements from root hashes.
--
-- Note that this module is not meant to be used as a handful data-structures
-- for classic operations; Those operations are better done with a `Map` or a
-- `HashMap` containers. A 'MerklePatriciaTree' can however be leveraged to
-- produce proofs which sizes are logarithmic in the size of the tree, and which
-- can be verified using only a root hash. This makes is feasible to embed
-- verifiable operations on an existing tree with only tiny proofs.
module Data.Tree.MerklePatricia (
  -- * Types
  MerklePatriciaTree,
  Proof,

  -- * Constructing
  fromList,
  toList,

  -- * Analyzing
  root,
  null,
  size,

  -- * Proof

  -- ** Constructing
  mkProof,
  unsafeMkProof,

  -- ** Using
  member,
  add,
  delete,

  -- * Debugging
  pretty,

  -- * Re-Exports

  -- | - 'Serialise'
  Serialise,
  -- | - 'HashAlgorithm'
  HashAlgorithm,
  -- | - 'Blake2b_160'
  Blake2b_160,
  -- | - 'Blake2b_224'
  Blake2b_224,
  -- | - 'Blake2b_256'
  Blake2b_256,
  -- Internals
  stripCommonPrefix,
  dropPrefix,
) where

import Prelude hiding (null)

-- import Data.Function
-- import Debug.Trace

import Codec.Serialise (Serialise, serialise)
import Control.Arrow (first, second)
import Control.Monad (guard, join)
import Crypto.Hash (Digest, hash, hashlazy)
import Crypto.Hash.Algorithms (Blake2b_160, Blake2b_224, Blake2b_256)
import Crypto.Hash.IO (HashAlgorithm)
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.List (foldl', intercalate, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import GHC.Stack (HasCallStack)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- * Type

type Prefix = String

-- | An opaque 'MerklePatriciaTree', parameterized by a hash algorithm and a
-- type for its values.
data MerklePatriciaTree alg a
  = Leaf Prefix (Digest alg) a
  | Node Prefix (Digest alg) [(Char, MerklePatriciaTree alg a)]
  | Empty (Digest alg)
  deriving (Show)

instance HashAlgorithm alg => Eq (MerklePatriciaTree alg a) where
  (root -> a) == (root -> b) = a == b

-- | An opaque 'Proof' which can be used to prove membership or atomic
-- operations such as 'add' or 'delete'
newtype Proof alg = Proof {unProof :: [(Prefix, [(Char, Digest alg)])]}
  deriving stock (Show)

--
-- Constructing
--

-- | Construct a Merkle-Patricia Tree from a list of (key, value) pairs.
--
-- This operation is __expensive__ for it computes intermediate hashes of each
-- nodes and leaves of the tree.
--
-- __properties:__
--
--     >>> ∀xs. toList (fromList xs) == xs
fromList ::
  forall alg a.
  (HashAlgorithm alg, Serialise a) =>
  [(String, a)] ->
  MerklePatriciaTree alg a
fromList = go ""
 where
  alphabet = ['0' .. '9'] ++ ['a' .. 'f']
  go c = \case
    [] ->
      Empty emptyRoot
    [(pre, a)] ->
      Leaf pre (hashLeaf (c ++ pre) a) a
    xs ->
      let (pre, xs') = stripCommonPrefix xs
          children = mapNonEmptyWithIndex (\i -> go [i] $ mapMaybe (project i) xs') alphabet
       in Node pre (hashNode (c ++ pre) $ second root <$> children) children

  mapNonEmptyWithIndex ::
    (i -> MerklePatriciaTree alg a) ->
    [i] ->
    [(i, MerklePatriciaTree alg a)]
  mapNonEmptyWithIndex fn =
    mapMaybe $ \i -> case fn i of
      Empty{} -> Nothing
      mpt -> Just (i, mpt)

  project :: Char -> (String, a) -> Maybe (String, a)
  project i = \case
    (h : q, a) | h == i -> Just (q, a)
    _ -> Nothing

-- | Convert a Merkle-Patricia Tree into a list of (key, value) pairs.
--
-- __properties:__
--
--     >>> ∀xs. toList (fromList xs) == xs
toList :: forall alg a. MerklePatriciaTree alg a -> [(String, a)]
toList = \case
  Empty{} ->
    []
  Leaf pre _ a ->
    [(pre, a)]
  Node pre _ children ->
    mconcat [first ((pre <> [c]) <>) <$> toList mpt | (c, mpt) <- children]

--
-- Analyzing
--

-- | Access the root hash of the 'MerklePatriciaTree'. Two trees with the same
-- root hash are equal.
root :: forall alg a. MerklePatriciaTree alg a -> Digest alg
root = \case
  Leaf _ h _ -> h
  Node _ h _ -> h
  Empty h -> h

-- | True if the given 'MerklePatriciaTree' is empty.
--
-- __properties:__
--
--     >>> ∀mpt. (size mpt == 0) == (null mpt)
--     True
null :: MerklePatriciaTree alg a -> Bool
null = \case
  Empty{} -> True
  _ -> False

-- | Count the number of values stored in the tree (that is, number of non-empty
-- leaves). Note that this does not necessarily correspond to the number of nodes
-- in the tree.
--
-- We have however the following property:
--
-- __properties:__
--
--     >>> ∀xs. size (fromList xs) == length xs
--     True
size :: MerklePatriciaTree alg a -> Int
size = \case
  Empty{} ->
    0
  Leaf{} ->
    1
  Node _ _ children ->
    foldl' (\sz (_, mpt) -> sz + size mpt) 0 children

--
-- Proof
--

-- | Construct a proof for a given `key` of the 'MerklePatriciaTree'. Returns
-- 'Nothing' if the key does not exists. The proof is compact and does not
-- information embedded in the element itself. This means that, the
-- corresponding (key, value) is necessary for any use of the proof.
mkProof ::
  forall alg a.
  (HashAlgorithm alg, Serialise a) =>
  String ->
  -- | A key pointing to one element in the tree
  MerklePatriciaTree alg a ->
  -- | The target tree
  Maybe (Proof alg)
mkProof ref = \case
  Empty{} ->
    Nothing
  Leaf pre _ _ ->
    if pre == ref
      then Just $ Proof [(pre, [])]
      else Nothing
  Node pre _ children ->
    case stripPrefix pre ref of
      Just (c : ref') -> do
        child <- lookup c children
        let others = flip mapMaybe children $ \(i, mpt) ->
              if i == c then Nothing else Just (i, root mpt)
        Proof proof <- mkProof ref' child
        pure $ Proof $ (pre, others) : proof
      _ ->
        Nothing

-- An unsafe version of 'mkProof', for testing only.
unsafeMkProof ::
  forall alg a.
  (HashAlgorithm alg, Serialise a, HasCallStack) =>
  String ->
  MerklePatriciaTree alg a ->
  Proof alg
unsafeMkProof ref =
  fromMaybe (error "unsafeMkProof: Nothing") . mkProof ref

-- | Check whether an element exists in the 'MerklePatriciaTree'.
--
-- __properties:__
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt. member (k, v) (root mpt) <$> mkProof k mpt
--     Just True
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, ∃k' / k ≠ k'. member (k', v) (root mpt) <$> mkProof k mpt
--     Just False
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, ∃v' / v ≠ v'. member (k, v') (root mpt) <$> mkProof k mpt
--     Just False
member ::
  forall alg a.
  (Serialise a, HashAlgorithm alg) =>
  (String, a) ->
  -- | The (key, value) to check
  Digest alg ->
  -- | The root of the target 'MerklePatriciaTree'
  Proof alg ->
  -- | A proof constructed from the original 'MerklePatriciaTree'
  Bool
member el newRoot proofs = fromMaybe False $ do
  (newRoot', _oldRoot) <- walkBackwards el proofs
  pure $ newRoot == newRoot'

-- | Check whether an element exists in the 'MerklePatriciaTree', yielding the
-- root hash of the new tree.
--
-- __properties:__
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, add (k, v) (root (mpt \ (k,v))) <$> mkProof k mpt
--     Just (root mpt)
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, ∃v' / v ≠ v'. add (k, v') (root (mpt \ (k,v))) <$> mkProof k mpt
--     Nothing
add ::
  forall alg a.
  (Serialise a, HashAlgorithm alg) =>
  (String, a) ->
  -- | The (key, value) to add
  Digest alg ->
  -- | The root of the 'MerklePatriciaTree' into which to add the pair
  Proof alg ->
  -- | A proof constructed from the __final__ 'MerklePatriciaTree'
  Maybe (Digest alg)
add el oldRoot proofs = do
  (newRoot, oldRoot') <- walkBackwards el proofs
  newRoot <$ guard (oldRoot' == oldRoot)

-- | Delete an element from a 'MerklePatriciaTree', yielding the root hash of
-- the new tree.
--
-- __properties:__
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, delete (k, v) (root mpt) <$> mkProof k mpt
--     Just (root (mpt \ (k, v)))
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, ∃k' / k ≠ k'. delete (k', v) (root mpt) <$> mkProof k mpt
--     Nothing
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, ∃v' / v ≠ v'. delete (k, v') (root mpt) <$> mkProof k mpt
--     Nothing
--
--     >>> ∀mpt, ∀(k, v) ∈ mpt, let Just p = mkProof k mpt. add (k, v) <$> delete (k, v) (root mpt) p <*> pure p
--     Just (root mpt)
delete ::
  forall alg a.
  (Serialise a, HashAlgorithm alg) =>
  (String, a) ->
  -- | The (key, value) to delete
  Digest alg ->
  -- | The root of the 'MerklePatriciaTree' from which to remove the pair
  Proof alg ->
  -- | A proof constructed from the __original__ 'MerklePatriciaTree'
  Maybe (Digest alg)
delete el newRoot proofs = do
  (newRoot', oldRoot) <- walkBackwards el proofs
  oldRoot <$ guard (newRoot' == newRoot)

--
-- Debugging
--

-- | Pretty-print a 'MerklePatriciaTree' in ascii, such that levels are aligned
-- horizontally (one identation-level per level in the tree), and prefixes are
-- positioned such that they are visually easy to follow.
pretty :: forall alg a. (Show a) => MerklePatriciaTree alg a -> String
pretty mpt =
  combine (hashes mpt) (body 0 mpt)
 where
  indent :: Int -> String -> String
  indent n str =
    intercalate "\n" ((replicate n ' ' <>) <$> lines str)

  combine :: [Digest alg] -> String -> String
  combine hs bs =
    intercalate "\n" $ flip fmap (zip hs (lines bs)) $ \(h, b) -> shortHash h <> ": " <> b

  hashes = \case
    Empty h -> [h]
    Leaf _ h _ -> [h]
    Node _ h children -> let hs = concatMap (hashes . snd) children in h : hs

  body n = \case
    Empty{} ->
      "ø"
    Leaf pre _ a ->
      pre <> " ↦ " <> take 5 (show a)
    Node pre _ children ->
      let t = n + length pre - 1
       in pre <> join ["\n" <> indent t (prettyElement 2 el) | el <- children]

  prettyElement :: Int -> (Char, MerklePatriciaTree alg a) -> String
  prettyElement n (i, sub) =
    ['\\', i] <> body n sub

--
-- Helper / Internal
--

-- The root hash of every empty tree.
emptyRoot :: forall alg. HashAlgorithm alg => Digest alg
emptyRoot = hashNode "" []

-- Walks a proof backwards and returns a root with element and root without element.
--
-- The 'root with element' is calculated by walking the proofs from the leaf,
-- reconstructing the tree as the proof is walked backwards. This can be
-- compared to a root hash to prove existence.
--
-- The 'root without element' is calculated by walking the proofs from the leaf,
-- but, discarding the leaf itself. The resulting root can be compared to a
-- known root hash to see whether the proof is a direct extension of a given
-- MPT.
walkBackwards ::
  forall alg a.
  (Serialise a, HashAlgorithm alg) =>
  (String, a) ->
  -- | leaf (key, value) to lookup
  Proof alg ->
  -- | Proof of existence of the (key, value) into the MPT
  Maybe (Digest alg, Digest alg)
walkBackwards (reverse -> ref0, a) (reverse . unProof -> proofs0) =
  -- The proof contains all the (other) nodes in the path from the root to the leaf
  -- containing the element 'a'. Thus, checking membership boils down to trying
  -- to re-construct the root hash from the leaf.
  --
  -- To do so, the proof is thereby processed in reverse order, and so is the
  -- prefix. Note also that, since the proof does not include the element
  -- itself, or hashes of the nodes containing the element itself these are
  -- re-computed from the provided element and reference.
  case proofs0 of
    ((pre, []) : proofs) ->
      go (hashLeaf pre a) Nothing (dropPrefix (reverse pre) ref0) proofs
    _ ->
      -- This proof is ill-constructed: the last element of a proof is
      -- necessarily a leaf. Anything else isn't a valid proof.
      Nothing
 where
  -- Once the prefix is entirely processed, the remaining proof is expected to
  -- be empty (as it was constructed with exactly the right number of elements)
  go newRoot oldRoot [] = \case
    [] ->
      Just (newRoot, fromMaybe emptyRoot oldRoot)
    _ ->
      Nothing
  -- This is the main part, which walks backward from the leaf through each stage
  -- of the tree. In this branch, the ref is necessarily at least one digit
  -- (which corresponds to the branch choice of the MPT). The current stage may
  -- also have a prefix. Each call to this function consumes both the digit and
  -- the prefix from the reference. The proof contains all _other_ hashes and
  -- digits for this level, the missing one being re-computed from the provided
  -- element.
  go h prev (c : ref) = \case
    [] -> Nothing
    ((pre, roots) : proofs) ->
      let oldHash =
            case [(c, hashPrefix [c] prevHash) | Just prevHash <- [prev]] ++ roots of
              [(_, single)] ->
                hashPrefix pre single
              children ->
                hashNode pre children
          newHash =
            hashNode pre $ (c, hashPrefix [c] h) : roots
       in go newHash (Just oldHash) (dropPrefix (reverse pre) ref) proofs

-- Computes the new hash of the given prefixed element. Note that this computes
-- one hash per character in the prefix and is necessary for the `add` and
-- `remove` operation to work. Indeed, since adding or removing elements in the
-- tree may change the structure of the tree (especially when going from one to
-- zero or vice-versa), it is important to construct root hashes in a way which
-- does not assume in which order the prefix is constructed.
--
-- That is, for a given prefix `xyz` and a value `V`, the resulting hash is:
--
--     h (x | h (y | h (z | h(V))))
--
-- and not:
--
--     h (xyz | h(V))
--
-- Doing this makes it possible to add or remove neighbor elements from the
-- tree while still allowing to re-computing hashes as necessary. As an obvious
-- downside, inserting any element with references of size `n` requires exactly
-- `n` hashes __per element__
hashPrefix :: forall alg. HashAlgorithm alg => String -> Digest alg -> Digest alg
hashPrefix pre payload0 = go payload0 (reverse pre)
 where
  go payload = \case
    [] ->
      payload
    (h : q) ->
      go (hash (B8.singleton h <> convert payload)) q

-- See notes on 'hashPrefix'
hashLeaf :: forall alg a. (HashAlgorithm alg, Serialise a) => String -> a -> Digest alg
hashLeaf pre a =
  hashPrefix pre (hashlazy $ serialise a)

-- See notes on 'hashPrefix'
hashNode ::
  forall alg a.
  (HashAlgorithm alg) =>
  String ->
  [(Char, Digest a)] ->
  Digest alg
hashNode pre (sortOn fst -> children) =
  let hashes =
        BS.concat $
          fmap
            (\(c, child) -> convert $ hash @_ @alg $ B8.singleton c <> convert child)
            children
   in hashPrefix pre (hashlazy $ serialise hashes)

-- TODO: Ideally, the length of the short hash should be dependent on the size
-- of the tree, the more elements, the longer the hash.
shortHash :: Digest alg -> String
shortHash =
  ("#" <>)
    . take 5
    . B8.unpack
    . convertToBase Base16
    . convert @_ @ByteString

stripCommonPrefix :: [(String, a)] -> (String, [(String, a)])
stripCommonPrefix = \case
  [] ->
    ("", [])
  [(pre, a)] ->
    (pre, [("", a)])
  h : q ->
    let pre = foldr (\(e, _) -> intersectPrefix e) (fst h) q
     in (pre, first (dropPrefix pre) <$> (h : q))
 where
  intersectPrefix :: String -> String -> String
  intersectPrefix xs ys = case (xs, ys) of
    (x : qx, y : qy)
      | x == y ->
        x : intersectPrefix qx qy
    _ ->
      []

dropPrefix :: String -> String -> String
dropPrefix pre str =
  fromMaybe str (stripPrefix pre str)
