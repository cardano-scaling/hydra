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
  depth,

  -- * Proof

  -- ** Constructing
  mkProof,
  unsafeMkProof,

  -- ** Using
  member,
  add,
  delete,

  -- * Debugging
  proofSize,
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
  Bit (..),
  bitToChar,
  bitToByteString,
  toBits,
  unsafeFromBits,
  stripCommonPrefix,
  dropPrefix,
) where

import Prelude hiding (null)

import Codec.Serialise (Serialise, serialise)
import Control.Arrow (first, second)
import Control.Monad (guard, join)
import Crypto.Hash (Digest, hash, hashlazy)
import Crypto.Hash.Algorithms (Blake2b_160, Blake2b_224, Blake2b_256)
import Crypto.Hash.IO (HashAlgorithm)
import Data.Bits (shiftL, testBit, (.|.))
import Data.ByteArray (convert)
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.List (foldl', intercalate, sortOn, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word8)
import GHC.Stack (HasCallStack)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- * Type

-- | An opaque 'MerklePatriciaTree', parameterized by a hash algorithm and a
-- type for its values.
data MerklePatriciaTree alg a
  = Leaf [Bit] (Digest alg) a
  | Node [Bit] (Digest alg) [(Bit, MerklePatriciaTree alg a)]
  deriving (Show)

instance HashAlgorithm alg => Eq (MerklePatriciaTree alg a) where
  (root -> a) == (root -> b) = a == b

-- | An opaque 'Proof' which can be used to prove membership or atomic
-- operations such as 'add' or 'delete'
newtype Proof alg = Proof {unProof :: [([Bit], [(Bit, Digest alg)])]}
  deriving stock (Show)

data Bit = Hi | Lo deriving (Show, Eq, Ord)

bitToChar :: Bit -> Char
bitToChar = \case
  Hi -> '1'
  Lo -> '0'

bitToByteString :: Bit -> ByteString
bitToByteString =
  B8.singleton . bitToChar

toBits :: ByteString -> [Bit]
toBits = foldMap bits . BS.unpack
 where
  bits :: Word8 -> [Bit]
  bits b = [if testBit b i then Hi else Lo | i <- [0 .. 7]]

unsafeFromBits :: HasCallStack => [Bit] -> ByteString
unsafeFromBits = BS.pack . go
 where
  go = \case
    b0 : b1 : b2 : b3 : b4 : b5 : b6 : b7 : q ->
      let shiftL' Hi = shiftL 1
          shiftL' _ = const 0
          bit =
            shiftL' b0 0
              .|. shiftL' b1 1
              .|. shiftL' b2 2
              .|. shiftL' b3 3
              .|. shiftL' b4 4
              .|. shiftL' b5 5
              .|. shiftL' b6 6
              .|. shiftL' b7 7

          str = go q
       in bit : str
    [] ->
      []
    _ ->
      error "fromBits: wrong number of bits."

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
  [(ByteString, a)] ->
  MerklePatriciaTree alg a
fromList = go [] . fmap (first toBits)
 where
  alphabet = [Lo, Hi]
  go c = \case
    [] ->
      Node [] (hashNode c []) []
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
      mpt | null mpt -> Nothing
      mpt -> Just (i, mpt)

  project :: Bit -> ([Bit], a) -> Maybe ([Bit], a)
  project i = \case
    (h : q, a) | h == i -> Just (q, a)
    _ -> Nothing

-- | Convert a Merkle-Patricia Tree into a list of (key, value) pairs.
--
-- __properties:__
--
--     >>> ∀xs. toList (fromList xs) == xs
toList :: forall alg a. MerklePatriciaTree alg a -> [(ByteString, a)]
toList = fmap (first unsafeFromBits) . go
 where
  go = \case
    Leaf pre _ a ->
      [(pre, a)]
    Node pre _ children ->
      mconcat [first ((pre <> [c]) <>) <$> go mpt | (c, mpt) <- children]

--
-- Analyzing
--

-- | Access the root hash of the 'MerklePatriciaTree'. Two trees with the same
-- root hash are equal.
root :: forall alg a. MerklePatriciaTree alg a -> Digest alg
root = \case
  Leaf _ h _ -> h
  Node _ h _ -> h

-- | True if the given 'MerklePatriciaTree' is empty.
--
-- __properties:__
--
--     >>> ∀mpt. (size mpt == 0) == (null mpt)
--     True
null :: MerklePatriciaTree alg a -> Bool
null = \case
  Node _ _ [] -> True
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
  Leaf{} ->
    1
  Node _ _ children ->
    foldl' (\sz (_, mpt) -> sz + size mpt) 0 children

-- | Count the maximum number of levels in the tree.
depth :: MerklePatriciaTree alg a -> Int
depth = \case
  Leaf{} ->
    0
  Node _ _ children ->
    1 + foldl' (\sup (_, mpt) -> max sup (depth mpt)) 0 children

--
-- Proof
--

-- | Construct a proof for a given `key` of the 'MerklePatriciaTree'. Returns
-- 'Nothing' if the key does not exists. The proof is compact and does not
-- information embedded in the element itself. This means that, the
-- corresponding (key, value) is necessary for any use of the proof.
mkProof ::
  ByteString ->
  -- | A key pointing to one element in the tree
  MerklePatriciaTree alg a ->
  -- | The target tree
  Maybe (Proof alg)
mkProof (toBits -> ref0) =
  go ref0
 where
  go ref = \case
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
          Proof proof <- go ref' child
          pure $ Proof $ (pre, others) : proof
        _ ->
          Nothing

-- An unsafe version of 'mkProof', for testing only.
unsafeMkProof ::
  HasCallStack =>
  ByteString ->
  MerklePatriciaTree alg a ->
  Proof alg
unsafeMkProof ref =
  fromMaybe (error "unsafeMkProof: Nothing") . mkProof ref

-- | Size of a 'Proof'. Returns the number of hashes needed for the proof.
proofSize ::
  forall alg.
  Proof alg ->
  Int
proofSize (Proof proof) = case proof of
  [] ->
    0
  ((_pre, children) : rest) ->
    length children + proofSize (Proof rest)

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
  -- | The (key, value) to check
  (ByteString, a) ->
  -- | The root of the 'MerklePatriciaTree' with (supposedly) the (key, value)
  Digest alg ->
  -- | A proof for the (key, value) in the corresponding 'MerklePatriciaTree'
  Proof alg ->
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
  -- | The (key, value) to add.
  (ByteString, a) ->
  -- | The root of the 'MerklePatriciaTree' __without__ the (key, value)
  Digest alg ->
  -- | A proof for the (key, value) in the resulting 'MerklePatriciaTree'
  Proof alg ->
  -- | Returns, the root of the 'MerklePatriciaTree' __with__ the (key, value)
  -- added, or 'Nothing' if the proof is invalid for the given root.
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
  -- | The (key, value) to delete.
  (ByteString, a) ->
  -- | The root of the 'MerklePatriciaTree' __with__ the (key, value)
  Digest alg ->
  -- | A proof for the (key, value) in the original 'MerklePatriciaTree'
  Proof alg ->
  -- | Returns, the root of the 'MerklePatriciaTree' __without__ the (key, value),
  -- or 'Nothing' if the (key, value) is not present in the original tree.
  Maybe (Digest alg)
delete el newRoot proofs = do
  (newRoot', oldRoot) <- walkBackwards el proofs
  oldRoot <$ guard (newRoot' == newRoot)

--
-- Debugging
--

-- | Pretty-print a 'MerklePatriciaTree' in ascii, such that levels are aligned
-- horizontally (one identation-level per level in the tree), and prefixes are
-- positioned such that they are visually easy to follow. The outputs look like
-- this:
--
-- @
-- *--- Root hash
-- |                      *---- Values at the leaves
-- |                      |
-- *-> #3801f:            |
--     #ee41c: \\0         |
--     #16944:  \\0      vvvvv
--     #9d288:   \\001100 ↦ ()
--     #d0bd1:   \\100110 ↦ ()
--     #38e90:  \\1000110 ↦ ()
--     #978d8: \\1
--     #5ac65:  \\0
--     #6f2de:   \\00
--     #4fe20:     \\0110 ↦ ()
--     #d1489:     \\1100 ↦ ()
--     #d0bd1:   \\100110 ↦ ()
--     #22882:  \\100
--     #4fe20:   ^ \\0110 ↦ ()
--     #d1489:   | \\1100 ↦ ()
--      ^^^^^    |   ^^^
--        |      |    |
--        |      |    *--- Leaf or node's prefix
--        |      |
--        |      *---- Branch bit
--        |
--        |
--        |
--        *----- Nodes / Leaves' hashes
-- @
--
-- Useful for debugging / reasonning.
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
    Leaf _ h _ -> [h]
    Node _ h children -> let hs = concatMap (hashes . snd) children in h : hs

  body n = \case
    Leaf pre _ a ->
      fmap bitToChar pre <> " ↦ " <> take 5 (show a)
    Node pre _ children ->
      let t = n + length pre - 1
       in fmap bitToChar pre
            <> join ["\n" <> indent t (prettyElement 2 el) | el <- children]

  prettyElement :: Int -> (Bit, MerklePatriciaTree alg a) -> String
  prettyElement n (i, sub) =
    ['\\', bitToChar i] <> body n sub

--
-- Helper / Internal
--

-- The root hash of every empty tree.
emptyRoot :: forall alg. HashAlgorithm alg => Digest alg
emptyRoot = hashNode [] []

-- Walks a proof backwards and returns a root with element and root without element.
--
-- Let's take an example, with the following MPT of size 5 and all values are
-- simply units (see also 'pretty' for explanation about how to read the
-- pretty-printed MPTs). We'll walk the proof for '00100110', so let's also put
-- side-by-side the same MPT with that element removed as it helps reasonning.
--
--     #dddda:
--     #a7b9e: \0
--     #93119:  \0100110 ↦ ()              #6d40d:
--     #38e90:  \1000110 ↦ ()              #ceb6f: \01000110 ↦ ()
--     #fa61b: \1                          #fa61b: \1
--     #3e241:  \0                         #3e241:  \0
--     #25f0c:   \000110 ↦ ()              #25f0c:   \000110 ↦ ()
--     #d0bd1:   \100110 ↦ ()              #d0bd1:   \100110 ↦ ()
--     #38e90:  \1000110 ↦ ()              #38e90:  \1000110 ↦ ()
--
-- Let's now consider the proof for the key '00100110':
--
-- - (ø, [(1,fa61b)])
-- - (ø, [(1,38e90)])
-- - (100110, [])
--
-- At each level (visible through indentation), the proof contains the common
-- prefix for that level, as well as the _other_ children needed to reconstruct
-- the hash. We need therefore to reconstruct the proof from the _end_, and to
-- go up one level at the time until we reach the root. Note however that we
-- calculate __two hashes__ at the same time:
--
-- - The resulting root hash reconstructed from the proof, with the element.
-- - The resulting root hash reconstructed from the proof, _without the element_.
--
-- Both results are useful to determine whether a modification (addition /
-- deletion) in the tree is legal.
--
-- The first part of the proof being considered is (100110, []) which
-- represents the leaf and the suffix of the reference (or the leaf's prefix).
-- There's no children and no value, because the value is provided by the caller
-- for verification. From there, we can easily calculate the resulting leaf hash
-- by re-hashing the value and the corresponding part of the suffix and then go
-- recursively through each node levels above, in reverse order.
--
-- The next one is (ø, [(1,38e90)]) which indeed correspond to the _other_branch
-- once we have consumed '100110' from the end of the reference. The path to the
-- element is on the branch `0`, thus we need the branch `1` to continue the
-- proof.
--
-- This is also where it gets interesting. Calculating the root hash for
-- the sub-MPT _with_ the element is trivial. We can re-construct the hash for the
-- branch on our path, and the proof contains the hash of the other branch.
-- Calculating the hash of the sub-MPT _without_ the element is more complex
-- because, the structure of the tree changes. When the element is present, we
-- have a Node with no prefix, and two branches on '0' and '1'. When the element
-- is not present, we have a leaf, with '1000110' as prefix.
--
-- What is perhaps taken for granted at the moment (and quite convenient you'll
-- admit!) is that, the hash of the Leaf with prefix 1000110 and the hash of the
-- node from the branch '1' with a single leaf with prefix 000110 are actually
-- the same!
-- This is done _by construction_ to make it possible to reconstruct hashes even
-- if the structure of the tree changes! For more details, see the doc-string on
-- the 'hashPrefix' function, but in short: a prefix is hashed in multiple rounds,
-- one hash per bit, all nested. Such that it's easy to add or peel a layer
-- from the oignon-hash. To get a sense of this, imagine for a moment that this
-- wasn't the case, and that the hash of the leaf would be different than the
-- hash of a node with leaf... how would you reconstruct the hash of the MPT
-- with the target element removed? When constructed "naively", we would typically
-- construct the node hash as:
--
--        *--- Node's prefix
--        v
--   hash(ø|hash(000110|hash(value)))
--                 ^
--                 *--- Leaf's prefix
--
-- But now, to reconstruct the tree _without_ the element, we would need to
-- construct (still naively) the hash as:
--
--           *--- Leaf's prefix
--           v
--   hash(1000110|hash(value))
--
-- We don't have hash(value) in the proof, and we don't really have a way to get
-- from the former to the latter. So, the trick in the construction of the tree
-- and all the hashes is to (a) include the branch bit in the hash and (b) hash
-- every bit separately, in an oignon pattern such that, the proof has
-- actually the following shape:
--
--   hash(1|hash(0|hash(0|hash(0|hash(1|hash(1|hash(0|hash(value))))))))
--
-- And now, it becomes trivial to move forward since, with both representation,
-- hashes become equal and we can easily add new layers on top if needed. As
-- this is the case for the _next_ node. The first node is easy to re-calculate
-- because, it is transformed into a leaf in the absence of the proved element.
--
-- Then, the next (and last) part of the proof is indeed (ø, [(1,38e90)]). The
-- case where the element is present is trivial. When it's absent however, we
-- need to calculate the hash of the '0' branch, which is straightforward with
-- the way we construct hashes:
--
--    hash(0|prev_hash)  *where prev_hash is the hash we calculated just above.
--
-- This wouldn't be possible with a naive hash construction, but is rendered
-- easy by hashing each bit separately. Doing this recursively up to the top
-- yields two root hashes which can then be used for membership verification,
-- addition or deletion.
walkBackwards ::
  forall alg a.
  (Serialise a, HashAlgorithm alg) =>
  -- | leaf (key, value) to lookup
  (ByteString, a) ->
  -- | Proof of existence of the (key, value) into the MPT
  Proof alg ->
  Maybe (Digest alg, Digest alg)
walkBackwards (reverse . toBits -> ref0, a) (reverse . unProof -> proofs0) =
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
  -- of the tree. In this branch, the ref is necessarily at least one bit
  -- (which corresponds to the branch choice of the MPT). The current stage may
  -- also have a prefix. Each call to this function consumes both the bit and
  -- the prefix from the reference. The proof contains all _other_ hashes and
  -- bits for this level, the missing one being re-computed from the provided
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
hashPrefix :: forall alg. HashAlgorithm alg => [Bit] -> Digest alg -> Digest alg
hashPrefix pre payload0 = go payload0 (reverse pre)
 where
  go payload = \case
    [] ->
      payload
    (h : q) ->
      go (hash (bitToByteString h <> convert payload)) q

-- See notes on 'hashPrefix'
hashLeaf :: forall alg a. (HashAlgorithm alg, Serialise a) => [Bit] -> a -> Digest alg
hashLeaf pre a =
  hashPrefix pre (hashlazy $ serialise a)

-- See notes on 'hashPrefix'
hashNode ::
  forall alg a.
  (HashAlgorithm alg) =>
  [Bit] ->
  [(Bit, Digest a)] ->
  Digest alg
hashNode pre (sortOn fst -> children) =
  let hashes =
        BS.concat $
          fmap
            (\(c, child) -> convert $ hash @_ @alg $ bitToByteString c <> convert child)
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

stripCommonPrefix :: forall a v. Eq a => [([a], v)] -> ([a], [([a], v)])
stripCommonPrefix = \case
  [] ->
    ([], [])
  [(pre, a)] ->
    (pre, [([], a)])
  h : q ->
    let pre = foldr (\(e, _) -> intersectPrefix e) (fst h) q
     in (pre, first (dropPrefix pre) <$> (h : q))
 where
  intersectPrefix :: [a] -> [a] -> [a]
  intersectPrefix xs ys = case (xs, ys) of
    (x : qx, y : qy)
      | x == y ->
        x : intersectPrefix qx qy
    _ ->
      []

dropPrefix :: Eq a => [a] -> [a] -> [a]
dropPrefix pre str =
  fromMaybe str (stripPrefix pre str)
