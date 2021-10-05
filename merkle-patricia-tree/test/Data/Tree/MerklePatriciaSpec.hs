{-# LANGUAGE TypeApplications #-}

module Data.Tree.MerklePatriciaSpec (
  spec,
) where

import Prelude hiding (null)

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function (
  (&),
 )
import Data.List (
  stripPrefix,
  (\\),
 )
import Data.Maybe (isJust, isNothing)
import Data.Tree.MerklePatricia (
  Bit (..),
  Blake2b_160,
  Blake2b_224,
  HashAlgorithm,
  MerklePatriciaTree,
  Proof,
  Serialise,
  add,
  bitToChar,
  delete,
  depth,
  deserialise,
  dropPrefix,
  fromList,
  member,
  null,
  pretty,
  proofSize,
  root,
  serialise,
  size,
  toBits,
  toList,
  unsafeFromBits,
  unsafeMkProof,
 )
import Test.Hspec (
  Spec,
  context,
  parallel,
  shouldBe,
  specify,
 )
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  Testable,
  checkCoverage,
  choose,
  counterexample,
  cover,
  elements,
  forAll,
  forAllBlind,
  forAllShrink,
  forAllShrinkBlind,
  frequency,
  label,
  listOf,
  property,
  shrinkList,
  vectorOf,
  withMaxSuccess,
  (===),
  (==>),
 )

spec :: Spec
spec = parallel $ do
  context "MerklePatriciaTree" $ do
    context "Generator" $ do
      specify "Generate interesting trees, but not too big." $
        forAllMPT @Int $ \mpt ->
          (size mpt < 100)
            & cover 1 (size mpt == 0) "size == 0"
            & cover 3 (size mpt == 1) "size == 1"
            & cover 80 (size mpt > 1) "size >  1"

    context "fromList" $ do
      specify "roundtrip fromList/toList" $
        forAllMPT @Int $ \mpt ->
          let list = toList mpt
           in fromList list == mpt & counterexample (show list)

      specify "size (fromList xs) == length xs" $
        forAllMPT @Int $ \mpt ->
          size mpt === length (toList mpt)

    context "member" $ do
      specify "can construct and verify any proof for any element" $ do
        forAllNonEmptyMPT @Int $ \mpt (k, v) proof ->
          member (k, v) (root mpt) proof === True
            & counterexample ("proof:        " <> show proof)
            & counterexample ("(key, value): " <> show (k, v))

      specify "changing value invalidates proof" $ do
        forAllNonEmptyMPT @Int $ \mpt (k, v) proof ->
          let v' = v + 1
           in member (k, v') (root mpt) proof === False
                & counterexample ("proof:        " <> show proof)
                & counterexample ("value':       " <> show v')
                & counterexample ("(key, value): " <> show (k, v))

      specify "changing key invalidates proof" $ do
        forAllNonEmptyMPT @Int $ \mpt (k, v) proof ->
          forAllBlind (genReference $ BS.length k) $ \k' ->
            member (k', v) (root mpt) proof === (k == k')
              & counterexample ("proof:        " <> show proof)
              & counterexample ("key':         " <> show k')
              & counterexample ("(key, value): " <> show (k, v))

    context "add" $ do
      specify "adding element yields corresponding root" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          let mpt = fromList (toList mpt' \\ [(k, v)])
           in add (k, v) (root mpt) proof === Just (root mpt')
                & counterexample ("mpt: " <> pretty mpt)

      specify "adding altered element yields different root" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          let mpt = fromList (toList mpt' \\ [(k, v)])
              v' = v + 1
           in case add (k, v') (root mpt) proof of
                Nothing -> False
                Just someRoot -> someRoot /= root mpt'
                & counterexample ("value': " <> show v')
                & counterexample ("mpt:    " <> pretty mpt)

      specify "added element is member of new root" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          let mpt = fromList (toList mpt' \\ [(k, v)])
           in case add (k, v) (root mpt) proof of
                Nothing ->
                  property False
                    & counterexample ("mpt: " <> pretty mpt)
                    & counterexample "add returned Nothing!"
                Just newRoot ->
                  member (k, v) newRoot proof === True
                    & counterexample ("new root: " <> pretty mpt)
                    & counterexample ("mpt     : " <> pretty mpt)

      specify "adding unknown element gives Nothing" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          forAllBlind (genReference $ BS.length k) $ \k' ->
            (k /= k')
              ==> let mpt = fromList (toList mpt' \\ [(k, v)])
                   in add (k', v) (root mpt) proof === Nothing
                        & counterexample ("key':   " <> show k')
                        & counterexample ("mpt:    " <> pretty mpt)

    context "delete" $ do
      specify "removing an element yields corresponding root" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          let mpt = fromList (toList mpt' \\ [(k, v)])
           in delete (k, v) (root mpt') proof === Just (root mpt)
                & counterexample ("mpt:    " <> pretty mpt)

      specify "cannot delete an element that isn't there" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          forAllBlind (genReference $ BS.length k) $ \k' ->
            (k /= k')
              ==> delete (k', v) (root mpt') proof === Nothing
              & counterexample ("key': " <> show k')

      specify "cannot delete an altered element" $ do
        forAllNonEmptyMPT @Int $ \mpt' (k, v) proof ->
          let v' = v + 1
           in delete (k, v') (root mpt') proof === Nothing
                & counterexample ("value': " <> show v')

      specify "can re-add deleted element" $ do
        forAllNonEmptyMPT @Int $ \mpt (k, v) proof ->
          case delete (k, v) (root mpt) proof of
            Nothing ->
              property False
                & counterexample "delete returned Nothing!"
            Just newRoot ->
              add (k, v) newRoot proof === Just (root mpt)
                & counterexample ("newRoot: " <> show newRoot)

    context "Sizes" $ do
      specify "trees aren't too deep (<= 4 * log(U))" $ do
        withMaxSuccess 1000 $
          forAllBlind (genLargeMPT @() arbitrary) $ \mpt ->
            -- NOTE: We know the depth to be < log(|U|) for alphabet of size 16,
            -- so for alphabet of size 2, we have to multiple by 4 (2**4 = 16)
            -- to keep the same probabilities:
            --
            --     log (16^x) = log (2^4x) = 4*log(2^x)
            --
            let upperBound = 4 * ceiling @Double (log $ fromIntegral $ size mpt)
             in depth mpt <= upperBound
                  & counterexample ("MPT's depth: " <> show (depth mpt))
                  & counterexample ("MPT's size:  " <> show (size mpt))
                  & counterexample ("upperBound:  " <> show upperBound)

      specify "proofs aren't too large" $ do
        forAllBlind (genLargeMPT @() arbitrary) $ \mpt ->
          forAllBlind (elements $ toList mpt) $ \(k, _v) ->
            let proof = unsafeMkProof k mpt

                alphabetSize = 2
                s = fromIntegral (size mpt)
                d = fromIntegral (depth mpt)
                p = proofSize proof

                -- This calculation stems from:
                --
                -- - The probability of finding a collision between two digits (1/16)
                -- - The probability of finding multiple successive collisions
                -- - The fact that, at each level, a node may have up to 16
                --   children, but only 15 are necessary to construct a proof.
                --
                -- Note that, since this is probabilistic, it's hard to come up
                -- with a property which is always true since the equation:
                --
                --   p <= upperBound
                --
                -- may be sometimes false. We can however leverage QuickCheck
                -- coverage for that which will check that our label meets
                -- certain coverage requirements, according to some
                -- distribution which tolerates a few results off.
                upperBound =
                  ( ceiling @Double $
                      sum $
                        flip map [1 .. d] $ \i ->
                          (alphabetSize - 1) * min 1 (s / (alphabetSize ** i))
                  )
             in property (p < 2 * upperBound) -- No proof is larger than 2 * upperBound
                  & cover 1.0 (p <= upperBound) "Most proofs are smaller than upperBound"
                  & label (show p)
                  & counterexample ("proof size:     " <> show p)
                  & counterexample ("MPT's size:     " <> show s)
                  & counterexample ("MPT's depth:    " <> show d)
                  & counterexample ("upperBound:     " <> show upperBound)
                  & checkCoverage

  context "bits from strings" $ do
    let matrix =
          [ ("", [])
          , ("a", [Hi, Lo, Lo, Lo, Lo, Hi, Hi, Lo])
          , ("abc", [Hi, Lo, Lo, Lo, Lo, Hi, Hi, Lo, Lo, Hi, Lo, Lo, Lo, Hi, Hi, Lo, Hi, Hi, Lo, Lo, Lo, Hi, Hi, Lo])
          ]
    forM_ matrix $ \(input, output) ->
      specify (show input) $ toBits input `shouldBe` output

  context "toBits" $ do
    specify "should roundtrip with unsafeFromBits" $
      forAllShrink (genReference 8) shrinkReference $ \ref ->
        let bits = toBits ref
         in unsafeFromBits bits === ref
              & counterexample (fmap bitToChar bits)

  context "Prefixes" $ do
    specify "compare with oracle" $
      forAllShrink (genReference 8) shrinkReference $ \(toBits -> ref) ->
        forAll (genPrefix ref) $ \pre ->
          let result = dropPrefix pre ref
              oracle = stripPrefix pre ref
              prop = case oracle of
                Nothing ->
                  result == ref
                Just res ->
                  res == res
           in prop
                & cover 5 (isNothing oracle) "no common prefix"
                & cover 80 (isJust oracle) "has common prefix"
                & counterexample ("result: " <> fmap bitToChar result)
                & counterexample ("oracle: " <> maybe "ø" (fmap bitToChar) oracle)

  context "Proof" $ do
    specify "can serialise/deserialise" $
      forAllNonEmptyMPT @Int $ \_ _ proof ->
        let serialised = serialise proof
            deserialised = deserialise serialised
         in deserialised == proof
              & counterexample ("serialised: " <> show serialised)
              & counterexample ("deserialised: " <> show deserialised)
              & counterexample ("proof: " <> show proof)

--
-- Helpers
--

-- | Helper to write a property about an arbitrary Merkle-Patricia Tree.
forAllMPT ::
  forall a prop alg.
  (alg ~ Blake2b_160) =>
  (Arbitrary a, Serialise a, Show a, Testable prop) =>
  (MerklePatriciaTree alg a -> prop) ->
  Property
forAllMPT prop = forAllShrinkBlind (genMPT (arbitrary @a)) shrinkMPT $ \mpt ->
  counterexample (pretty mpt) (prop mpt)

-- | Like 'forAllMPT', but ensure the MPT has at least one element and, provides
-- one of those elements to the continuation as well as a proof for that
-- element.
forAllNonEmptyMPT ::
  forall a prop alg.
  (alg ~ Blake2b_160) =>
  (Arbitrary a, Serialise a, Show a, Testable prop) =>
  (MerklePatriciaTree alg a -> (ByteString, a) -> Proof alg -> prop) ->
  Property
forAllNonEmptyMPT prop =
  forAllMPT $ \mpt ->
    not (null mpt) ==> forAllBlind (elements $ toList mpt) $ \(k, v) ->
      prop mpt (k, v) (unsafeMkProof k mpt)

--
-- Generators
--

genReference :: Int -> Gen ByteString
genReference n =
  BS.pack <$> vectorOf n arbitrary

shrinkReference :: ByteString -> [ByteString]
shrinkReference =
  fmap BS.pack . shrinkList pure . BS.unpack

genPrefix :: [Bit] -> Gen [Bit]
genPrefix ref = do
  n <- choose (0, length ref)
  frequency
    [ (10, pure $ take n ref)
    , (1, take n . toBits <$> genReference (length ref))
    ]

genMPT :: Serialise a => Gen a -> Gen (MerklePatriciaTree Blake2b_160 a)
genMPT genA = do
  fromList <$> listOf ((,) <$> genReference 8 <*> genA)

genLargeMPT :: Serialise a => Gen a -> Gen (MerklePatriciaTree Blake2b_224 a)
genLargeMPT genA = do
  n <- choose (100, 1000)
  fromList <$> vectorOf n ((,) <$> genReference 32 <*> genA)

shrinkMPT ::
  (Serialise a, HashAlgorithm alg) =>
  MerklePatriciaTree alg a ->
  [MerklePatriciaTree alg a]
shrinkMPT = fmap fromList . shrinkList (pure []) . toList
