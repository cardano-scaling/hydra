{-# LANGUAGE TypeApplications #-}

module Data.Tree.MerklePatriciaSpec (
  spec,
) where

import Prelude hiding (null)

import Data.Function (
  (&),
 )
import Data.List (
  stripPrefix,
  (\\),
 )
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Tree.MerklePatricia (
  Blake2b_160,
  Blake2b_224,
  HashAlgorithm,
  MerklePatriciaTree,
  Proof,
  Serialise,
  add,
  delete,
  depth,
  dropPrefix,
  fromList,
  member,
  null,
  pretty,
  proofSize,
  root,
  size,
  toList,
  unsafeMkProof,
 )
import Test.Hspec (
  Spec,
  context,
  parallel,
  specify,
 )
import Test.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  Testable,
  choose,
  counterexample,
  cover,
  elements,
  forAll,
  forAllBlind,
  forAllShrink,
  forAllShrinkBlind,
  frequency,
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
          forAllBlind (genReference $ length k) $ \k' ->
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
          forAllBlind (genReference $ length k) $ \k' ->
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
          forAllBlind (genReference $ length k) $ \k' ->
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

      specify "trees aren't too deep (<= 1 + log(D))" $ do
        withMaxSuccess 1000 $
          forAllBlind (genLargeMPT @() arbitrary) $ \mpt ->
            let sup = ceiling @Double $ log $ fromIntegral $ size mpt
             in depth mpt <= sup
                  & counterexample ("MPT's depth:      " <> show (depth mpt))
                  & counterexample ("MPT's size:       " <> show (size mpt))
                  & counterexample ("sup:              " <> show sup)

      specify "proofs aren't too large (<= 15 * (depth - 1))" $ do
        withMaxSuccess 1000 $
          forAllBlind (genLargeMPT @() arbitrary) $ \mpt ->
            forAllBlind (elements $ toList mpt) $ \(k, _v) ->
              let proof = unsafeMkProof k mpt
                  alphabetSize = 16
                  -- This is a pessimistic upper bound, in practice, it turns out
                  -- to be less.
                  sup = (alphabetSize - 1) * (depth mpt - 1)
               in proofSize proof <= sup
                    & counterexample ("proof size:     " <> show (proofSize proof))
                    & counterexample ("MPT's size:     " <> show (size mpt))
                    & counterexample ("MPT's depth:    " <> show (depth mpt))
                    & counterexample ("sup:            " <> show sup)

  context "Prefixes" $ do
    specify "compare with oracle" $
      forAllShrink (genReference 8) shrinkReference $ \ref ->
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
                & counterexample ("result: " <> result)
                & counterexample ("oracle: " <> fromMaybe "ø" oracle)

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
  (MerklePatriciaTree alg a -> (String, a) -> Proof alg -> prop) ->
  Property
forAllNonEmptyMPT prop =
  forAllMPT $ \mpt ->
    not (null mpt) ==> forAllBlind (elements $ toList mpt) $ \(k, v) ->
      prop mpt (k, v) (unsafeMkProof k mpt)

--
-- Generators
--

genReference :: Int -> Gen String
genReference n =
  vectorOf n (elements $ ['0' .. '9'] ++ ['a' .. 'f'])

shrinkReference :: String -> [String]
shrinkReference = shrink

genPrefix :: String -> Gen String
genPrefix ref = do
  n <- choose (0, length ref)
  frequency
    [ (10, pure (take n ref))
    , (1, take n <$> genReference (length ref))
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
