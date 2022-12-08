{-# LANGUAGE TypeApplications #-}

module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT
import qualified PlutusTx.Builtins as Plutus
import Test.QuickCheck (
  Property,
  Testable,
  checkCoverage,
  counterexample,
  cover,
  elements,
  forAll,
  forAllShrink,
  listOf1,
  (===),
 )

spec :: Spec
spec = parallel $ do
  prop "fromList . toList roundtrips MT" prop_roundtripFromToList
  prop "can check membership of an element" prop_member
  prop "tree is balanced" prop_treeIsBalanced

prop_roundtripFromToList :: Property
prop_roundtripFromToList =
  forAllMerkleTree $ \tree ->
    let l = MT.toList tree
     in MT.fromList l === tree
          & counterexample ("List: " <> show l)

prop_member :: Property
prop_member =
  forAllNonEmptyMerkleTree $ \(tree, e, proof) ->
    MT.member e (MT.rootHash tree) proof
      & counterexample ("Proof: " <> show proof)

prop_treeIsBalanced :: Property
prop_treeIsBalanced =
  forAllNonEmptyMerkleTree $ \(tree, _, proof) ->
    let treeSize = MT.size tree
        treeDepthUpperBound = floor (logBase @Double 2 (fromIntegral treeSize)) + 1
     in length proof <= fromIntegral treeSize
          & cover 100 (length proof <= treeDepthUpperBound) "proofs are in log(n) size of tree"
          & counterexample ("proof: " <> show proof)
          & counterexample ("tree size: " <> show treeSize)
          & counterexample ("max tree depth: " <> show treeDepthUpperBound)
          & checkCoverage

forAllMerkleTree :: Testable prop => (MerkleTree -> prop) -> Property
forAllMerkleTree =
  forAllShrink genMerkleTree shrinkMerkleTree

forAllNonEmptyMerkleTree ::
  Testable prop =>
  ((MerkleTree, Plutus.BuiltinByteString, MT.Proof) -> prop) ->
  Property
forAllNonEmptyMerkleTree action =
  forAll genNonEmptyMerkleTree $ \tree ->
    forAll (elements $ MT.toList tree) $ \e ->
      action (tree, e, fromJust $ MT.mkProof e tree)

genMerkleTree :: Gen MerkleTree
genMerkleTree =
  MT.fromList . fmap (Plutus.toBuiltin . BS.pack) <$> arbitrary

genNonEmptyMerkleTree :: Gen MerkleTree
genNonEmptyMerkleTree = do
  nonEmptyBSList <- listOf1 $ fmap (Plutus.toBuiltin . BS.pack) arbitrary
  pure $ MT.fromList nonEmptyBSList

shrinkMerkleTree :: MerkleTree -> [MerkleTree]
shrinkMerkleTree _ = []
