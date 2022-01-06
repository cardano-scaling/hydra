module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.ByteString as BS
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT
import qualified PlutusTx.Builtins as Plutus
import Test.QuickCheck (Property, Testable, elements, forAll, forAllShrink, (===), (==>))

spec :: Spec
spec = do
  prop "fromList . toList roundtrips MT" prop_roundtripFromToList
  prop "can check membership of an element" prop_member

prop_roundtripFromToList :: Property
prop_roundtripFromToList =
  forAllMerkleTree $ \tree ->
    MT.fromList (MT.toList tree) === tree

prop_member :: Property
prop_member =
  forAllNonEmptyMerkleTree $ \(tree, e) ->
    MT.member e (MT.rootHash tree) (MT.mkProof e tree)

forAllMerkleTree :: Testable prop => (MerkleTree -> prop) -> Property
forAllMerkleTree =
  forAllShrink genMerkleTree shrinkMerkleTree

forAllNonEmptyMerkleTree ::
  Testable prop =>
  ((MerkleTree, Plutus.BuiltinByteString) -> prop) ->
  Property
forAllNonEmptyMerkleTree action =
  forAllMerkleTree $ \tree ->
    not (MT.null tree) ==> forAll (elements $ MT.toList tree) $ \e ->
      action (tree, e)

genMerkleTree :: Gen MerkleTree
genMerkleTree =
  MT.fromList . fmap (Plutus.toBuiltin . BS.pack) <$> arbitrary

shrinkMerkleTree :: MerkleTree -> [MerkleTree]
shrinkMerkleTree _ = []
