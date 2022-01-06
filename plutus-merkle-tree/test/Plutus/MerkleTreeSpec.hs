module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.ByteString as BS
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT
import qualified PlutusTx.Builtins as Plutus
import Test.QuickCheck (Property, Testable, forAllShrink, (===), (==>))

spec :: Spec
spec =
  prop "fromList . toList roundtrips MT" prop_roundtripFromToList

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
    not (null tree) ==> undefined

genMerkleTree :: Gen MerkleTree
genMerkleTree =
  MT.fromList . fmap (Plutus.toBuiltin . BS.pack) <$> arbitrary

shrinkMerkleTree :: MerkleTree -> [MerkleTree]
shrinkMerkleTree _ = []
