module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.ByteString as BS
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT
import qualified PlutusTx.Builtins as Plutus
import Test.QuickCheck (Property, forAllShrink, (===))

spec :: Spec
spec =
  prop "fromList . toList roundtrips MT" prop_roundtripFromToList

prop_roundtripFromToList :: Property
prop_roundtripFromToList =
  forAllMerkleTree $ \tree ->
    MT.fromList (MT.toList tree) === tree

forAllMerkleTree :: (MerkleTree -> Property) -> Property
forAllMerkleTree = forAllShrink genMerkleTree shrinkMerkleTree

genMerkleTree :: Gen MerkleTree
genMerkleTree =
  MT.fromList . fmap (Plutus.toBuiltin . BS.pack) <$> arbitrary

shrinkMerkleTree :: MerkleTree -> [MerkleTree]
shrinkMerkleTree _ = []
