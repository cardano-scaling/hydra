module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Plutus.MerkleTree (MerkleTree)
import qualified Plutus.MerkleTree as MT
import Test.Hydra.Prelude
import Test.QuickCheck (Property, (===))

spec :: Spec
spec =
  prop "fromList . toList roundtrips MT" $ prop_roundtripFromToList

prop_roundtripFromToList :: Property
prop_roundtripFromToList =
  forAllMerkleTree $ \tree ->
    MT.fromList (MT.toList tree) === tree

forAllMerkleTree :: (MerkleTree -> Property) -> Property
forAllMerkleTree = error "not implemented"
