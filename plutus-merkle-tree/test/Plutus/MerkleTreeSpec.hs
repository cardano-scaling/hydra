module Plutus.MerkleTreeSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Plutus.MerkleTree (MerkleTree, rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.MerkleTreeValidator (emptyValidator, merkleTreeValidator)
import qualified PlutusTx.Builtins as Plutus
import Test.Plutus.Validator (ExUnits (ExUnits), defaultMaxExecutionUnits, distanceExUnits, evaluateScriptExecutionUnits)
import Test.QuickCheck (
  Property,
  Testable,
  counterexample,
  elements,
  forAll,
  forAllShrink,
  (===),
  (==>),
 )

spec :: Spec
spec = do
  prop "fromList . toList roundtrips MT" prop_roundtripFromToList
  prop "can check membership of an element" prop_member
  prop "execution units" prop_executionUnitsComparison

prop_roundtripFromToList :: Property
prop_roundtripFromToList =
  forAllMerkleTree $ \tree ->
    MT.fromList (MT.toList tree) === tree

prop_member :: Property
prop_member =
  forAllNonEmptyMerkleTree $ \(tree, e, proof) ->
    MT.member e (MT.rootHash tree) proof
      & counterexample ("Proof: " <> show proof)

prop_executionUnitsComparison :: Property
prop_executionUnitsComparison =
  forAllNonEmptyMerkleTree $ \(tree, e, proof) ->
    let mtExUnits = evaluateScriptExecutionUnits merkleTreeValidator (e, rootHash tree, proof)
        emptyExUnits = evaluateScriptExecutionUnits emptyValidator ()
        ExUnits mem cpu = distanceExUnits mtExUnits emptyExUnits
        ExUnits maxMem maxCpu = defaultMaxExecutionUnits
        ratioMem :: Double = fromRational $ fromIntegral mem * 100 % fromIntegral maxMem
        ratioCpu :: Double = fromRational $ fromIntegral cpu * 100 % fromIntegral maxCpu
     in ratioMem < 1 && ratioCpu < 1
          & counterexample ("mem units: " <> show mem <> " (" <> show ratioMem <> ")")
          & counterexample ("cpu units: " <> show cpu <> " (" <> show ratioCpu <> ")")
          & counterexample ("proof size: " <> show (length proof))

forAllMerkleTree :: Testable prop => (MerkleTree -> prop) -> Property
forAllMerkleTree =
  forAllShrink genMerkleTree shrinkMerkleTree

forAllNonEmptyMerkleTree ::
  Testable prop =>
  ((MerkleTree, Plutus.BuiltinByteString, MT.Proof) -> prop) ->
  Property
forAllNonEmptyMerkleTree action =
  forAllMerkleTree $ \tree ->
    not (MT.null tree) ==> forAll (elements $ MT.toList tree) $ \e ->
      action (tree, e, fromJust $ MT.mkProof e tree)

genMerkleTree :: Gen MerkleTree
genMerkleTree =
  MT.fromList . fmap (Plutus.toBuiltin . BS.pack) <$> arbitrary

shrinkMerkleTree :: MerkleTree -> [MerkleTree]
shrinkMerkleTree _ = []
