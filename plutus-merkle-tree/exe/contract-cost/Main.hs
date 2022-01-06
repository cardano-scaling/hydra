import Hydra.Prelude

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Plutus.MerkleTree as MT
import Plutus.MerkleTreeValidator (merkleTreeValidator)
import qualified PlutusTx.Builtins as Plutus
import Test.Plutus.Validator (ExUnits (ExUnits), evaluateScriptExecutionUnits)
import Test.QuickCheck (generate, vectorOf)

main :: IO ()
main =
  forM_ ([1 .. 10] <> [20, 30 .. 100] <> [120, 140 .. 500]) $ \numElems -> do
    utxo <- fmap Plutus.toBuiltin <$> genFakeUtxos numElems
    let tree = MT.fromList utxo
        accumulateCost e (curMem, curCpu) =
          let proof = fromJust $ MT.mkProof e tree
              ExUnits mem cpu = evaluateScriptExecutionUnits merkleTreeValidator (e, MT.rootHash tree, proof)
           in (mem + curMem, cpu + curCpu)

        (totalMem, totalCpu) = foldr accumulateCost (0, 0) utxo

    putTextLn $
      show numElems
        <> "\t"
        <> show (fromIntegral totalMem `div` numElems)
        <> "\t"
        <> show (fromIntegral totalCpu `div` numElems)
 where
  -- NOTE: assume size of a UTXO is around  60
  genFakeUtxos numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)
