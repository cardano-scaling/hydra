{-# LANGUAGE TypeApplications #-}

import Hydra.Prelude

import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import qualified Plutus.MerkleTree as MT
import Plutus.MerkleTreeValidator (merkleTreeValidator)
import qualified PlutusTx.Builtins as Plutus
import Test.Plutus.Validator (
  ExUnits (ExUnits),
  defaultMaxExecutionUnits,
  evaluateScriptExecutionUnits,
 )
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

    let ExUnits (fromIntegral @_ @Double -> maxMem) (fromIntegral @_ @Double -> maxCpu) =
          defaultMaxExecutionUnits

    putTextLn $
      show numElems
        <> "\t"
        <> show (100 * fromIntegral (fromIntegral totalMem `div` numElems) / maxMem)
        <> "\t"
        <> show (100 * fromIntegral (fromIntegral totalCpu `div` numElems) / maxCpu)
 where
  -- NOTE: assume size of a UTXO is around  60
  genFakeUtxos numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)
