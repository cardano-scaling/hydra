{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

import Hydra.Prelude

import Control.Exception (ErrorCall)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust)
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import Plutus.MerkleTreeValidator (merkleTreeValidator, mtBuilderValidator)
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

    let (memberMem, memberCpu) = executionCostForMember utxo
        ExUnits (fromIntegral @_ @Double -> maxMem) (fromIntegral @_ @Double -> maxCpu) =
          defaultMaxExecutionUnits
        (builderMem, builderCpu) = executionCostForBuilder utxo

    putText $
      show numElems
        <> "\t"
        <> show (100 * fromIntegral (fromIntegral memberMem `div` numElems) / maxMem)
        <> "\t"
        <> show (100 * fromIntegral (fromIntegral memberCpu `div` numElems) / maxCpu)
    -- NOTE builder validator is likely to fail and thus raise an exception at low values
    -- of numElems, so we put 0 instead
    putTextLn
      ( "\t"
          <> show (100 * fromIntegral builderMem / maxMem)
          <> "\t"
          <> show (100 * fromIntegral builderCpu / maxCpu)
      )
      `catch` \(_ :: ErrorCall) -> putTextLn "\t0\t0"
 where
  -- NOTE: assume size of a UTXO is around  60
  genFakeUtxos numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)

executionCostForMember :: [Plutus.BuiltinByteString] -> (Natural, Natural)
executionCostForMember utxo =
  let tree = MT.fromList utxo
      accumulateCost e (curMem, curCpu) =
        let proof = fromJust $ MT.mkProof e tree
            ExUnits mem cpu = evaluateScriptExecutionUnits merkleTreeValidator (e, MT.rootHash tree, proof)
         in (mem + curMem, cpu + curCpu)
   in foldr accumulateCost (0, 0) utxo

executionCostForBuilder :: [Plutus.BuiltinByteString] -> (Natural, Natural)
executionCostForBuilder utxo =
  let tree = MT.fromList utxo
      root = rootHash tree
      ExUnits mem cpu = evaluateScriptExecutionUnits mtBuilderValidator (utxo, root)
   in (mem, cpu)
