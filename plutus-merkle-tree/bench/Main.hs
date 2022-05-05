{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Hydra.Prelude hiding (catch)

import qualified Cardano.Ledger.Alonzo.Scripts as Ledger
import Data.ByteString (hPut)
import qualified Data.ByteString as BS
import Data.Fixed (E2, Fixed)
import Data.Maybe (fromJust)
import Plutus.MerkleTree (rootHash)
import qualified Plutus.MerkleTree as MT
import qualified Plutus.V1.Ledger.Api as Plutus
import Test.Plutus.Validator (ExUnits (ExUnits), evaluateScriptExecutionUnits)
import Test.QuickCheck (generate, vectorOf)
import Validators (merkleTreeBuilderValidator, merkleTreeMemberValidator)

-- import Plutus.Orphans ()

newtype MemUnit = MemUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype CpuUnit = CpuUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

main :: IO ()
main = do
  mt <- costOfMerkleTree
  hPut stdout (encodeUtf8 mt)

costOfMerkleTree :: IO Text
costOfMerkleTree = markdownMerkleTreeCost <$> computeMerkleTreeCost
 where
  markdownMerkleTreeCost stats =
    unlines $
      [ "## Cost of on-chain Merkle-Tree"
      , ""
      , "| Size | % member max mem | % member max cpu | % builder max mem | % builder max cpu |"
      , "| :--- | ---------------: | ---------------: | ----------------: | ----------------: |"
      ]
        <> fmap
          ( \(numElems, memberMem, memberCpu, builderMem, builderCpu) ->
              "| "
                <> show numElems
                <> " | "
                <> show (100 * fromIntegral (fromIntegral memberMem `div` numElems) / maxMem)
                <> " | "
                <> show (100 * fromIntegral (fromIntegral memberCpu `div` numElems) / maxCpu)
                <> " | "
                <> show (100 * fromIntegral builderMem / maxMem)
                <> " | "
                <> show (100 * fromIntegral builderCpu / maxCpu)
                <> " |"
          )
          stats

  -- TODO: Avoid re-hard-coding parameters here.
  maxMem, maxCpu :: Fixed E2
  Ledger.ExUnits
    (fromIntegral @_ @(Fixed E2) -> maxMem)
    (fromIntegral @_ @(Fixed E2) -> maxCpu) = Ledger.ExUnits 14_000_000 10_000_000_000

computeMerkleTreeCost :: IO [(Int, MemUnit, CpuUnit, MemUnit, CpuUnit)]
computeMerkleTreeCost =
  mapM compute [1, 2, 5, 10, 20, 50, 100, 500]
 where
  compute numElems = do
    utxo <- fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

    let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
        (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
    pure (numElems, MemUnit memberMem, CpuUnit memberCpu, MemUnit builderMem, CpuUnit builderCpu)

  -- NOTE: assume size of a UTXO is around  60 bytes
  genFakeUTxOs numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)

executionCostForMember :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForMember utxo =
  let tree = MT.fromList utxo
      accumulateCost e acc =
        acc >>= \(curMem, curCpu) ->
          let proof = fromJust $ MT.mkProof e tree
           in case evaluateScriptExecutionUnits merkleTreeMemberValidator (e, MT.rootHash tree, proof) of
                Right (ExUnits mem cpu) ->
                  Right (mem + curMem, cpu + curCpu)
                Left err -> Left err
   in foldr accumulateCost (Right (0, 0)) utxo

executionCostForBuilder :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForBuilder utxo =
  let tree = MT.fromList utxo
      root = rootHash tree
   in evaluateScriptExecutionUnits merkleTreeBuilderValidator (utxo, root) <&> \case
        ExUnits mem cpu -> (mem, cpu)
