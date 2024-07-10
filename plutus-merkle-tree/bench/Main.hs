module Main where

import Hydra.Prelude hiding (catch)

import Data.ByteString (hPut)
import Data.ByteString qualified as BS
import Data.Fixed (E2, Fixed)
import Data.Maybe (fromJust)
import Plutus.MerkleTree (rootHash)
import Plutus.MerkleTree qualified as MT
import PlutusTx.Prelude qualified as Plutus
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Plutus.Validator (ExecutionUnits (..), defaultMaxExecutionUnits, evaluateScriptExecutionUnits)
import Test.QuickCheck (generate, vectorOf)
import Validators (merkleTreeBuilderValidator, merkleTreeMemberValidator)

newtype MemUnit = MemUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

newtype CpuUnit = CpuUnit Natural
  deriving newtype (Eq, Show, Ord, Num, Real, Enum, Integral)

main :: IO ()
main = do
  outputDirectory <-
    getArgs <&> getOutputDirectory
  mt <- encodeUtf8 . (unlines pageHeader <>) <$> costOfMerkleTree
  case outputDirectory of
    Nothing ->
      hPut stdout mt
    Just out -> do
      createDirectoryIfMissing True out
      writeFileBS (out </> "mt-benchmarks.md") mt

getOutputDirectory :: [String] -> Maybe String
getOutputDirectory = listToMaybe

pageHeader :: [Text]
pageHeader =
  [ "--- "
  , "sidebar_label: 'Plutus Merkle-Tree Benchmarks' "
  , "sidebar_position: 3 "
  , "--- "
  , ""
  , "# Plutus Merkle tree contract"
  , ""
  ]

costOfMerkleTree :: IO Text
costOfMerkleTree = markdownMerkleTreeCost <$> computeMerkleTreeCost
 where
  markdownMerkleTreeCost stats =
    unlines $
      [ "## On-chain Merkle tree costs"
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

  maxMem, maxCpu :: Fixed E2
  maxMem = fromIntegral $ executionMemory defaultMaxExecutionUnits
  maxCpu = fromIntegral $ executionSteps defaultMaxExecutionUnits

computeMerkleTreeCost :: IO [(Int, MemUnit, CpuUnit, MemUnit, CpuUnit)]
computeMerkleTreeCost =
  mapM compute [1, 2, 5, 10, 20, 50, 100, 500, 1000, 10000]
 where
  compute numElems = do
    utxo <- fmap Plutus.toBuiltin <$> genFakeUTxOs numElems

    let (memberMem, memberCpu) = fromRight (0, 0) $ executionCostForMember utxo
        (builderMem, builderCpu) = fromRight (0, 0) $ executionCostForBuilder utxo
    pure (numElems, MemUnit memberMem, CpuUnit memberCpu, MemUnit builderMem, CpuUnit builderCpu)

  -- NOTE: assume the size of a UTXO is around  60 bytes
  genFakeUTxOs numElems = generate (vectorOf numElems $ BS.pack <$> vectorOf 60 arbitrary)

executionCostForMember :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForMember utxo =
  let tree = MT.fromList utxo
      accumulateCost e acc =
        acc >>= \(curMem, curCpu) ->
          let proof = fromJust $ MT.mkProof e tree
           in case evaluateScriptExecutionUnits merkleTreeMemberValidator (e, MT.rootHash tree, proof) of
                Right ExecutionUnits{executionMemory, executionSteps} ->
                  Right (executionMemory + curMem, executionSteps + curCpu)
                Left err -> Left err
   in foldr accumulateCost (Right (0, 0)) utxo

executionCostForBuilder :: [Plutus.BuiltinByteString] -> Either Text (Natural, Natural)
executionCostForBuilder utxo =
  let tree = MT.fromList utxo
      root = rootHash tree
   in evaluateScriptExecutionUnits merkleTreeBuilderValidator (utxo, root) <&> \case
        ExecutionUnits{executionMemory, executionSteps} -> (executionMemory, executionSteps)
