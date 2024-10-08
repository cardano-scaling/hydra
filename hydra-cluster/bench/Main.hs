{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench, benchDemo)
import Bench.Options (Options (..), benchOptionsParser)
import Bench.Summary (Summary (..), errorSummary, markdownReport, textReport)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (Dataset (..), generateConstantUTxODataset, generateDemoUTxODataset)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.Environment (withArgs)
import System.FilePath (takeDirectory, (</>))
import Test.HUnit.Lang (formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  execParser benchOptionsParser >>= \case
    StandaloneOptions{outputDirectory, timeoutSeconds, scalingFactor, clusterSize, startingNodeId} -> do
      play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId
    DatasetOptions{datasetFiles, outputDirectory, timeoutSeconds, startingNodeId} -> do
      let action = bench startingNodeId timeoutSeconds
      putTextLn $ "Running benchmark with datasets: " <> show datasetFiles
      datasets <- forM datasetFiles loadDataset
      run outputDirectory datasets action
    DemoOptions{outputDirectory, scalingFactor, timeoutSeconds, networkId, nodeSocket, hydraClients} -> do
      let action = benchDemo networkId nodeSocket timeoutSeconds hydraClients
      playDemo outputDirectory scalingFactor networkId nodeSocket action
 where
  playDemo outputDirectory scalingFactor networkId nodeSocket action = do
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    hydraNodeKeys <- mapM (fmap snd . keysFor) [AliceFunds, BobFunds, CarolFunds]
    dataset <- generateDemoUTxODataset networkId nodeSocket hydraNodeKeys numberOfTxs
    workDir <- maybe (createTempDir "bench-demo") pure outputDirectory
    results <- runSingle dataset action workDir
    summarizeResults outputDirectory [results]
    removeDirectoryRecursive workDir

  play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId = do
    (_, faucetSk) <- keysFor Faucet
    -- XXX: Scaling factor is unintuitive and should rather be a number of txs directly
    putStrLn $ "Generating dataset with scaling factor: " <> show scalingFactor
    dataset <- generate $ do
      numberOfTxs <- scale (* scalingFactor) getSize
      generateConstantUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
    workDir <- maybe (createTempDir "bench-single") pure outputDirectory
    saveDataset (workDir </> "dataset.json") dataset
    let action = bench startingNodeId timeoutSeconds
    results <- runSingle dataset action workDir
    summarizeResults outputDirectory [results]

  runSingle dataset action dir = do
    withArgs [] $ do
      try @_ @HUnitFailure (action dir dataset) >>= \case
        Left exc -> pure $ Left (dataset, dir, errorSummary dataset exc, TestFailed exc)
        Right summary@Summary{totalTxs, numberOfTxs, numberOfInvalidTxs}
          | numberOfTxs /= totalTxs -> pure $ Left (dataset, dir, summary, NotEnoughTransactions numberOfTxs totalTxs)
          | numberOfInvalidTxs == 0 -> pure $ Right summary
          | otherwise -> pure $ Left (dataset, dir, summary, InvalidTransactions numberOfInvalidTxs)

  run outputDirectory datasets action = do
    results <- forM datasets $ \dataset -> do
      withTempDir "bench-dataset" $ \dir -> do
        -- XXX: Wait between each bench run to give the OS time to cleanup resources??
        threadDelay 10
        runSingle dataset action dir
    summarizeResults outputDirectory results

  summarizeResults :: Maybe FilePath -> [Either (Dataset, FilePath, Summary, BenchmarkFailed) Summary] -> IO ()
  summarizeResults outputDirectory results = do
    let (failures, summaries) = partitionEithers results
    case failures of
      [] -> writeBenchmarkReport outputDirectory summaries
      errs ->
        mapM_
          ( \(_, dir, summary, exc) ->
              writeBenchmarkReport outputDirectory [summary]
                >> benchmarkFailedWith dir exc
          )
          errs
          >> exitFailure

  loadDataset :: FilePath -> IO Dataset
  loadDataset f = do
    putStrLn $ "Reading dataset from: " <> f
    eitherDecodeFileStrict' f >>= either (die . show) pure

  saveDataset :: FilePath -> Dataset -> IO ()
  saveDataset f dataset = do
    createDirectoryIfMissing True (takeDirectory f)
    putStrLn $ "Writing dataset to: " <> f
    encodeFile f dataset

data BenchmarkFailed
  = TestFailed HUnitFailure
  | InvalidTransactions Int
  | NotEnoughTransactions Int Int

benchmarkFailedWith :: FilePath -> BenchmarkFailed -> IO ()
benchmarkFailedWith benchDir = \case
  (TestFailed (HUnitFailure sourceLocation reason)) -> do
    putStrLn $ "Benchmark failed " <> formatLocation sourceLocation <> ": " <> formatFailureReason reason
    putStrLn $ "To re-run with same dataset, pass '--work-directory=" <> benchDir <> "' to the executable"
  (NotEnoughTransactions actual expected) -> do
    putStrLn $ "Benchmark resulted in " <> show actual <> " transactions; but wanted " <> show expected <> "."
  (InvalidTransactions n) -> do
    putStrLn $ "Benchmark has " <> show n <> " invalid transactions"
    putStrLn $
      "Check logs in "
        <> benchDir
        <> " or re-run with same dataset, passing '--work-directory="
        <> benchDir
        <> "' to the 'single' command"
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

writeBenchmarkReport :: Maybe FilePath -> [Summary] -> IO ()
writeBenchmarkReport outputDirectory summaries = do
  dumpToStdout
  whenJust outputDirectory writeReport
 where
  dumpToStdout = mapM_ putTextLn (concatMap textReport summaries)

  writeReport outputDir = do
    let reportPath = outputDir </> "end-to-end-benchmarks.md"
    putStrLn $ "Writing report to: " <> reportPath
    now <- getCurrentTime
    let report = markdownReport now summaries
    createDirectoryIfMissing True outputDir
    writeFileBS reportPath . encodeUtf8 $ unlines report
