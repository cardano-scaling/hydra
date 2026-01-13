{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench, benchDemo)
import Bench.Options (BenchType (..), Options (..), benchOptionsParser)
import Bench.Summary (Summary (..), SystemStats, errorSummary, markdownReport, textReport)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (Dataset (..), generateConstantUTxODataset, generateDemoUTxODataset, generateGrowingUTxODataset)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing, listDirectory, removeDirectoryRecursive)
import System.Environment (withArgs)
import System.FilePath (takeDirectory, (</>))
import Test.HUnit.Lang (formatFailureReason)
import Test.QuickCheck (generate)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  execParser benchOptionsParser >>= \case
    StandaloneOptions{outputDirectory, timeoutSeconds, numberOfTxs, clusterSize, startingNodeId, benchType} -> do
      (_, faucetSk) <- keysFor Faucet
      -- XXX: Scaling factor is unintuitive and should rather be a number of txs directly
      putStrLn $ "Generating dataset with number of txs: " <> show numberOfTxs
      dataset <- generate $ do
        case benchType of
          Constant -> generateConstantUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
          Growing -> generateGrowingUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
      workDir <- maybe (createTempDir "bench-single") checkEmpty outputDirectory
      saveDataset (workDir </> "dataset.json") dataset
      let action = bench startingNodeId timeoutSeconds
      results <- runSingle dataset workDir action
      summarizeResults outputDirectory [results]
    DemoOptions{outputDirectory, numberOfTxs, timeoutSeconds, networkId, nodeSocket, hydraClients} -> do
      (_, faucetSk) <- keysFor Faucet
      dataset <- generateDemoUTxODataset networkId nodeSocket faucetSk (length hydraClients) numberOfTxs
      workDir <- maybe (createTempDir "bench-demo") checkEmpty outputDirectory
      results <-
        runSingle dataset workDir $
          benchDemo networkId nodeSocket timeoutSeconds hydraClients
      summarizeResults outputDirectory [results]
      removeDirectoryRecursive workDir
    DatasetOptions{datasetFiles, outputDirectory, timeoutSeconds, startingNodeId} -> do
      let action = bench startingNodeId timeoutSeconds
      putTextLn $ "Running benchmark with datasets: " <> show datasetFiles
      datasets <- forM datasetFiles loadDataset
      results <- forM datasets $ \dataset -> do
        withTempDir "bench-dataset" $ \dir -> do
          -- XXX: Wait between each bench run to give the OS time to cleanup resources??
          threadDelay 10
          runSingle dataset dir action
      summarizeResults outputDirectory results
 where
  checkEmpty fp = do
    createDirectoryIfMissing True fp
    listDirectory fp >>= \case
      [] -> pure fp
      _files -> die $ "ERROR: Output directory not empty: " <> fp

  runSingle ::
    Dataset ->
    FilePath ->
    (FilePath -> Dataset -> IO (Summary, SystemStats)) ->
    IO (Either (Dataset, FilePath, Summary, BenchmarkFailed) (Summary, SystemStats))
  runSingle dataset dir action = do
    withArgs [] $ do
      try @_ @HUnitFailure (action dir dataset) >>= \case
        Left exc -> pure $ Left (dataset, dir, errorSummary dataset exc, TestFailed exc)
        Right (summary@Summary{totalTxs, numberOfTxs, numberOfInvalidTxs}, systemStats)
          | numberOfTxs /= totalTxs -> pure $ Left (dataset, dir, summary, NotEnoughTransactions numberOfTxs totalTxs)
          | numberOfInvalidTxs == 0 -> pure $ Right (summary, systemStats)
          | otherwise -> pure $ Left (dataset, dir, summary, InvalidTransactions numberOfInvalidTxs)

  summarizeResults :: Maybe FilePath -> [Either (Dataset, FilePath, Summary, BenchmarkFailed) (Summary, SystemStats)] -> IO ()
  summarizeResults outputDirectory results = do
    let (failures, summaries) = partitionEithers results
    case failures of
      [] -> writeBenchmarkReport outputDirectory summaries
      errs -> do
        forM_ errs $ \(_, dir, summary, exc) -> do
          writeBenchmarkReport outputDirectory [(summary, [])]
          benchmarkFailedWith dir exc
        exitFailure

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

writeBenchmarkReport :: Maybe FilePath -> [(Summary, SystemStats)] -> IO ()
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
