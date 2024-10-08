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
import Hydra.Generator (ClientKeys (..), Dataset (..), generateConstantUTxODataset, generateDemoUTxODataset)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing)
import System.Environment (withArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Test.HUnit.Lang (formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  execParser benchOptionsParser >>= \case
    StandaloneOptions{outputDirectory, timeoutSeconds, scalingFactor, clusterSize, startingNodeId} -> do
      workDir <- createSystemTempDirectory "bench"
      play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir
    DatasetOptions{datasetFiles, outputDirectory, timeoutSeconds, startingNodeId} -> do
      let action = bench startingNodeId timeoutSeconds
      run outputDirectory datasetFiles action
    DemoOptions{outputDirectory, scalingFactor, timeoutSeconds, networkId, nodeSocket, hydraClients} -> do
      let action = benchDemo networkId nodeSocket timeoutSeconds hydraClients
      playDemo outputDirectory scalingFactor networkId nodeSocket action
 where
  playDemo outputDirectory scalingFactor networkId nodeSocket action = do
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    let actors = [(Alice, AliceFunds), (Bob, BobFunds), (Carol, CarolFunds)]
    let toClientKeys (actor, funds) = do
          sk <- snd <$> keysFor actor
          fundsSk <- snd <$> keysFor funds
          pure $ ClientKeys sk fundsSk
    clientKeys <- forM actors toClientKeys
    dataset <- generateDemoUTxODataset networkId nodeSocket clientKeys numberOfTxs
    results <- withTempDir "bench-demo" $ \dir -> do
      runSingle dataset action (fromMaybe dir outputDirectory)
    summarizeResults outputDirectory [results]

  play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir = do
    (_, faucetSk) <- keysFor Faucet
    putStrLn $ "Generating single dataset in work directory: " <> workDir
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    dataset <- generate $ generateConstantUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
    let datasetPath = workDir </> "dataset.json"
    saveDataset datasetPath dataset
    let action = bench startingNodeId timeoutSeconds
    run outputDirectory [datasetPath] action

  runSingle dataset action dir = do
    withArgs [] $ do
      try @_ @HUnitFailure (action dir dataset) >>= \case
        Left exc -> pure $ Left (dataset, dir, errorSummary dataset exc, TestFailed exc)
        Right summary@Summary{totalTxs, numberOfTxs, numberOfInvalidTxs}
          | numberOfTxs /= totalTxs -> pure $ Left (dataset, dir, summary, NotEnoughTransactions numberOfTxs totalTxs)
          | numberOfInvalidTxs == 0 -> pure $ Right summary
          | otherwise -> pure $ Left (dataset, dir, summary, InvalidTransactions numberOfInvalidTxs)

  run outputDirectory datasetFiles action = do
    results <- forM datasetFiles $ \datasetPath -> do
      putTextLn $ "Running benchmark with dataset " <> show datasetPath
      dataset <- loadDataset datasetPath
      withTempDir ("bench-" <> takeFileName datasetPath) $ \dir -> do
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
