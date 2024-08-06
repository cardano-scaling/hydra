{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench, benchDemo)
import Bench.Options (Options (..), benchOptionsParser)
import Bench.Summary (Summary (..), markdownReport, textReport)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (ClientKeys (..), Dataset (..), genDatasetConstantUTxO, generateConstantUTxODataset)
import Options.Applicative (execParser)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath (takeFileName, (</>))
import Test.HUnit.Lang (formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

main :: IO ()
main =
  execParser benchOptionsParser >>= \case
    StandaloneOptions{workDirectory = Just workDir, outputDirectory, timeoutSeconds, startingNodeId, scalingFactor, clusterSize} -> do
      -- XXX: This option is a bit weird as it allows to re-run a test by
      -- providing --work-directory, which is now redundant of the dataset
      -- sub-command.
      existsDir <- doesDirectoryExist workDir
      if existsDir
        then replay outputDirectory timeoutSeconds startingNodeId workDir
        else play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir
    StandaloneOptions{workDirectory = Nothing, outputDirectory, timeoutSeconds, scalingFactor, clusterSize, startingNodeId} -> do
      workDir <- createSystemTempDirectory "bench"
      play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir
    DatasetOptions{datasetFiles, outputDirectory, timeoutSeconds, startingNodeId} -> do
      let action = bench startingNodeId timeoutSeconds
      run outputDirectory datasetFiles action
    DemoOptions{outputDirectory, scalingFactor, timeoutSeconds, networkId, nodeSocket} -> do
      workDir <- createSystemTempDirectory "demo-bench"
      (_, faucetSk) <- keysFor Faucet
      clientKeys <- do
        aliceSk <- snd <$> keysFor Alice
        aliceFundsSk <- snd <$> keysFor AliceFunds
        bobSk <- snd <$> keysFor Bob
        bobFundsSk <- snd <$> keysFor BobFunds
        carolSk <- snd <$> keysFor Carol
        carolFundsSk <- snd <$> keysFor CarolFunds
        let alice = ClientKeys aliceSk aliceFundsSk
            bob = ClientKeys bobSk bobFundsSk
            carol = ClientKeys carolSk carolFundsSk
        pure [alice, bob, carol]
      playDemo outputDirectory timeoutSeconds scalingFactor faucetSk clientKeys workDir networkId nodeSocket
 where
  playDemo outputDirectory timeoutSeconds scalingFactor faucetSk clientKeys workDir networkId nodeSocket = do
    putStrLn $ "Generating single dataset in work directory: " <> workDir
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    dataset <- generate $ genDatasetConstantUTxO faucetSk clientKeys numberOfTxs
    let datasetPath = workDir </> "dataset.json"
    saveDataset datasetPath dataset
    let action = benchDemo networkId nodeSocket timeoutSeconds
    run outputDirectory [datasetPath] action

  play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir = do
    putStrLn $ "Generating single dataset in work directory: " <> workDir
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    dataset <- generateConstantUTxODataset (fromIntegral clusterSize) numberOfTxs
    let datasetPath = workDir </> "dataset.json"
    saveDataset datasetPath dataset
    let action = bench startingNodeId timeoutSeconds
    run outputDirectory [datasetPath] action

  replay outputDirectory timeoutSeconds startingNodeId benchDir = do
    let datasetPath = benchDir </> "dataset.json"
    putStrLn $ "Replaying single dataset from work directory: " <> datasetPath
    let action = bench startingNodeId timeoutSeconds
    run outputDirectory [datasetPath] action

  run outputDirectory datasetFiles action = do
    results <- forM datasetFiles $ \datasetPath -> do
      putTextLn $ "Running benchmark with dataset " <> show datasetPath
      dataset <- loadDataset datasetPath
      withTempDir ("bench-" <> takeFileName datasetPath) $ \dir ->
        withArgs [] $ do
          -- XXX: Wait between each bench run to give the OS time to cleanup resources??
          threadDelay 10
          try @_ @HUnitFailure (action dir dataset) >>= \case
            Left exc -> pure $ Left (dataset, dir, TestFailed exc)
            Right summary@Summary{numberOfInvalidTxs}
              | numberOfInvalidTxs == 0 -> pure $ Right summary
              | otherwise -> pure $ Left (dataset, dir, InvalidTransactions numberOfInvalidTxs)
    let (failures, summaries) = partitionEithers results
    case failures of
      [] -> benchmarkSucceeded outputDirectory summaries
      errs -> mapM_ (\(_, dir, exc) -> benchmarkFailedWith dir exc) errs >> exitFailure

  loadDataset :: FilePath -> IO Dataset
  loadDataset f = do
    putStrLn $ "Reading dataset from: " <> f
    eitherDecodeFileStrict' f >>= either (die . show) pure

  saveDataset :: FilePath -> Dataset -> IO ()
  saveDataset f dataset = do
    putStrLn $ "Writing dataset to: " <> f
    encodeFile f dataset

data BenchmarkFailed
  = TestFailed HUnitFailure
  | InvalidTransactions Int

benchmarkFailedWith :: FilePath -> BenchmarkFailed -> IO ()
benchmarkFailedWith benchDir = \case
  (TestFailed (HUnitFailure sourceLocation reason)) -> do
    putStrLn $ "Benchmark failed " <> formatLocation sourceLocation <> ": " <> formatFailureReason reason
    putStrLn $ "To re-run with same dataset, pass '--work-directory=" <> benchDir <> "' to the executable"
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

benchmarkSucceeded :: Maybe FilePath -> [Summary] -> IO ()
benchmarkSucceeded outputDirectory summaries = do
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
