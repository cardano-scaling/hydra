{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (bench)
import Bench.Options (Options (..), benchOptionsParser)
import Bench.Summary (Summary (..), markdownReport, textReport)
import Cardano.Binary (decodeFull, serialize)
import Data.Aeson (eitherDecodeFileStrict')
import Data.ByteString (hPut)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (
  ShelleyBasedEra (..),
  ShelleyGenesis (..),
  fromLedgerPParams,
 )
import Hydra.Generator (Dataset (..), generateConstantUTxODataset)
import Options.Applicative (
  execParser,
 )
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))
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
      run outputDirectory timeoutSeconds startingNodeId datasetFiles
 where
  play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId workDir = do
    putStrLn $ "Generating single dataset in work directory: " <> workDir
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    pparams <-
      eitherDecodeFileStrict' ("config" </> "devnet" </> "genesis-shelley.json") >>= \case
        Left err -> fail $ show err
        Right shelleyGenesis ->
          pure $ fromLedgerPParams ShelleyBasedEraShelley (sgProtocolParams shelleyGenesis)
    dataset <- generateConstantUTxODataset pparams (fromIntegral clusterSize) numberOfTxs
    let datasetPath = workDir </> "dataset.cbor"
    saveDataset datasetPath dataset
    run outputDirectory timeoutSeconds startingNodeId [datasetPath]

  replay outputDirectory timeoutSeconds startingNodeId benchDir = do
    let datasetPath = benchDir </> "dataset.cbor"
    putStrLn $ "Replaying single dataset from work directory: " <> datasetPath
    run outputDirectory timeoutSeconds startingNodeId [datasetPath]

  run outputDirectory timeoutSeconds startingNodeId datasetFiles = do
    results <- forM datasetFiles $ \datasetPath -> do
      putTextLn $ "Running benchmark with dataset " <> show datasetPath
      dataset <- loadDataset datasetPath
      withTempDir ("bench-" <> takeFileName datasetPath) $ \dir ->
        withArgs [] $ do
          -- XXX: Wait between each bench run to give the OS time to cleanup resources??
          threadDelay 10
          try @_ @HUnitFailure (bench startingNodeId timeoutSeconds dir dataset) >>= \case
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
    readFileBS f >>= either (die . show) pure . (decodeFull . LBS.fromStrict . Base16.decodeLenient)

  saveDataset :: FilePath -> Dataset -> IO ()
  saveDataset f dataset = do
    putStrLn $ "Writing dataset to: " <> f
    createDirectoryIfMissing True $ takeDirectory f
    writeFileBS f $ Base16.encode $ LBS.toStrict $ serialize dataset

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
    now <- getCurrentTime
    let report = markdownReport now summaries
    createDirectoryIfMissing True outputDir
    withFile (outputDir </> "end-to-end-benchmarks.md") WriteMode $ \hdl -> do
      hPut hdl $ encodeUtf8 $ unlines report
