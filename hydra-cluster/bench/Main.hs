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
import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.HUnit.Lang (formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

main :: IO ()
main =
  execParser benchOptionsParser >>= \case
    StandaloneOptions{workDirectory = Just benchDir, outputDirectory, timeoutSeconds, startingNodeId, scalingFactor, clusterSize} -> do
      existsDir <- doesDirectoryExist benchDir
      if existsDir
        then replay outputDirectory timeoutSeconds startingNodeId benchDir
        else createDirectory benchDir >> play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId benchDir
    StandaloneOptions{workDirectory = Nothing, outputDirectory, timeoutSeconds, scalingFactor, clusterSize, startingNodeId} -> do
      tmpDir <- createSystemTempDirectory "bench"
      play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId tmpDir
    DatasetOptions{datasetFiles, outputDirectory, timeoutSeconds, startingNodeId} -> do
      benchDir <- createSystemTempDirectory "bench"
      datasets <- mapM loadDataset datasetFiles
      let targets = zip datasets $ (benchDir </>) . show <$> [1 .. length datasets]
      forM_ (snd <$> targets) (createDirectoryIfMissing True)
      run outputDirectory timeoutSeconds startingNodeId targets
 where
  play outputDirectory timeoutSeconds scalingFactor clusterSize startingNodeId benchDir = do
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    pparams <-
      eitherDecodeFileStrict' ("config" </> "devnet" </> "genesis-shelley.json") >>= \case
        Left err -> fail $ show err
        Right shelleyGenesis ->
          pure $ fromLedgerPParams ShelleyBasedEraShelley (sgProtocolParams shelleyGenesis)
    dataset <- generateConstantUTxODataset pparams (fromIntegral clusterSize) numberOfTxs
    saveDataset (benchDir </> "dataset.cbor") dataset
    run outputDirectory timeoutSeconds startingNodeId [(dataset, benchDir)]

  replay outputDirectory timeoutSeconds startingNodeId benchDir = do
    dataset <- loadDataset $ benchDir </> "dataset.cbor"
    putStrLn $ "Using UTxO and Transactions from: " <> benchDir
    run outputDirectory timeoutSeconds startingNodeId [(dataset, benchDir)]

  run outputDirectory timeoutSeconds startingNodeId targets = do
    results <- forM targets $ \(dataset, dir) -> do
      putTextLn $ "Replaying dataset " <> show dataset.title
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
