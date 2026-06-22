{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (BenchRunOptions (..), bench, benchDemo)
import Bench.Options (Options (..), UTxOSize (..), benchOptionsParser)
import Bench.Summary (Summary (..), SystemStats, errorSummary, markdownReport, matrixMarkdownReport, textReport)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.List qualified as List
import Data.Text qualified as T
import Hydra.Cluster.Fixture (Actor (..))
import Hydra.Cluster.Util (keysFor)
import Hydra.Generator (Dataset (..), generateConstantUTxODataset, generateDemoUTxODataset, generateGrowingUTxODataset, generateMixedUTxODataset)
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
    StandaloneOptions{outputDirectory, timeoutSeconds, startingNodeId, datasetFiles, incrementalOps, waitForTxValid} -> do
      datasets <- forM datasetFiles loadDataset
      putTextLn $ "Running benchmark with datasets: " <> show datasetFiles
      let action = bench startingNodeId timeoutSeconds BenchRunOptions{incrementalOps, waitForTxValid}
      results <- forM datasets $ \dataset ->
        withTempDir "bench-dataset" $ \dir -> do
          threadDelay 10
          runSingle dataset dir action
      summarizeResults outputDirectory results
    DemoOptions{outputDirectory, numberOfTxs, timeoutSeconds, networkId, nodeSocket, hydraClients, pumbaCommand} -> do
      (_, faucetSk) <- keysFor Faucet
      dataset <- generateDemoUTxODataset networkId nodeSocket faucetSk (length hydraClients) numberOfTxs
      workDir <- maybe (createTempDir "bench-demo") checkEmpty outputDirectory
      results <-
        runSingle dataset workDir $
          benchDemo networkId nodeSocket timeoutSeconds hydraClients pumbaCommand BenchRunOptions{incrementalOps = False, waitForTxValid = False}
      summarizeResults outputDirectory [results]
      removeDirectoryRecursive workDir
    DatasetOptions{outputDirectory, timeoutSeconds, datasetUTxO, numberOfTxs, clusterSize, startingNodeId, incrementalOps, waitForTxValid} -> do
      (_, faucetSk) <- keysFor Faucet
      workDir <- maybe (createTempDir "bench-e2e") checkEmpty outputDirectory
      let action = bench startingNodeId timeoutSeconds BenchRunOptions{incrementalOps, waitForTxValid}
      dataset <- generate $ case datasetUTxO of
        Constant -> generateConstantUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
        Growing -> generateGrowingUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
        Mixed -> generateMixedUTxODataset faucetSk (fromIntegral clusterSize) numberOfTxs
      saveDataset (workDir </> "dataset.json") dataset
      putStrLn $ "Saved dataset in: " <> (workDir </> "dataset.json")
      results <- do
        -- XXX: Wait between each bench run to give the OS time to cleanup resources??
        threadDelay 10
        runSingle dataset workDir action
      summarizeResults outputDirectory [results]
    MatrixOptions{outputDirectory, timeoutSeconds, numberOfTxs, startingNodeId, clusterSizes, utxoShapes, incrementalModes, waitForTxValidModes} -> do
      (_, faucetSk) <- keysFor Faucet
      -- NOTE: Unlike a standalone matrix run, the docs pipeline points
      -- --output-directory at the shared benchmarks/ directory that already
      -- holds other reports (tx-cost, ledger-bench, end-to-end-benchmarks.md),
      -- so we must not require it to be empty. Per-cell data goes into fresh
      -- cell-N/ subdirs and the report is the distinct scenarios.md file.
      workDir <- maybe (createTempDir "bench-matrix") ensureDirectory outputDirectory
      let cells =
            [ (cs, sh, im, wt)
            | cs <- clusterSizes
            , sh <- utxoShapes
            , im <- incrementalModes
            , wt <- waitForTxValidModes
            ]
      putStrLn $ "Running matrix with " <> show (length cells) <> " cells"
      results <- forM (zip [0 :: Int ..] cells) $ \(i, (cs, sh, im, wt)) -> do
        let cellDir = workDir </> ("cell-" <> show i)
        createDirectoryIfMissing True cellDir
        dataset <- generate $ case sh of
          Constant -> generateConstantUTxODataset faucetSk (fromIntegral cs) numberOfTxs
          Growing -> generateGrowingUTxODataset faucetSk (fromIntegral cs) numberOfTxs
          Mixed -> generateMixedUTxODataset faucetSk (fromIntegral cs) numberOfTxs
        let labelled = dataset{title = Just (matrixCellTitle cs sh im wt)}
        saveDataset (cellDir </> "dataset.json") labelled
        threadDelay 10
        let nodeIdOffset = startingNodeId + i * fromIntegral (List.maximum clusterSizes)
        let action = bench nodeIdOffset timeoutSeconds BenchRunOptions{incrementalOps = im, waitForTxValid = wt}
        -- Run the cluster in a throwaway dir, not 'cellDir': node working state
        -- (etcd WAL, cardano-node db, logs) must not reach the published docs.
        -- Only 'dataset.json' and the aggregated 'scenarios.md' are kept.
        withTempDir ("bench-matrix-cell-" <> show i) $ \runDir ->
          runSingle labelled runDir action
      summarizeMatrixResults outputDirectory results
 where
  matrixCellTitle :: Word64 -> UTxOSize -> Bool -> Bool -> Text
  matrixCellTitle cs sh im wt =
    "Nodes="
      <> T.pack (show cs)
      <> ", "
      <> T.pack (show sh)
      <> (if im then ", incremental ops on" else ", incremental ops off")
      <> (if wt then ", wait for tx valid" else ", fire and forget")
  checkEmpty fp = do
    createDirectoryIfMissing True fp
    listDirectory fp >>= \case
      [] -> pure fp
      _files -> die $ "ERROR: Output directory not empty: " <> fp

  ensureDirectory fp = do
    createDirectoryIfMissing True fp
    pure fp

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

  summarizeMatrixResults :: Maybe FilePath -> [Either (Dataset, FilePath, Summary, BenchmarkFailed) (Summary, SystemStats)] -> IO ()
  summarizeMatrixResults outputDirectory results = do
    -- For the matrix runner, include failed cells in the report with their
    -- error summaries so the reader can see which scenarios did not complete.
    -- A single flaky cell is logged but doesn't fail the matrix step; the
    -- generated scenarios.md keeps the remaining cells' useful data.
    let cells =
          flip map results $ \case
            Left (_, _, summary, _) -> (summary, [])
            Right s -> s
    writeMatrixReport outputDirectory cells
    forM_ (lefts results) $ \(_, dir, _, exc) -> benchmarkFailedWith dir exc
    let okCount = length (rights results)
        total = length results
    putStrLn $ "Matrix summary: " <> show okCount <> "/" <> show total <> " cells succeeded"
    when (okCount == 0) exitFailure

  loadDataset :: FilePath -> IO Dataset
  loadDataset f = do
    putStrLn $ "Reading datasets from: " <> f
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

writeMatrixReport :: Maybe FilePath -> [(Summary, SystemStats)] -> IO ()
writeMatrixReport outputDirectory summaries = do
  mapM_ putTextLn (concatMap textReport summaries)
  whenJust outputDirectory writeReport
 where
  writeReport outputDir = do
    let reportPath = outputDir </> "scenarios.md"
    putStrLn $ "Writing matrix report to: " <> reportPath
    now <- getCurrentTime
    let report = matrixMarkdownReport now summaries
    createDirectoryIfMissing True outputDir
    writeFileBS reportPath . encodeUtf8 $ unlines report
