module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (Summary (..), bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.ByteString (hPut)
import Hydra.Cardano.Api (
  ShelleyBasedEra (..),
  ShelleyGenesis (..),
  fromLedgerPParams,
 )
import Hydra.Generator (generateConstantUTxODataset)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  strOption,
  value,
 )
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

data Options = Options
  { workDirectory :: Maybe FilePath
  , outputDirectory :: Maybe FilePath
  , scalingFactor :: Int
  , timeoutSeconds :: DiffTime
  , clusterSize :: Word64
  , startingNodeId :: Int
  }

benchOptionsParser :: Parser Options
benchOptionsParser =
  Options
    <$> optional
      ( strOption
          ( long "work-directory"
              <> help
                "Directory containing generated transactions, UTxO set, log files for spawned processes, etc. \
                \ * If the directory exists, it's assumed to be used for replaying \
                \   a previous benchmark and is expected to contain 'txs.json' and \
                \   'utxo.json' files, \
                \ * If the directory does not exist, it will be created and \
                \   populated with new transactions and UTxO set."
          )
      )
    <*> optional
      ( strOption
          ( long "output-directory"
              <> metavar "DIR"
              <> help
                "The directory where to output markdown-formatted benchmark results. \
                \ If not set, raw text summary will be printed to the console. (default: none)"
          )
      )
    <*> option
      auto
      ( long "scaling-factor"
          <> value 100
          <> metavar "INT"
          <> help "The scaling factor to apply to transactions generator (default: 100)"
      )
    <*> option
      auto
      ( long "timeout"
          <> value 600.0
          <> metavar "SECONDS"
          <> help
            "The timeout for the run, in seconds (default: '600s')"
      )
    <*> option
      auto
      ( long "cluster-size"
          <> value 3
          <> metavar "INT"
          <> help
            "The number of Hydra nodes to start and connect (default: 3)"
      )
    <*> option
      auto
      ( long "starting-node-id"
          <> short 'i'
          <> value 0
          <> metavar "INT"
          <> help
            "The starting point for ids allocated to nodes in the cluster. This \
            \ id controls TCP ports allocation for various servers run by the nodes, \
            \ it's useful to change if local processes on the machine running the \
            \ benchmark conflicts with default ports allocation scheme (default: 0)"
      )

benchOptions :: ParserInfo Options
benchOptions =
  info
    (benchOptionsParser <**> helper)
    ( fullDesc
        <> progDesc
          "Starts a cluster of Hydra nodes interconnected through a network and \
          \talking to a local cardano devnet, generates an initial UTxO set and a bunch \
          \of valid transactions, and send those transactions to the cluster as \
          \fast as possible.\n \
          \Arguments can control various parameters of the run, like number of nodes, \
          \and number of transactions generated"
        <> header "bench - load tester for Hydra node cluster"
    )

main :: IO ()
main =
  execParser benchOptions >>= \case
    o@Options{workDirectory = Just benchDir} -> do
      existsDir <- doesDirectoryExist benchDir
      if existsDir
        then replay o benchDir
        else createDirectory benchDir >> play o benchDir
    o -> do
      tmpDir <- createSystemTempDirectory "bench"
      play o tmpDir
 where
  play options@Options{scalingFactor, clusterSize} benchDir = do
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    pparams <-
      eitherDecodeFileStrict' ("config" </> "devnet" </> "genesis-shelley.json") >>= \case
        Left err -> fail $ show err
        Right shelleyGenesis ->
          pure $ fromLedgerPParams ShelleyBasedEraShelley (sgProtocolParams shelleyGenesis)
    dataset <- generateConstantUTxODataset pparams (fromIntegral clusterSize) numberOfTxs
    saveDataset benchDir dataset
    run options benchDir dataset

  replay options benchDir = do
    datasets <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "dataset.json")
    putStrLn $ "Using UTxO and Transactions from: " <> benchDir
    run options benchDir datasets

  run options@Options{timeoutSeconds, clusterSize, startingNodeId} benchDir datasets = do
    putStrLn $ "Test logs available in: " <> (benchDir </> "test.log")
    withArgs [] $
      try (bench startingNodeId timeoutSeconds benchDir datasets clusterSize) >>= \case
        Left (err :: HUnitFailure) ->
          benchmarkFailedWith benchDir err
        Right summary ->
          benchmarkSucceeded options benchDir summary

  saveDataset tmpDir dataset = do
    let txsFile = tmpDir </> "dataset.json"
    putStrLn $ "Writing dataset to: " <> txsFile
    encodeFile txsFile dataset

benchmarkFailedWith :: FilePath -> HUnitFailure -> IO ()
benchmarkFailedWith benchDir (HUnitFailure _ reason) = do
  putStrLn $ "Benchmark failed: " <> formatFailureReason reason
  putStrLn $ "To re-run with same dataset, pass '--work-directory=" <> benchDir <> "' to the executable"
  exitFailure

benchmarkSucceeded :: Options -> FilePath -> Summary -> IO ()
benchmarkSucceeded Options{outputDirectory, clusterSize} _ Summary{numberOfTxs, averageConfirmationTime, percentBelow100ms} =
  maybe dumpToStdout writeMarkdownReportTo outputDirectory
 where
  dumpToStdout = do
    putTextLn $ "Confirmed txs: " <> show numberOfTxs
    putTextLn $ "Average confirmation time (ms): " <> show averageConfirmationTime
    putTextLn $ "Confirmed below 100ms: " <> show percentBelow100ms <> "%"

  writeMarkdownReportTo outputDir = do
    existsDir <- doesDirectoryExist outputDir
    unless existsDir $ createDirectory outputDir
    withFile (outputDir </> "end-to-end-benchmarks.md") WriteMode $ \hdl -> do
      hPut hdl $ encodeUtf8 $ unlines pageHeader
      hPut hdl $ encodeUtf8 $ unlines $ formattedSummary

  pageHeader :: [Text]
  pageHeader =
    [ "--- "
    , "sidebar_label: 'End-to-End Benchmarks' "
    , "sidebar_position: 4 "
    , "--- "
    , ""
    , "# End-To-End Benchmark Results "
    , ""
    , "This page is intended to collect the latest end-to-end benchmarks \
      \ results produced by Hydra's Continuous Integration system from \
      \ the latest `master` code. Please take those results with a grain of \
      \ salt as they are produced from basic cloud VMs and not controlled \
      \ hardware. Instead of focusing on the _absolute_ results, the emphasis \
      \ should be on relative results, eg. how the timings for a scenario \
      \ evolve as the code changes."
    , ""
    , "_Generated at_  " <> show now
    , ""
    ]

  {-# NOINLINE now #-}
  now :: UTCTime
  now = unsafePerformIO getCurrentTime

  summaryTitle = "Benchmark (nodes=" <> show clusterSize <> ")"

  formattedSummary :: [Text]
  formattedSummary =
    [ "## " <> summaryTitle
    , ""
    , "| _Number of txs_ | " <> show numberOfTxs <> " |"
    , "| _Avg. Confirmation Time (ms)_ | " <> show averageConfirmationTime <> " |"
    , "| _Share of Txs (%) < 100ms_ | " <> show percentBelow100ms <> " |"
    ]
