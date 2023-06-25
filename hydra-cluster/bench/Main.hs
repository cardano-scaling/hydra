module Main where

import Hydra.Prelude
import Test.Hydra.Prelude

import Bench.EndToEnd (Summary (..), bench)
import Data.Aeson (eitherDecodeFileStrict', encodeFile)
import Data.ByteString (hPut)
import Data.Fixed (Nano)
import Data.Time (nominalDiffTimeToSeconds)
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
  command,
  execParser,
  fullDesc,
  header,
  help,
  helpDoc,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  str,
  strOption,
  value,
 )
import Options.Applicative.Builder (argument)
import Options.Applicative.Help (Doc, align, fillSep, line, (<+>))
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.HUnit.Lang (HUnitFailure (..), formatFailureReason)
import Test.QuickCheck (generate, getSize, scale)

data Options
  = StandaloneOptions
      { workDirectory :: Maybe FilePath
      , outputDirectory :: Maybe FilePath
      , scalingFactor :: Int
      , timeoutSeconds :: DiffTime
      , clusterSize :: Word64
      , startingNodeId :: Int
      }
  | DatasetOptions
      { datasetFiles :: [FilePath]
      , outputDirectory :: Maybe FilePath
      , scalingFactor :: Int
      , timeoutSeconds :: DiffTime
      , clusterSize :: Word64
      , startingNodeId :: Int
      }

benchOptionsParser :: ParserInfo Options
benchOptionsParser =
  info
    ( hsubparser
        ( command "single" standaloneOptionsInfo
            <> command "datasets" datasetOptionsInfo
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc
          "Starts a cluster of Hydra nodes interconnected through a network and \
          \talking to a local cardano devnet, generates an initial UTxO set and a bunch \
          \of valid transactions, and send those transactions to the cluster as \
          \fast as possible.\n \
          \Arguments control various parameters of the run, like number of nodes, \
          \number of transactions generated, or the 'scenarios' to run. See individual \
          \help for each command for more usage info."
        <> header "bench - load tester for Hydra node cluster"
    )

standaloneOptionsInfo :: ParserInfo Options
standaloneOptionsInfo =
  info
    standaloneOptionsParser
    (progDesc "Run a single scenario, generating or reusing a previous dataset, into some directory.")

standaloneOptionsParser :: Parser Options
standaloneOptionsParser =
  StandaloneOptions
    <$> optional
      ( strOption
          ( long "work-directory"
              <> helpDoc
                ( Just $
                    "Directory containing generated transactions, UTxO set, log files for spawned processes, etc."
                      <> item
                        [ "If the directory exists, it's assumed to be used for replaying"
                        , "a previous benchmark and is expected to contain 'txs.json' and"
                        , "'utxo.json' files,"
                        ]
                      <> item
                        [ "If the directory does not exist, it will be created and"
                        , "populated with new transactions and UTxO set."
                        ]
                )
          )
      )
    <*> optional outputDirectoryParser
    <*> scalingFactorParser
    <*> timeoutParser
    <*> clusterSizeParser
    <*> startingNodeIdParser

item :: [Doc] -> Doc
item items = line <> ("* " <+> align (fillSep items))

outputDirectoryParser :: Parser FilePath
outputDirectoryParser =
  strOption
    ( long "output-directory"
        <> metavar "DIR"
        <> help
          "The directory where to output markdown-formatted benchmark results. \
          \ If not set, raw text summary will be printed to the console. (default: none)"
    )

scalingFactorParser :: Parser Int
scalingFactorParser =
  option
    auto
    ( long "scaling-factor"
        <> value 100
        <> metavar "INT"
        <> help "The scaling factor to apply to transactions generator (default: 100)"
    )

timeoutParser :: Parser DiffTime
timeoutParser =
  option
    auto
    ( long "timeout"
        <> value 600.0
        <> metavar "SECONDS"
        <> help
          "The timeout for the run, in seconds (default: '600s')"
    )

clusterSizeParser :: Parser Word64
clusterSizeParser =
  option
    auto
    ( long "cluster-size"
        <> value 3
        <> metavar "INT"
        <> help
          "The number of Hydra nodes to start and connect (default: 3)"
    )

startingNodeIdParser :: Parser Int
startingNodeIdParser =
  option
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

datasetOptionsInfo :: ParserInfo Options
datasetOptionsInfo =
  info
    datasetOptionsParser
    ( progDesc
        "Run scenarios from one or several dataset files, concatenating the \
        \ output to single document. This is useful to produce a summary \
        \ page describing alternative runs."
    )

datasetOptionsParser :: Parser Options
datasetOptionsParser =
  DatasetOptions
    <$> many filepathParser
    <*> optional outputDirectoryParser
    <*> scalingFactorParser
    <*> timeoutParser
    <*> clusterSizeParser
    <*> startingNodeIdParser

filepathParser :: Parser FilePath
filepathParser =
  argument
    str
    ( metavar "FILE"
        <> help "Path to a JSON-formatted dataset descriptor file."
    )

main :: IO ()
main =
  execParser benchOptionsParser >>= \case
    o@StandaloneOptions{workDirectory = Just benchDir} -> do
      existsDir <- doesDirectoryExist benchDir
      if existsDir
        then replay o benchDir
        else createDirectory benchDir >> play o benchDir
    o -> do
      tmpDir <- createSystemTempDirectory "bench"
      play o tmpDir
 where
  play options@StandaloneOptions{scalingFactor, clusterSize} benchDir = do
    numberOfTxs <- generate $ scale (* scalingFactor) getSize
    pparams <-
      eitherDecodeFileStrict' ("config" </> "devnet" </> "genesis-shelley.json") >>= \case
        Left err -> fail $ show err
        Right shelleyGenesis ->
          pure $ fromLedgerPParams ShelleyBasedEraShelley (sgProtocolParams shelleyGenesis)
    dataset <- generateConstantUTxODataset pparams (fromIntegral clusterSize) numberOfTxs
    saveDataset benchDir dataset
    run options benchDir dataset
  play _ _ = error "Not implemented"

  replay options benchDir = do
    datasets <- either die pure =<< eitherDecodeFileStrict' (benchDir </> "dataset.json")
    putStrLn $ "Using UTxO and Transactions from: " <> benchDir
    run options benchDir datasets

  run options@StandaloneOptions{timeoutSeconds, clusterSize, startingNodeId} benchDir datasets = do
    putStrLn $ "Test logs available in: " <> (benchDir </> "test.log")
    withArgs [] $
      try (bench startingNodeId timeoutSeconds benchDir datasets clusterSize) >>= \case
        Left (err :: HUnitFailure) ->
          benchmarkFailedWith benchDir err
        Right summary ->
          benchmarkSucceeded options benchDir summary
  run _ _ _ = error "Not implemented"

  saveDataset tmpDir dataset = do
    let txsFile = tmpDir </> "dataset.json"
    putStrLn $ "Writing dataset to: " <> txsFile
    encodeFile txsFile dataset

benchmarkFailedWith :: FilePath -> HUnitFailure -> IO ()
benchmarkFailedWith benchDir (HUnitFailure sourceLocation reason) = do
  putStrLn $ "Benchmark failed " <> formatLocation sourceLocation <> ": " <> formatFailureReason reason
  putStrLn $ "To re-run with same dataset, pass '--work-directory=" <> benchDir <> "' to the executable"
  exitFailure
 where
  formatLocation = maybe "" (\loc -> "at " <> prettySrcLoc loc)

benchmarkSucceeded :: Options -> FilePath -> Summary -> IO ()
benchmarkSucceeded StandaloneOptions{outputDirectory, clusterSize} _ Summary{numberOfTxs, averageConfirmationTime, percentBelow100ms} = do
  now <- getCurrentTime
  maybe dumpToStdout (writeMarkdownReportTo now) outputDirectory
 where
  dumpToStdout = do
    putTextLn $ "Confirmed txs: " <> show numberOfTxs
    putTextLn $ "Average confirmation time (ms): " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime)
    putTextLn $ "Confirmed below 100ms: " <> show percentBelow100ms <> "%"

  writeMarkdownReportTo now outputDir = do
    existsDir <- doesDirectoryExist outputDir
    unless existsDir $ createDirectory outputDir
    withFile (outputDir </> "end-to-end-benchmarks.md") WriteMode $ \hdl -> do
      hPut hdl $ encodeUtf8 $ unlines $ pageHeader now
      hPut hdl $ encodeUtf8 $ unlines formattedSummary

  pageHeader :: UTCTime -> [Text]
  pageHeader now =
    [ "--- "
    , "sidebar_label: 'End-to-End Benchmarks' "
    , "sidebar_position: 4 "
    , "--- "
    , ""
    , "# End-To-End Benchmark Results "
    , ""
    , "This page is intended to collect the latest end-to-end benchmarks \
      \ results produced by Hydra's Continuous Integration system from \
      \ the latest `master` code."
    , ""
    , ":::caution"
    , ""
    , "Please take those results with a grain of \
      \ salt as they are currently produced from very limited cloud VMs and not controlled \
      \ hardware. Instead of focusing on the _absolute_ results, the emphasis \
      \ should be on relative results, eg. how the timings for a scenario \
      \ evolve as the code changes."
    , ""
    , ":::"
    , ""
    , "_Generated at_  " <> show now
    , ""
    ]

  formattedSummary :: [Text]
  formattedSummary =
    [ "## Baseline Scenario"
    , ""
    , -- TODO: make the description part of the Dataset
      "This scenario represents a minimal case and as such is a good baseline against which \
      \ to assess the overhead introduced by more complex setups. There is a single hydra-node \
      \ with a single client submitting single input and single output transactions with a \
      \ constant UTxO set of 1."
    , ""
    , "| Number of nodes |  " <> show clusterSize <> " | "
    , "| -- | -- |"
    , "| _Number of txs_ | " <> show numberOfTxs <> " |"
    , "| _Avg. Confirmation Time (ms)_ | " <> show (nominalDiffTimeToMilliseconds averageConfirmationTime) <> " |"
    , "| _Share of Txs (%) < 100ms_ | " <> show percentBelow100ms <> " |"
    ]

  nominalDiffTimeToMilliseconds :: NominalDiffTime -> Nano
  nominalDiffTimeToMilliseconds = fromRational . (* 1000) . toRational . nominalDiffTimeToSeconds
benchmarkSucceeded _ _ _ = error "Not implemented"
