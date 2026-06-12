module Bench.Options where

import Hydra.Prelude

import Data.Text (splitOn)
import Hydra.Cardano.Api (NetworkId, SocketPath)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Network (Host, readHost)
import Hydra.Options (networkIdParser, nodeSocketParser)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  flag,
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  short,
  str,
  strOption,
  value,
 )
import Options.Applicative.Builder (argument)

data UTxOSize = Constant | Growing | Mixed deriving stock (Eq, Show, Read)

data Options
  = StandaloneOptions
      { datasetFiles :: [FilePath]
      , outputDirectory :: Maybe FilePath
      , timeoutSeconds :: NominalDiffTime
      , startingNodeId :: Int
      , incrementalOps :: Bool
      , waitForTxValid :: Bool
      }
  | DatasetOptions
      { outputDirectory :: Maybe FilePath
      , timeoutSeconds :: NominalDiffTime
      , datasetUTxO :: UTxOSize
      , numberOfTxs :: Int
      , clusterSize :: Word64
      , startingNodeId :: Int
      , incrementalOps :: Bool
      , waitForTxValid :: Bool
      }
  | DemoOptions
      { outputDirectory :: Maybe FilePath
      , numberOfTxs :: Int
      , timeoutSeconds :: NominalDiffTime
      , networkId :: NetworkId
      , nodeSocket :: SocketPath
      , hydraClients :: [Host]
      , pumbaCommand :: Maybe String
      }
  | MatrixOptions
      { outputDirectory :: Maybe FilePath
      , timeoutSeconds :: NominalDiffTime
      , numberOfTxs :: Int
      , startingNodeId :: Int
      , clusterSizes :: [Word64]
      , utxoShapes :: [UTxOSize]
      , incrementalModes :: [Bool]
      , waitForTxValidModes :: [Bool]
      }

benchOptionsParser :: ParserInfo Options
benchOptionsParser =
  info
    ( hsubparser
        ( command "single" standaloneOptionsInfo
            <> command "datasets" datasetOptionsInfo
            <> command "demo" demoOptionsInfo
            <> command "matrix" matrixOptionsInfo
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc
          "Starts a cluster of Hydra nodes interconnected through a network and \
          \talking to a local cardano devnet, possibly generates an initial UTxO set and a bunch \
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
    (progDesc "Runs a scenario reusing a previous dataset/s from some directory.")

standaloneOptionsParser :: Parser Options
standaloneOptionsParser =
  StandaloneOptions
    <$> many filepathParser
    <*> optional outputDirectoryParser
    <*> timeoutParser
    <*> startingNodeIdParser
    <*> incrementalOpsParser
    <*> waitForTxValidParser

outputDirectoryParser :: Parser FilePath
outputDirectoryParser =
  strOption
    ( long "output-directory"
        <> metavar "DIR"
        <> help
          "The directory where to output markdown-formatted benchmark results. \
          \ If not set, raw text summary will be printed to the console. (default: none)"
    )

numberOfTxsParser :: Parser Int
numberOfTxsParser =
  option
    auto
    ( long "number-of-txs"
        <> value 100
        <> metavar "INT"
        <> help "Number of txs to generate (default: 100)"
    )

timeoutParser :: Parser NominalDiffTime
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

utxoSizeParser :: Parser UTxOSize
utxoSizeParser =
  option
    auto
    ( long "utxo-size"
        <> value Constant
        <> metavar "UTxOSize"
        <> help
          "Generated UTxO shape. 'Constant' keeps the head UTxO at a fixed size by \
          \ submitting self-transfers. 'Growing' adds one extra output per tx so the \
          \ head UTxO grows over the run. 'Mixed' grows for the first half of the run \
          \ then contracts via 2-in 1-out merges for the second half."
    )

demoOptionsInfo :: ParserInfo Options
demoOptionsInfo =
  info
    demoOptionsParser
    ( progDesc
        "Run bench scenario over local demo. \
        \ This requires having in the background: \
        \   * cardano node running on specified node-socket. \
        \   * hydra nodes listening on specified hosts."
    )

demoOptionsParser :: Parser Options
demoOptionsParser =
  DemoOptions
    <$> optional outputDirectoryParser
    <*> numberOfTxsParser
    <*> timeoutParser
    <*> networkIdParser
    <*> nodeSocketParser
    <*> many hydraClientsParser
    <*> optional pumbaCommandParser

pumbaCommandParser :: Parser String
pumbaCommandParser =
  strOption
    ( long "pumba-command"
        <> metavar "CMD"
        <> help
          "Shell command to run as a network fault injector (e.g. pumba) once the \
          \Head is open and deposits are finalized. The process is started just before \
          \transaction submission begins and is terminated before the Head is closed."
    )

hydraClientsParser :: Parser Host
hydraClientsParser =
  option (maybeReader readHost) $
    long "hydra-client"
      <> help
        ( "A hydra node api address to connect to. This is using the form <host>:<port>, \
          \where <host> can be an IP address, or a host name. Can be \
          \provided multiple times, once for each participant (current maximum limit is "
            <> show maximumNumberOfParties
            <> " )."
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
    <$> optional outputDirectoryParser
    <*> timeoutParser
    <*> utxoSizeParser
    <*> numberOfTxsParser
    <*> clusterSizeParser
    <*> startingNodeIdParser
    <*> incrementalOpsParser
    <*> waitForTxValidParser

incrementalOpsParser :: Parser Bool
incrementalOpsParser =
  flag
    False
    True
    ( long "incremental-ops"
        <> help
          "If set, the bench will also exercise one incremental commit and \
          \ one incremental decommit per client during the main transaction \
          \ submission window, and report their finalisation times. Off by \
          \ default."
    )

waitForTxValidParser :: Parser Bool
waitForTxValidParser =
  flag
    False
    True
    ( long "wait-for-tx-valid"
        <> help
          "If set, the submitter waits for each tx's confirmation before \
          \ posting the next one (one in-flight tx per client). If unset, \
          \ txs are fired as fast as the queue drains so the head's \
          \ saturation throughput is exercised. Off by default."
    )

filepathParser :: Parser FilePath
filepathParser =
  argument
    str
    ( metavar "FILE"
        <> help "Path to a JSON-formatted dataset descriptor file."
    )

matrixOptionsInfo :: ParserInfo Options
matrixOptionsInfo =
  info
    matrixOptionsParser
    ( progDesc
        "Run a scenario matrix over cluster sizes, UTxO shapes, and \
        \ incremental-ops modes. Writes a single 'scenarios.md' to the output \
        \ directory with a leading comparison table plus one section per cell."
    )

matrixOptionsParser :: Parser Options
matrixOptionsParser =
  MatrixOptions
    <$> optional outputDirectoryParser
    <*> timeoutParser
    <*> numberOfTxsParser
    <*> startingNodeIdParser
    <*> clusterSizesParser
    <*> utxoShapesParser
    <*> incrementalModesParser
    <*> waitForTxValidModesParser

clusterSizesParser :: Parser [Word64]
clusterSizesParser =
  option
    (maybeReader parseListOf)
    ( long "cluster-sizes"
        <> value [1, 2, 3]
        <> metavar "LIST"
        <> help "Comma-separated cluster sizes to iterate (default: 1,2,3)"
    )

utxoShapesParser :: Parser [UTxOSize]
utxoShapesParser =
  option
    (maybeReader parseListOf)
    ( long "shapes"
        <> value [Constant, Growing, Mixed]
        <> metavar "LIST"
        <> help "Comma-separated UTxO shapes to iterate. 'Constant' uses self-transfers so the head UTxO stays flat; 'Growing' adds outputs over time; 'Mixed' grows then contracts. (default: Constant,Growing,Mixed)"
    )

incrementalModesParser :: Parser [Bool]
incrementalModesParser =
  option
    (maybeReader parseListOf)
    ( long "incremental-modes"
        <> value [False, True]
        <> metavar "LIST"
        <> help "Comma-separated incremental-ops modes, e.g. False,True (default: False,True)"
    )

waitForTxValidModesParser :: Parser [Bool]
waitForTxValidModesParser =
  option
    (maybeReader parseListOf)
    ( long "wait-modes"
        <> value [False, True]
        <> metavar "LIST"
        <> help
          "Comma-separated wait-for-tx-valid modes. True keeps one in-flight \
          \ tx per client (round-trip-bound throughput); False fires all txs \
          \ as fast as possible (saturation throughput). Default: False,True"
    )

parseListOf :: Read a => String -> Maybe [a]
parseListOf s = traverse (readMaybe . toString) (splitOn "," (toText s))
