module Bench.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId, SocketPath)
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Network (Host, readHost)
import Hydra.Options (networkIdParser, nodeSocketParser)
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
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

data Options
  = StandaloneOptions
      { scalingFactor :: Int
      , clusterSize :: Word64
      , outputDirectory :: Maybe FilePath
      , timeoutSeconds :: NominalDiffTime
      , startingNodeId :: Int
      }
  | DatasetOptions
      { datasetFiles :: [FilePath]
      , outputDirectory :: Maybe FilePath
      , timeoutSeconds :: NominalDiffTime
      , startingNodeId :: Int
      }
  | DemoOptions
      { outputDirectory :: Maybe FilePath
      , scalingFactor :: Int
      , timeoutSeconds :: NominalDiffTime
      , networkId :: NetworkId
      , nodeSocket :: SocketPath
      , hydraClients :: [Host]
      }

benchOptionsParser :: ParserInfo Options
benchOptionsParser =
  info
    ( hsubparser
        ( command "single" standaloneOptionsInfo
            <> command "datasets" datasetOptionsInfo
            <> command "demo" demoOptionsInfo
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
    (progDesc "Runs a single scenario, generating or reusing a previous dataset from some directory.")

standaloneOptionsParser :: Parser Options
standaloneOptionsParser =
  StandaloneOptions
    <$> scalingFactorParser
    <*> clusterSizeParser
    <*> optional outputDirectoryParser
    <*> timeoutParser
    <*> startingNodeIdParser

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
    <*> scalingFactorParser
    <*> timeoutParser
    <*> networkIdParser
    <*> nodeSocketParser
    <*> many hydraClientsParser

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
    <$> many filepathParser
    <*> optional outputDirectoryParser
    <*> timeoutParser
    <*> startingNodeIdParser

filepathParser :: Parser FilePath
filepathParser =
  argument
    str
    ( metavar "FILE"
        <> help "Path to a JSON-formatted dataset descriptor file."
    )
