module Hydra.Explorer.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId, SlotNo (..), SocketPath, serialiseToRawBytesHexText)
import Hydra.ChainObserver.Options (projectPathParser)
import Hydra.Network (PortNumber, readPort)
import Hydra.Options (
  networkIdParser,
  nodeSocketParser,
  startChainFromParser,
 )
import Options.Applicative (
  Parser,
  ParserInfo,
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
  showDefault,
  value,
 )

data DirectOptions = DirectOptions
  { networkId :: NetworkId
  , port :: PortNumber
  , nodeSocket :: SocketPath
  , startChainFrom :: Maybe ChainPoint
  }
  deriving stock (Show, Eq)

data BlockfrostOptions = BlockfrostOptions
  { port :: PortNumber
  , projectPath :: FilePath
  , startChainFrom :: Maybe ChainPoint
  }
  deriving stock (Show, Eq)

data Options = DirectOpts DirectOptions | BlockfrostOpts BlockfrostOptions
  deriving stock (Show, Eq)

apiPortParser :: Parser PortNumber
apiPortParser =
  option
    (maybeReader readPort)
    ( long "api-port"
        <> value 9090
        <> showDefault
        <> metavar "PORT"
        <> help "Listen port for incoming client API connections."
    )

directOptionsParser :: Parser Options
directOptionsParser =
  DirectOpts
    <$> ( DirectOptions
            <$> networkIdParser
            <*> apiPortParser
            <*> nodeSocketParser
            <*> optional startChainFromParser
        )

blockfrostOptionsParser :: Parser Options
blockfrostOptionsParser =
  BlockfrostOpts
    <$> ( BlockfrostOptions
            <$> apiPortParser
            <*> projectPathParser
            <*> optional startChainFromParser
        )

directOptionsInfo :: ParserInfo Options
directOptionsInfo =
  info
    directOptionsParser
    (progDesc "Direct Mode")

blockfrostOptionsInfo :: ParserInfo Options
blockfrostOptionsInfo =
  info
    blockfrostOptionsParser
    (progDesc "Blockfrost Mode")

hydraExplorerOptions :: ParserInfo Options
hydraExplorerOptions =
  info
    ( hsubparser
        ( command "direct" directOptionsInfo
            <> command "blockfrost" blockfrostOptionsInfo
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc "Explore hydra heads from chain."
        <> header "hydra-explorer"
    )

toArgStartChainFrom :: Maybe ChainPoint -> [String]
toArgStartChainFrom = \case
  Just ChainPointAtGenesis ->
    ["--start-chain-from", "0"]
  Just (ChainPoint (SlotNo slotNo) headerHash) ->
    let headerHashBase16 = toString (serialiseToRawBytesHexText headerHash)
     in ["--start-chain-from", show slotNo <> "." <> headerHashBase16]
  Nothing ->
    []

toArgProjectPath :: FilePath -> [String]
toArgProjectPath projectPath = ["--project-path", projectPath]
