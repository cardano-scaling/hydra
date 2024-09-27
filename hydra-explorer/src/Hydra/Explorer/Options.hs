module Hydra.Explorer.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId, SlotNo (..), SocketPath, serialiseToRawBytesHexText)
import Hydra.ChainObserver.Options (projectPathParser, startFromBlockHashParser)
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

type Options :: Type
data Options
  = Options
      { networkId :: NetworkId
      , port :: PortNumber
      , nodeSocket :: SocketPath
      , startChainFrom :: Maybe ChainPoint
      }
  | BlockfrostOptions
      { port :: PortNumber
      , projectPath :: FilePath
      , startFromBlockHash :: Maybe Text
      }
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

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> apiPortParser
    <*> nodeSocketParser
    <*> optional startChainFromParser

blockfrostOptionsParser :: Parser Options
blockfrostOptionsParser =
  BlockfrostOptions
    <$> apiPortParser
    <*> projectPathParser
    <*> optional startFromBlockHashParser

directOptionsInfo :: ParserInfo Options
directOptionsInfo =
  info
    optionsParser
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

toArgStartFromBlockHash :: Maybe Text -> [String]
toArgStartFromBlockHash = \case
  Just startFromBlockHash ->
    ["--start-from-block-hash", toString startFromBlockHash]
  Nothing ->
    []
