module Hydra.ChainObserver.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint, NetworkId, SocketPath)
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
  metavar,
  option,
  progDesc,
  str,
  value,
 )

type Options :: Type
data Options
  = Options
      { networkId :: NetworkId
      , nodeSocket :: SocketPath
      , startChainFrom :: Maybe ChainPoint
      -- ^ Point at which to start following the chain.
      }
  | BlockfrostOptions
      { projectPath :: FilePath
      , startFromBlockHash :: Maybe Text
      -- ^ Point at which to start following the blockfrost chain.
      }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> nodeSocketParser
    <*> optional startChainFromParser

blockfrostOptionsParser :: Parser Options
blockfrostOptionsParser =
  BlockfrostOptions
    <$> projectPathParser
    <*> optional startFromBlockHashParser

projectPathParser :: Parser FilePath
projectPathParser =
  option str $
    long "project-path"
      <> metavar "BLOCKFROST_TOKEN_PATH"
      <> value "project_token_hash"
      <> help
        "The path where the Blockfrost project token hash is stored.\
        \It expects token prefixed with Blockfrost environment name\
        \e.g.: testnet-someTokenHash"

startFromBlockHashParser :: Parser Text
startFromBlockHashParser =
  option str $
    long "start-from-block-hash"
      <> metavar "BLOCK_HASH"
      <> help
        "The hash of the block we want to start observing the chain from. Only \
        \used if the last known head state is older than given point. If not \
        \given and no known head state, the chain tip is used."

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

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    ( hsubparser
        ( command "direct" directOptionsInfo
            <> command "blockfrost" blockfrostOptionsInfo
        )
        <**> helper
    )
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
