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
      , startChainFrom :: Maybe ChainPoint
      -- ^ Point at which to start following the chain.
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
    <*> optional startChainFromParser

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
