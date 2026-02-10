{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.ChainObserver.Options where

import "hydra-prelude" Hydra.Prelude
import "base" Data.Version (showVersion)
import "hydra-cardano-api" Hydra.Cardano.Api (ChainPoint, NetworkId, SocketPath)
import "hydra-node" Hydra.NetworkVersions (hydraNodeVersion)
import "hydra-node" Hydra.Options (
  networkIdParser,
  nodeSocketParser,
  startChainFromParser,
 )
import "network-uri" Network.URI (URI, parseURI)
import "optparse-applicative" Options.Applicative (
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  infoOption,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
  str,
  value,
 )

data Options = Options
  { backend :: Backend
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  , explorerBaseURI :: Maybe URI
  -- ^ Whether to report observations to a hydra-explorer.
  }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> backendParser
    <*> optional startChainFromParser
    <*> optional explorerParser

data Backend
  = Direct
      { networkId :: NetworkId
      , nodeSocket :: SocketPath
      }
  | Blockfrost
      { projectPath :: FilePath
      }
  deriving stock (Show, Eq)

backendParser :: Parser Backend
backendParser =
  directParser <|> blockfrostParser
 where
  directParser =
    Direct <$> networkIdParser <*> nodeSocketParser

  blockfrostParser =
    Blockfrost <$> projectPathParser

projectPathParser :: Parser FilePath
projectPathParser =
  option str $
    long "blockfrost-project-path"
      <> metavar "BLOCKFROST_TOKEN_PATH"
      <> value "project_token_hash"
      <> help
        "The path where the Blockfrost project token hash is stored.\
        \It expects token prefixed with Blockfrost environment name\
        \e.g.: testnet-someTokenHash"

explorerParser :: Parser URI
explorerParser =
  option (maybeReader parseURI) $
    long "explorer"
      <> metavar "URI"
      <> help "Observer API endpoint of a hydra-explorer instance to report observations to, e.g. http://localhost:8080/api/"

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    (optionsParser <**> versionInfo <**> helper)
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
 where
  versionInfo :: Parser (a -> a)
  versionInfo =
    infoOption
      (showVersion hydraNodeVersion)
      (long "version" <> help "Show version")
