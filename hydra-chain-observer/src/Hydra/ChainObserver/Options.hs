{-# LANGUAGE DuplicateRecordFields #-}

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
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
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
  }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> backendParser
    <*> optional startChainFromParser

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

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
