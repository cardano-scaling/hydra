module Hydra.ChainObserver.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint, NetworkId, SocketPath)
import Hydra.Options (
  networkIdParser,
  nodeSocketParser,
  startChainFromParser,
 )
import Network.URI (URI, parseURI)
import Options.Applicative (
  Parser,
  ParserInfo,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  maybeReader,
  metavar,
  option,
  progDesc,
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

data Backend = Direct
  { networkId :: NetworkId
  , nodeSocket :: SocketPath
  }
  deriving stock (Show, Eq)

backendParser :: Parser Backend
backendParser =
  directParser
 where
  directParser =
    Direct <$> networkIdParser <*> nodeSocketParser

explorerParser :: Parser URI
explorerParser =
  option (maybeReader parseURI) $
    long "explorer"
      <> metavar "URI"
      <> help "Observer API endpoint of a hydra-explorer instance to report observations to, e.g. http://localhost:8080/api/"

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
