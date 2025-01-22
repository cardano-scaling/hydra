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
  helper,
  info,
  progDesc,
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

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
