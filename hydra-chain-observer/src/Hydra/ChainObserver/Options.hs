module Hydra.ChainObserver.Options where

import Hydra.Prelude

import Hydra.Network (IP, PortNumber)

import Hydra.Cardano.Api (ChainPoint, NetworkId, SocketPath)
import Hydra.Options (
  hostParser,
  networkIdParser,
  nodeSocketParser,
  portParser,
  startChainFromParser,
 )
import Options.Applicative (Parser, ParserInfo, fullDesc, header, helper, info, progDesc)

type Options :: Type
data Options = Options
  { networkId :: NetworkId
  , nodeSocket :: SocketPath
  , host :: IP
  , port :: PortNumber
  , startChainFrom :: Maybe ChainPoint
  -- ^ Point at which to start following the chain.
  }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> nodeSocketParser
    <*> hostParser
    <*> portParser
    <*> optional startChainFromParser

hydraChainObserverOptions :: ParserInfo Options
hydraChainObserverOptions =
  info
    ( optionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Observe hydra transactions on-chain."
        <> header "hydra-chain-observer"
    )
