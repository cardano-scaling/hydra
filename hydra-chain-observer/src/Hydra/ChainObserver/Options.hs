module Hydra.ChainObserver.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (NetworkId, SocketPath)
import Hydra.Options (networkIdParser, nodeSocketParser)
import Options.Applicative (Parser, ParserInfo, fullDesc, header, helper, info, progDesc)

type Options :: Type
data Options = Options
  { networkId :: NetworkId
  , nodeSocket :: SocketPath
  }
  deriving stock (Show, Eq)

-- TODO: --start-chain-from would be useful here as well

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> nodeSocketParser

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
