module Hydra.Explorer.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId, SocketPath)
import Hydra.Network (PortNumber)
import Hydra.Options (
  apiPortParser,
  networkIdParser,
  nodeSocketParser,
  startChainFromParser,
 )
import Options.Applicative (Parser, ParserInfo, fullDesc, header, helper, info, progDesc)

type Options :: Type
data Options = Options
  { networkId :: NetworkId
  , port :: PortNumber
  , nodeSocket :: SocketPath
  , startChainFrom :: Maybe ChainPoint
  }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> apiPortParser
    <*> nodeSocketParser
    <*> optional startChainFromParser

hydraExplorerOptions :: ParserInfo Options
hydraExplorerOptions =
  info
    ( optionsParser
        <**> helper
    )
    ( fullDesc
        <> progDesc "Explore hydra heads from chain."
        <> header "hydra-explorer"
    )
