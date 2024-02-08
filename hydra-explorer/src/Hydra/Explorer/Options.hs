module Hydra.Explorer.Options where

import Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId, SocketPath)
import Hydra.Network (Host)
import Hydra.Options (
  networkIdParser,
  nodeSocketParser,
  peerParser,
  startChainFromParser,
 )
import Options.Applicative (Parser, ParserInfo, fullDesc, header, helper, info, progDesc)

type Options :: Type
data Options = Options
  { networkId :: NetworkId
  , host :: Host
  , nodeSocket :: SocketPath
  , startChainFrom :: Maybe ChainPoint
  }
  deriving stock (Show, Eq)

optionsParser :: Parser Options
optionsParser =
  Options
    <$> networkIdParser
    <*> peerParser
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
