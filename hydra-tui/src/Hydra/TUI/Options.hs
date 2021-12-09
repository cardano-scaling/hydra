module Hydra.TUI.Options where

import Hydra.Prelude

import Hydra.Ledger.Cardano (NetworkId)
import Hydra.Network (Host (Host))
import Options.Applicative (Parser, auto, help, long, option, short, showDefault, value)

data Options = Options
  { hydraNodeHost :: Host
  , cardanoNodeSocket :: FilePath
  , cardanoNetworkId :: NetworkId
  , cardanoVerificationKey :: FilePath
  }

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parseNodeHost
    <*> error "parse node socket"
    <*> parseCardanoNetworkId
    <*> parseCardanoVerificationKey

parseNodeHost :: Parser Host
parseNodeHost =
  option
    auto
    ( long "connect"
        <> short 'c'
        <> help "Hydra-node to connect to in the form of <host>:<port>"
        <> value (Host "0.0.0.0" 4001)
        <> showDefault
    )

parseCardanoNetworkId :: Parser NetworkId
parseCardanoNetworkId = error "parseCardanoNetworkId"

parseCardanoVerificationKey :: Parser FilePath
parseCardanoVerificationKey = error "parseCardanoVerificationKey"
