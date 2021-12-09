module Hydra.TUI.Options where

import Hydra.Prelude

import Hydra.Network (Host (Host))
import Options.Applicative (Parser, auto, help, long, option, short, showDefault, value)

data Options = Options
  { hydraNodeHost :: Host
  , cardanoNodeSocket :: FilePath
  }

parseOptions :: Parser Options
parseOptions =
  Options <$> parseNodeHost <*> pure ""

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
