module Main where

import Hydra.Prelude

import Hydra.TUI qualified as TUI
import Hydra.TUI.Options (parseOptions)
import Options.Applicative (execParser, info)

main :: IO ()
main =
  execParser (info parseOptions mempty) >>= void . TUI.run
