module Main where

import "hydra-prelude" Hydra.Prelude

import "hydra-tui" Hydra.TUI qualified as TUI
import "hydra-tui" Hydra.TUI.Options (parseOptions)
import "optparse-applicative" Options.Applicative (execParser, info)

main :: IO ()
main =
  execParser (info parseOptions mempty) >>= void . TUI.run
