module Main where

import "hydra-prelude" Hydra.Prelude
import "optparse-applicative" Options.Applicative (execParser, info)

import Hydra.TUI qualified as TUI
import Hydra.TUI.Options (parseOptions)

main :: IO ()
main =
  execParser (info parseOptions mempty) >>= void . TUI.run
