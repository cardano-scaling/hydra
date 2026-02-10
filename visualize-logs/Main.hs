-- | Parse hydra-node logs format more easy on the eyes. Parser works with regular json logs as well as journalctl format.
module Main where

import "hydra-prelude" Hydra.Prelude
import "optparse-applicative" Options.Applicative (execParser)

import VisualizeLogs

main :: IO ()
main = do
  args <- execParser opts
  visualize $ paths args
