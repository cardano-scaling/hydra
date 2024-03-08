module Main where

import Hydra.Prelude

import Hydra.Explorer (run)
import Hydra.Explorer.Options (hydraExplorerOptions)
import Options.Applicative (execParser)

main :: IO ()
main =
  execParser hydraExplorerOptions >>= run
