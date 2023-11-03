module Hydra.ChainObserver (
  main,
) where

import Hydra.Prelude

import Hydra.ChainObserver.Options (hydraChainObserverOptions)
import Options.Applicative (execParser)

main :: IO ()
main = do
  xs <- execParser hydraChainObserverOptions
  print xs
