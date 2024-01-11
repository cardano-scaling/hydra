module Main where

import Hydra.ChainObserver (defaultObserverHandler)
import Hydra.ChainObserver qualified
import Hydra.Prelude

main :: IO ()
main = Hydra.ChainObserver.main defaultObserverHandler
