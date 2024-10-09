module Main where

import Hydra.ChainObserver qualified
import Hydra.ChainObserver.NodeClient (defaultObserverHandler)
import Hydra.Prelude

main :: IO ()
main = Hydra.ChainObserver.main defaultObserverHandler
