module Main where

import Hydra.Prelude

import Hydra.ChainObserver qualified
import Hydra.ChainObserver.NodeClient (defaultObserverHandler)
import Hydra.Tx.ScriptRegistry (serialisedScriptRegistry)

main :: IO ()
main = Hydra.ChainObserver.main serialisedScriptRegistry defaultObserverHandler
