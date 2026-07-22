module Main where

import Hydra.Prelude

import Hydra.ChainObserverSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-chain-observer"
    [ testSpec "ChainObserver" Hydra.ChainObserverSpec.spec
    ]
