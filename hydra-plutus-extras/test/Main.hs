module Main where

import Hydra.Prelude

import Hydra.Plutus.Extras.TimeSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-plutus-extras"
    [ testSpec "Plutus.Extras.Time" Hydra.Plutus.Extras.TimeSpec.spec
    ]
