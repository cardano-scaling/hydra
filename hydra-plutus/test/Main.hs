module Main where

import Hydra.Prelude

import Hydra.Data.ContestationPeriodSpec qualified
import Hydra.Plutus.GoldenSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-plutus"
    [ testSpec "Data.ContestationPeriod" Hydra.Data.ContestationPeriodSpec.spec
    , pure Hydra.Plutus.GoldenSpec.tests
    ]
