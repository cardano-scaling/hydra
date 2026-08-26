module Main where

import Hydra.Agda.OffChainReferenceSpec qualified
import Hydra.Agda.ReferenceSpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-agda"
    [ testSpec "Agda.Reference" Hydra.Agda.ReferenceSpec.spec
    , testSpec "Agda.OffChainReference" Hydra.Agda.OffChainReferenceSpec.spec
    ]
