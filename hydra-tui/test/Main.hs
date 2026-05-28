module Main where

import Hydra.Prelude

import Hydra.TUI.OptionsSpec qualified
import Hydra.TUISpec qualified
import Test.Hydra.TastyMain (defaultMainHydra, testSpec)

main :: IO ()
main =
  defaultMainHydra
    "hydra-tui"
    [ testSpec "TUI" Hydra.TUISpec.spec
    , testSpec "TUI.Options" Hydra.TUI.OptionsSpec.spec
    ]
