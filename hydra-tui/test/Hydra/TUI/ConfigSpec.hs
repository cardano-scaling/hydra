module Hydra.TUI.ConfigSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.TUI.Config (
  Theme (..),
  TuiConfig (..),
  defaultConfig,
  encodeConfig,
  parseConfig,
 )

spec :: Spec
spec = parallel $ do
  it "roundtrips DarkTheme through encode/parse" $
    parseConfig (encodeConfig (TuiConfig DarkTheme)) `shouldBe` TuiConfig DarkTheme
  it "roundtrips LightTheme through encode/parse" $
    parseConfig (encodeConfig (TuiConfig LightTheme)) `shouldBe` TuiConfig LightTheme
  it "falls back to defaultConfig on empty input" $
    parseConfig "" `shouldBe` defaultConfig
  it "falls back to defaultConfig on unrecognised theme value" $
    parseConfig "theme: banana\n" `shouldBe` defaultConfig
