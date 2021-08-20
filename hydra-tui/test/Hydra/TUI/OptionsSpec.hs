module Hydra.TUI.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Network (Host (Host))
import Hydra.TUI.Options (parseNodeHost)
import Options.Applicative (Parser, ParserResult (Success), defaultPrefs, execParserPure, info)

spec :: Spec
spec = parallel $ do
  it "parses --connect option" $ do
    shouldParseWith parseNodeHost ["--connect", "127.0.0.1:4002"] (Host "127.0.0.1" 4002)

shouldParseWith :: (Show a, Eq a) => Parser a -> [String] -> a -> Expectation
shouldParseWith parser args result =
  case execParserPure defaultPrefs parserInfo args of
    Success a -> a `shouldBe` result
    err -> failure $ "Parser failed with " <> show err <> " on " <> show args
 where
  parserInfo = info parser mempty
