module Hydra.TUI.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Network (Host (Host))
import Hydra.TUI.Options (
  parseCardanoNetworkId,
  parseCardanoNodeSocket,
  parseCardanoSigningKey,
  parseNodeHost,
 )
import Options.Applicative (
  Parser,
  ParserResult (Success),
  defaultPrefs,
  execParserPure,
  info,
 )

spec :: Spec
spec = parallel $ do
  it "parses --connect option" $ do
    shouldParseWith parseNodeHost ["--connect", "127.0.0.1:4002"] (Host "127.0.0.1" 4002)
  it "parses --network-id option" $ do
    shouldParseWith parseCardanoNetworkId ["--network-id", "123"] (Testnet $ NetworkMagic 123)
  it "parses --cardano-signing-key option" $ do
    shouldParseWith parseCardanoSigningKey ["--cardano-signing-key", "foo.sk"] "foo.sk"
  it "parses --node-socket option" $ do
    shouldParseWith parseCardanoNodeSocket ["--node-socket", "something.socket"] "something.socket"

shouldParseWith :: (Show a, Eq a) => Parser a -> [String] -> a -> Expectation
shouldParseWith parser args result =
  case execParserPure defaultPrefs parserInfo args of
    Success a -> a `shouldBe` result
    err -> failure $ "Parser failed with " <> show err <> " on " <> show args
 where
  parserInfo = info parser mempty
