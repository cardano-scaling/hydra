module Hydra.TUI.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Network (Host (Host))
import Hydra.TUI.Options (
  Options (..),
  parseOptions,
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
    shouldParseWith parseOptions ["--connect", "127.0.0.2:4002"] defaultOptions{hydraNodeHost = Host "127.0.0.2" 4002}
  it "no arguments yield default options" $ do
    shouldParseWith parseOptions [] defaultOptions
  it "parses --testnet-magic option" $ do
    shouldParseWith parseOptions ["--testnet-magic", "123"] defaultOptions{cardanoNetworkId = Testnet $ NetworkMagic 123}
  it "parses --cardano-signing-key option" $ do
    shouldParseWith parseOptions ["--cardano-signing-key", "foo.sk"] defaultOptions{cardanoSigningKey = "foo.sk"}
  it "parses --node-socket option" $ do
    shouldParseWith parseOptions ["--node-socket", "something.socket"] defaultOptions{cardanoNodeSocket = "something.socket"}

defaultOptions :: Options
defaultOptions =
  Options
    { hydraNodeHost = Host "127.0.0.1" 4001
    , cardanoNetworkId = Testnet $ NetworkMagic 42
    , cardanoNodeSocket = "node.socket"
    , cardanoSigningKey = "me.sk"
    }

shouldParseWith :: (Show a, Eq a) => Parser a -> [String] -> a -> Expectation
shouldParseWith parser args result =
  case execParserPure defaultPrefs parserInfo args of
    Success a -> a `shouldBe` result
    err -> failure $ "Parser failed with " <> show err <> " on " <> show args
 where
  parserInfo = info parser mempty
