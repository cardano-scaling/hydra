module Hydra.TUI.OptionsSpec where

import           Hydra.Prelude
import           Test.Hydra.Prelude

import           Hydra.Ledger.Cardano (NetworkId (Mainnet, Testnet),
                                       NetworkMagic (NetworkMagic))
import           Hydra.Network        (Host (Host))
import           Hydra.TUI.Options    (parseCardanoNetworkId,
                                       parseCardanoNodeSocket,
                                       parseCardanoVerificationKey,
                                       parseNodeHost)
import           Options.Applicative  (Parser, ParserResult (Success),
                                       defaultPrefs, execParserPure, info)

spec :: Spec
spec = parallel $ do
  it "parses --connect option" $ do
    shouldParseWith parseNodeHost ["--connect", "127.0.0.1:4002"] (Host "127.0.0.1" 4002)
  it "parses --network-magic option" $ do
    shouldParseWith parseCardanoNetworkId ["--network-magic", "123"] (Testnet $ NetworkMagic 123)
  it "defaults --network-magic to Mainnet" $ do
    shouldParseWith parseCardanoNetworkId [] Mainnet
  it "parses --cardano-verification-key option" $ do
    shouldParseWith parseCardanoVerificationKey ["--cardano-verification-key", "foo.vk"] "foo.vk"
  it "parses --cardano-node-socket option" $ do
    shouldParseWith parseCardanoNodeSocket ["--cardano-node-socket", "something.socket"] "something.socket"

shouldParseWith :: (Show a, Eq a) => Parser a -> [String] -> a -> Expectation
shouldParseWith parser args result =
  case execParserPure defaultPrefs parserInfo args of
    Success a -> a `shouldBe` result
    err -> failure $ "Parser failed with " <> show err <> " on " <> show args
 where
  parserInfo = info parser mempty
