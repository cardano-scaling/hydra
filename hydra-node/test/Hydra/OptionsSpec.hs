module Hydra.OptionsSpec where

import Hydra.Chain.Direct (NetworkMagic (NetworkMagic))
import Hydra.Logging (Verbosity (Verbose))
import Hydra.Options
  ( ChainConfig (..),
    Options (..),
    ParserResult (..),
    parseHydraOptionsFromString,
  )
import Hydra.Prelude
import Test.Hydra.Prelude

spec :: Spec
spec = parallel $
  describe "Hydra Node Options" $ do
    it "has defaults" $
      [] `shouldParse` defaultOptions

    it "parses --host option given valid IPv4 and IPv6 addresses" $ do
      ["--host", "127.0.0.1"]
        `shouldParse` defaultOptions {host = "127.0.0.1"}
      ["--host", "2001:db8:11e:c00::101"]
        `shouldParse` defaultOptions {host = "2001:db8:11e:c00::101"}
      ["--host", "0.0.0.0"]
        `shouldParse` defaultOptions {host = "0.0.0.0"}
      shouldNotParse ["--host", "0.0.0"]
      shouldNotParse ["--host", "2001:db8:11e:c00:101"]

    it "parses --port option given valid port number" $ do
      ["--port", "12345"]
        `shouldParse` defaultOptions {port = 12345}
      shouldNotParse ["--port", "123456"]
      ["--port", "0"]
        `shouldParse` defaultOptions {port = 0}
      shouldNotParse ["--port", "-42"]

    it "parses --monitoring-port option given valid port number" $ do
      []
        `shouldParse` defaultOptions {monitoringPort = Nothing}
      ["--monitoring-port", "12345"]
        `shouldParse` defaultOptions {monitoringPort = Just 12345}

    it "parses --version flag as a parse error" $
      shouldNotParse ["--version"]

    it "parses --hydra-verification-key option as a filepath" $ do
      ["--hydra-verification-key", "./alice.vk"] `shouldParse` defaultOptions {hydraVerificationKeys = ["./alice.vk"]}
      ["--hydra-verification-key", "/foo"] `shouldParse` defaultOptions {hydraVerificationKeys = ["/foo"]}
      ["--hydra-verification-key", "bar"] `shouldParse` defaultOptions {hydraVerificationKeys = ["bar"]}
      ["--hydra-verification-key", "alice.vk", "--hydra-verification-key", "bob.vk"]
        `shouldParse` defaultOptions {hydraVerificationKeys = ["alice.vk", "bob.vk"]}

    it "parses --hydra-signing-key option as a filepath" $
      ["--hydra-signing-key", "./alice.sk"] `shouldParse` defaultOptions {hydraSigningKey = "./alice.sk"}

    it "parses --network-magic option as a number" $ do
      shouldNotParse ["--network-magic", "abc"]
      ["--network-magic", "0"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkMagic = NetworkMagic 0
                }
          }
      ["--network-magic", "-1"] -- Word32 overflow expected
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkMagic = NetworkMagic 4294967295
                }
          }
      ["--network-magic", "123"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkMagic = NetworkMagic 123
                }
          }

    it "parses --node-socket as a filepath" $
      ["--node-socket", "foo.sock"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { nodeSocket = "foo.sock"
                }
          }

    it "parses --cardano-signing-key option as a filepath" $
      ["--cardano-signing-key", "./alice-cardano.sk"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { cardanoSigningKey = "./alice-cardano.sk"
                }
          }

    it "parses --cardano-verification-key option as a filepath" $
      ["--cardano-verification-key", "./alice-cardano.vk"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { cardanoVerificationKeys = ["./alice-cardano.vk"]
                }
          }

defaultOptions :: Options
defaultOptions =
  Options
    { verbosity = Verbose "HydraNode",
      nodeId = 1,
      host = "127.0.0.1",
      port = 5001,
      apiHost = "127.0.0.1",
      apiPort = 4001,
      monitoringPort = Nothing,
      hydraSigningKey = "hydra.sk",
      hydraVerificationKeys = [],
      chainConfig = defaultChainConfig
    }

defaultChainConfig :: ChainConfig
defaultChainConfig =
  DirectChainConfig
    { networkMagic = NetworkMagic 42,
      nodeSocket = "node.socket",
      cardanoSigningKey = "cardano.sk",
      cardanoVerificationKeys = []
    }

shouldParse :: [String] -> Options -> Expectation
shouldParse args options =
  case parseHydraOptionsFromString args of
    Success a -> a `shouldBe` options
    err -> failure (show err)

shouldNotParse :: [String] -> Expectation
shouldNotParse args =
  case parseHydraOptionsFromString args of
    Success a -> failure $ "Unexpected successful parse to " <> show a
    Failure _ -> pure ()
    CompletionInvoked _ -> failure "Unexpected completion invocation"
