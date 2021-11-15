module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.Direct (NetworkMagic (NetworkMagic))
import Hydra.Network (Host (Host), MockChain (..), defaultMockChain)
import Hydra.Options (
  ChainConfig (..),
  Options (..),
  ParserResult (..),
  defaultOptions,
  parseHydraOptionsFromString,
 )

spec :: Spec
spec = parallel $
  describe "Hydra Node Options" $ do
    it "parses --host option given valid IPv4 and IPv6 addresses" $ do
      ["--host", "127.0.0.1"]
        `shouldParse` defaultOptions{host = "127.0.0.1"}
      ["--host", "2001:db8:11e:c00::101"]
        `shouldParse` defaultOptions{host = "2001:db8:11e:c00::101"}
      ["--host", "0.0.0.0"]
        `shouldParse` defaultOptions{host = "0.0.0.0"}
      shouldNotParse ["--host", "0.0.0"]
      shouldNotParse ["--host", "2001:db8:11e:c00:101"]

    it "parses --port option given valid port number" $ do
      ["--port", "12345"]
        `shouldParse` defaultOptions{port = 12345}
      shouldNotParse ["--port", "123456"]
      ["--port", "0"]
        `shouldParse` defaultOptions{port = 0}
      shouldNotParse ["--port", "-42"]

    -- TODO(SN): Move thes examples rather into a 'instance Read Host' test and
    -- only check for correct format / wiring here using a single test case This
    -- became evident when realizing that the 'hydra-tui' is also relying on this
    -- Read instance for parsing, but in a different command line flag.
    it "parses --peer <host>:<port> option" $ do
      ["--peer", "1.2.3.4:4567"]
        `shouldParse` defaultOptions{peers = [Host "1.2.3.4" 4567]}
      ["--peer", "1.2.3.4:4567", "--peer", "1.2.3.5:4568"]
        `shouldParse` defaultOptions{peers = [Host "1.2.3.4" 4567, Host "1.2.3.5" 4568]}
      ["--peer", "foo.com:4567"]
        `shouldParse` defaultOptions{peers = [Host "foo.com" 4567]}
      shouldNotParse ["--peer", "foo.com:456789"]
    it "does parse --peer given ipv6 addresses" $ do
      pendingWith "we do not support it"
      ["--peer", ":::1:4567"]
        `shouldParse` defaultOptions{peers = [Host ":::1" 4567]}

    it "parses --monitoring-port option given valid port number" $ do
      []
        `shouldParse` defaultOptions{monitoringPort = Nothing}
      ["--monitoring-port", "12345"]
        `shouldParse` defaultOptions{monitoringPort = Just 12345}

    it "parses --version flag as a parse error" $
      shouldNotParse ["--version"]

    it "parses --hydra-verification-key option as a filepath" $ do
      ["--hydra-verification-key", "./alice.vk"] `shouldParse` defaultOptions{hydraVerificationKeys = ["./alice.vk"]}
      ["--hydra-verification-key", "/foo"] `shouldParse` defaultOptions{hydraVerificationKeys = ["/foo"]}
      ["--hydra-verification-key", "bar"] `shouldParse` defaultOptions{hydraVerificationKeys = ["bar"]}
      ["--hydra-verification-key", "alice.vk", "--hydra-verification-key", "bob.vk"]
        `shouldParse` defaultOptions{hydraVerificationKeys = ["alice.vk", "bob.vk"]}

    it "parses --me option as a filepath" $
      ["--hydra-signing-key", "./alice.sk"] `shouldParse` defaultOptions{hydraSigningKey = "./alice.sk"}

    it "parses --mock-chain-ports option as a list of ports to connect to" $
      ["--mock-chain-ports", "(1,2,3)"]
        `shouldParse` defaultOptions
          { chainConfig =
              MockChainConfig
                defaultMockChain
                  { syncPort = 1
                  , catchUpPort = 2
                  , postTxPort = 3
                  }
          }

    it "parses --mock-chain-host option as the mock-chain host to connect to" $
      ["--mock-chain-host", "1.2.3.4"]
        `shouldParse` defaultOptions
          { chainConfig =
              MockChainConfig
                defaultMockChain
                  { mockChainHost = "1.2.3.4"
                  }
          }

    it "parses mandatory options for direct chain configuration" $
      ["--node-socket", "foo.sock", "--cardano-signing-key", "my.sk"]
        `shouldParse` defaultOptions
          { chainConfig =
              DirectChainConfig
                { networkMagic = NetworkMagic 42
                , nodeSocket = "foo.sock"
                , cardanoSigningKey = "my.sk"
                , cardanoVerificationKeys = []
                }
          }

    it "fails to parse options for direct chain when missing --cardano-signing-key" $
      shouldNotParse ["--node-socket", "foo.sock"]

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
