module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId (..), unsafeDeserialiseFromRawBytesBase16)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.Network (Host (Host))
import Hydra.Options (
  ChainConfig (..),
  LedgerConfig (..),
  Options (..),
  ParserResult (..),
  defaultChainConfig,
  defaultLedgerConfig,
  defaultOptions,
  parseHydraOptionsFromString,
  toArgs,
 )
import Test.QuickCheck (Property, counterexample, forAll, property, (===))

spec :: Spec
spec = parallel $
  describe "Hydra Node Options" $ do
    it "has defaults" $
      [] `shouldParse` defaultOptions

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

    it "parses --hydra-signing-key option as a filepath" $
      ["--hydra-signing-key", "./alice.sk"] `shouldParse` defaultOptions{hydraSigningKey = "./alice.sk"}

    it "parses --network-id option as a number" $ do
      shouldNotParse ["--network-id", "abc"]
      ["--network-id", "0"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkId = Testnet (NetworkMagic 0)
                }
          }
      ["--network-id", "-1"] -- Word32 overflow expected
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkId = Testnet (NetworkMagic 4294967295)
                }
          }
      ["--network-id", "123"]
        `shouldParse` defaultOptions
          { chainConfig =
              defaultChainConfig
                { networkId = Testnet (NetworkMagic 123)
                }
          }

    it "parses --mainnet flag" $ do
      shouldNotParse ["--mainnet"]

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

    it "parses --ledger-genesis-file as a filepath" $
      ["--ledger-genesis", "my-custom-genesis.json"]
        `shouldParse` defaultOptions
          { ledgerConfig =
              defaultLedgerConfig
                { cardanoLedgerGenesisFile = "my-custom-genesis.json"
                }
          }

    it "parses --ledger-protocol-parameters-file as a filepath" $
      ["--ledger-protocol-parameters", "my-custom-protocol-parameters.json"]
        `shouldParse` defaultOptions
          { ledgerConfig =
              defaultLedgerConfig
                { cardanoLedgerProtocolParametersFile = "my-custom-protocol-parameters.json"
                }
          }

    it "parses --start-chain-from as a pair of slot number and block header hash" $
      ["--start-chain-from", "1000.0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"]
        `shouldParse` defaultOptions
          { startChainFrom =
              Just (ChainPoint 1000 (unsafeDeserialiseFromRawBytesBase16 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"))
          }

    prop "roundtrip options" $
      forAll arbitrary canRoundtripOptionsAndPrettyPrinting

canRoundtripOptionsAndPrettyPrinting :: Options -> Property
canRoundtripOptionsAndPrettyPrinting opts =
  let args = toArgs opts
   in counterexample ("args:  " <> show args) $
        case parseHydraOptionsFromString args of
          Success opts' -> opts' === opts
          err -> property False & counterexample ("error : " <> show err)

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
