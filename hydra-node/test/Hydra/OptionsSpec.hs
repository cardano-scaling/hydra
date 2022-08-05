module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId (..), serialiseToRawBytesHexText, unsafeDeserialiseFromRawBytesBase16)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host (Host))
import Hydra.Options (
  ChainConfig (..),
  Command (..),
  LedgerConfig (..),
  Options (..),
  ParserResult (..),
  PublishOptions (..),
  defaultChainConfig,
  defaultLedgerConfig,
  parseHydraCommandFromArgs,
  toArgs,
 )
import Test.QuickCheck (Property, counterexample, forAll, property, (===))

spec :: Spec
spec = parallel $
  describe "Hydra Node Options" $ do
    it "has defaults" $
      [] `shouldParse` Run defaultOptions

    it "parses --host option given valid IPv4 and IPv6 addresses" $ do
      ["--host", "127.0.0.1"]
        `shouldParse` Run defaultOptions{host = "127.0.0.1"}
      ["--host", "2001:db8:11e:c00::101"]
        `shouldParse` Run defaultOptions{host = "2001:db8:11e:c00::101"}
      ["--host", "0.0.0.0"]
        `shouldParse` Run defaultOptions{host = "0.0.0.0"}
      shouldNotParse ["--host", "0.0.0"]
      shouldNotParse ["--host", "2001:db8:11e:c00:101"]

    it "parses --port option given valid port number" $ do
      ["--port", "12345"]
        `shouldParse` Run defaultOptions{port = 12345}
      shouldNotParse ["--port", "123456"]
      ["--port", "0"]
        `shouldParse` Run defaultOptions{port = 0}
      shouldNotParse ["--port", "-42"]

    -- TODO(SN): Move thes examples rather into a 'instance Read Host' test and
    -- only check for correct format / wiring here using a single test case This
    -- became evident when realizing that the 'hydra-tui' is also relying on this
    -- Read instance for parsing, but in a different command line flag.
    it "parses --peer <host>:<port> option" $ do
      ["--peer", "1.2.3.4:4567"]
        `shouldParse` Run defaultOptions{peers = [Host "1.2.3.4" 4567]}
      ["--peer", "1.2.3.4:4567", "--peer", "1.2.3.5:4568"]
        `shouldParse` Run defaultOptions{peers = [Host "1.2.3.4" 4567, Host "1.2.3.5" 4568]}
      ["--peer", "foo.com:4567"]
        `shouldParse` Run defaultOptions{peers = [Host "foo.com" 4567]}
      shouldNotParse ["--peer", "foo.com:456789"]
    it "does parse --peer given ipv6 addresses" $ do
      pendingWith "we do not support it"
      ["--peer", ":::1:4567"]
        `shouldParse` Run defaultOptions{peers = [Host ":::1" 4567]}

    it "parses --monitoring-port option given valid port number" $ do
      []
        `shouldParse` Run defaultOptions{monitoringPort = Nothing}
      ["--monitoring-port", "12345"]
        `shouldParse` Run defaultOptions{monitoringPort = Just 12345}
      ["--monitoring-port", "65535"]
        `shouldParse` Run defaultOptions{monitoringPort = Just 65535}

    it "parses --version flag as a parse error" $
      shouldNotParse ["--version"]

    it "parses --hydra-verification-key option as a filepath" $ do
      ["--hydra-verification-key", "./alice.vk"] `shouldParse` Run defaultOptions{hydraVerificationKeys = ["./alice.vk"]}
      ["--hydra-verification-key", "/foo"] `shouldParse` Run defaultOptions{hydraVerificationKeys = ["/foo"]}
      ["--hydra-verification-key", "bar"] `shouldParse` Run defaultOptions{hydraVerificationKeys = ["bar"]}
      ["--hydra-verification-key", "alice.vk", "--hydra-verification-key", "bob.vk"]
        `shouldParse` Run defaultOptions{hydraVerificationKeys = ["alice.vk", "bob.vk"]}

    it "parses --hydra-signing-key option as a filepath" $
      ["--hydra-signing-key", "./alice.sk"] `shouldParse` Run defaultOptions{hydraSigningKey = "./alice.sk"}

    it "parses --network-id option as a number" $ do
      shouldNotParse ["--network-id", "abc"]
      ["--network-id", "0"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 0)
                  }
            }
      ["--network-id", "-1"] -- Word32 overflow expected
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 4294967295)
                  }
            }
      ["--network-id", "123"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 123)
                  }
            }

    it "parses --mainnet flag" $ do
      shouldNotParse ["--mainnet"]

    it "parses --node-socket as a filepath" $
      ["--node-socket", "foo.sock"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { nodeSocket = "foo.sock"
                  }
            }

    it "parses --cardano-signing-key option as a filepath" $
      ["--cardano-signing-key", "./alice-cardano.sk"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { cardanoSigningKey = "./alice-cardano.sk"
                  }
            }

    it "parses --cardano-verification-key option as a filepath" $
      ["--cardano-verification-key", "./alice-cardano.vk"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { cardanoVerificationKeys = ["./alice-cardano.vk"]
                  }
            }

    it "parses --ledger-genesis-file as a filepath" $
      ["--ledger-genesis", "my-custom-genesis.json"]
        `shouldParse` Run
          defaultOptions
            { ledgerConfig =
                defaultLedgerConfig
                  { cardanoLedgerGenesisFile = "my-custom-genesis.json"
                  }
            }

    it "parses --ledger-protocol-parameters-file as a filepath" $
      ["--ledger-protocol-parameters", "my-custom-protocol-parameters.json"]
        `shouldParse` Run
          defaultOptions
            { ledgerConfig =
                defaultLedgerConfig
                  { cardanoLedgerProtocolParametersFile = "my-custom-protocol-parameters.json"
                  }
            }

    it "parses --start-chain-from as a pair of slot number and block header hash" $
      ["--start-chain-from", "1000.0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"]
        `shouldParse` Run
          defaultOptions
            { chainConfig =
                defaultChainConfig
                  { startChainFrom =
                      Just
                        ( ChainPoint
                            1000
                            (unsafeDeserialiseFromRawBytesBase16 "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
                        )
                  }
            }

    prop "parses --hydra-scripts-tx-id as a tx id" $ \txId ->
      ["--hydra-scripts-tx-id", toString $ serialiseToRawBytesHexText txId]
        `shouldParse` Run
          defaultOptions
            { hydraScriptsTxId = txId
            }

    prop "roundtrip options" $
      forAll arbitrary canRoundtripOptionsAndPrettyPrinting

    describe "publish-scripts sub-command" $ do
      xit "does not parse without any options" $
        shouldNotParse
          [ "publish-scripts"
          ]

      xit "does not parse with some missing option (1)" $
        shouldNotParse $
          mconcat
            [ ["publish-scripts"]
            , ["--node-socket", "foo"]
            , ["--network-id", "42"]
            ]

      xit "does not parse with some missing option (2)" $
        shouldNotParse $
          mconcat
            [ ["publish-scripts"]
            , ["--network-id", "42"]
            , ["--cardano-signing-key", "foo"]
            ]

      xit "does not parse with some missing option (3)" $
        shouldNotParse $
          mconcat
            [ ["publish-scripts"]
            , ["--node-socket", "foo"]
            , ["--cardano-signing-key", "foo"]
            ]

      it "should parse with all options" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "foo"]
          , ["--network-id", "42"]
          , ["--cardano-signing-key", "bar"]
          ]
          `shouldParse` ( Publish
                            PublishOptions
                              { publishNodeSocket = "foo"
                              , publishNetworkId = Testnet (NetworkMagic 42)
                              , publishSigningKey = "bar"
                              }
                        )

canRoundtripOptionsAndPrettyPrinting :: Options -> Property
canRoundtripOptionsAndPrettyPrinting opts =
  let args = toArgs opts
   in counterexample ("args:  " <> show args) $
        case parseHydraCommandFromArgs args of
          Success cmd -> cmd === Run opts
          err -> property False & counterexample ("error : " <> show err)

shouldParse :: [String] -> Command -> Expectation
shouldParse args cmd =
  case parseHydraCommandFromArgs args of
    Success a -> a `shouldBe` cmd
    err -> failure (show err)

shouldNotParse :: [String] -> Expectation
shouldNotParse args =
  case parseHydraCommandFromArgs args of
    Success a -> failure $ "Unexpected successful parse to " <> show a
    Failure _ -> pure ()
    CompletionInvoked _ -> failure "Unexpected completion invocation"

-- Default options as they should also be provided by the option parser.
defaultOptions :: Options
defaultOptions =
  Options
    { verbosity = Verbose "HydraNode"
    , nodeId = 1
    , host = "127.0.0.1"
    , port = 5001
    , peers = []
    , apiHost = "127.0.0.1"
    , apiPort = 4001
    , monitoringPort = Nothing
    , hydraSigningKey = "hydra.sk"
    , hydraVerificationKeys = []
    , hydraScriptsTxId = "0101010101010101010101010101010101010101010101010101010101010101"
    , chainConfig = defaultChainConfig
    , ledgerConfig = defaultLedgerConfig
    }
