module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  ChainPoint (..),
  NetworkId (..),
  serialiseToRawBytesHexText,
 )
import Hydra.Chain (maximumNumberOfParties)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.Network (Host (Host))
import Hydra.Options (
  ChainConfig (..),
  Command (..),
  DirectChainConfig (..),
  GenerateKeyPair (GenerateKeyPair),
  InvalidOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  ParserResult (..),
  PublishOptions (..),
  RunOptions (..),
  defaultDirectChainConfig,
  defaultLedgerConfig,
  defaultOfflineChainConfig,
  defaultRunOptions,
  outputFile,
  parseHydraCommandFromArgs,
  renderFailure,
  toArgs,
  validateRunOptions,
 )
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, chooseEnum, counterexample, forAll, property, vectorOf, (===))
import Text.Regex.TDFA ((=~))

spec :: Spec
spec = parallel $
  describe "Hydra Node RunOptions" $ do
    let genKeyString = vectorOf 10 $ chooseEnum ('a', 'z')
        genCardanoAndHydraKeys f1 f2 = flip generateWith 42 $ do
          cks <- replicateM (f1 maximumNumberOfParties) genKeyString
          hks <- replicateM (f2 maximumNumberOfParties) genKeyString
          pure (cks, hks)

    it ("validateRunOptions: using more than " <> show maximumNumberOfParties <> " parties should error out") $ do
      let (cardanoKeys, hydraKeys) = genCardanoAndHydraKeys (+ 2) (+ 1)
          chainCfg = Direct defaultDirectChainConfig{cardanoVerificationKeys = cardanoKeys}
      validateRunOptions (defaultRunOptions{hydraVerificationKeys = hydraKeys, chainConfig = chainCfg})
        `shouldBe` Left MaximumNumberOfPartiesExceeded
    it "validateRunOptions: loaded cardano keys needs to match with the hydra keys length" $ do
      let (cardanoKeys, hydraKeys) = genCardanoAndHydraKeys (subtract 2) (subtract 1)
          chainCfg = Direct defaultDirectChainConfig{cardanoVerificationKeys = cardanoKeys}
      validateRunOptions (defaultRunOptions{hydraVerificationKeys = hydraKeys, chainConfig = chainCfg})
        `shouldBe` Left CardanoAndHydraKeysMissmatch

    it "parses with default values" $
      [] `shouldParse` Run defaultRunOptions

    it "parses --host option given valid IPv4 and IPv6 addresses" $ do
      ["--host", "127.0.0.1"]
        `shouldParse` Run defaultRunOptions{host = "127.0.0.1"}
      ["--host", "2001:db8:11e:c00::101"]
        `shouldParse` Run defaultRunOptions{host = "2001:db8:11e:c00::101"}
      ["--host", "0.0.0.0"]
        `shouldParse` Run defaultRunOptions{host = "0.0.0.0"}
      shouldNotParse ["--host", "0.0.0"]
      shouldNotParse ["--host", "2001:db8:11e:c00:101"]

    it "parses --port option given valid port number" $ do
      ["--port", "12345"]
        `shouldParse` Run defaultRunOptions{port = 12345}
      shouldNotParse ["--port", "123456"]
      ["--port", "0"]
        `shouldParse` Run defaultRunOptions{port = 0}
      shouldNotParse ["--port", "-42"]

    -- TODO(SN): Move these examples rather into a 'instance Read Host' test and
    -- only check for correct format / wiring here using a single test case This
    -- became evident when realizing that the 'hydra-tui' is also relying on this
    -- Read instance for parsing, but in a different command line flag.
    it "parses --peer `<host>:<port>` option" $ do
      ["--peer", "1.2.3.4:4567"]
        `shouldParse` Run defaultRunOptions{peers = [Host "1.2.3.4" 4567]}
      ["--peer", "1.2.3.4:4567", "--peer", "1.2.3.5:4568"]
        `shouldParse` Run defaultRunOptions{peers = [Host "1.2.3.4" 4567, Host "1.2.3.5" 4568]}
      ["--peer", "foo.com:4567"]
        `shouldParse` Run defaultRunOptions{peers = [Host "foo.com" 4567]}
      shouldNotParse ["--peer", "foo.com:456789"]
    it "does parse --peer given ipv6 addresses" $ do
      pendingWith "we do not support it"
      ["--peer", ":::1:4567"]
        `shouldParse` Run defaultRunOptions{peers = [Host ":::1" 4567]}

    it "parses --monitoring-port option given valid port number" $ do
      []
        `shouldParse` Run defaultRunOptions{monitoringPort = Nothing}
      ["--monitoring-port", "12345"]
        `shouldParse` Run defaultRunOptions{monitoringPort = Just 12345}
      ["--monitoring-port", "65535"]
        `shouldParse` Run defaultRunOptions{monitoringPort = Just 65535}

    it "flag --version returns version with base version from cabal" $ do
      case parseHydraCommandFromArgs ["--version"] of
        Failure theFailure ->
          let (v, _ExitCode) = renderFailure theFailure "test"
           in v
                `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(:?-[a-zA-Z0-9]+)" :: String))
        _ -> failure "expected a version but did get something else"
    it "parses --hydra-verification-key option as a filepath" $ do
      ["--hydra-verification-key", "./alice.vk"]
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["./alice.vk"]}
      ["--hydra-verification-key", "/foo"]
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["/foo"]}
      ["--hydra-verification-key", "bar"]
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["bar"]}
      ["--hydra-verification-key", "alice.vk", "--hydra-verification-key", "bob.vk"]
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["alice.vk", "bob.vk"]}

    it "parses --hydra-signing-key option as a filepath" $
      ["--hydra-signing-key", "./alice.sk"]
        `shouldParse` Run defaultRunOptions{hydraSigningKey = "./alice.sk"}

    it "parses --testnet-magic option as a number" $ do
      shouldNotParse ["--testnet-magic", "abc"]
      ["--testnet-magic", "0"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { networkId = Testnet (NetworkMagic 0)
                    }
            }
      ["--testnet-magic", "-1"] -- Word32 overflow expected
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { networkId = Testnet (NetworkMagic 4294967295)
                    }
            }
      ["--testnet-magic", "123"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { networkId = Testnet (NetworkMagic 123)
                    }
            }

    it "parses --mainnet option" $ do
      ["--node-id", "hydra-node-1", "--mainnet"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { networkId = Mainnet
                    }
            }

    it "parses --contestation-period option as a number of seconds" $ do
      shouldNotParse ["--contestation-period", "abc"]
      shouldNotParse ["--contestation-period", "s"]
      shouldNotParse ["--contestation-period", "-1"]
      shouldNotParse ["--contestation-period", "0s"]
      shouldNotParse ["--contestation-period", "00s"]
      ["--contestation-period", "1s"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { contestationPeriod = UnsafeContestationPeriod 1
                    }
            }
      ["--contestation-period", "300s"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { contestationPeriod = UnsafeContestationPeriod 300
                    }
            }

    it "parses --mainnet flag" $ do
      ["--mainnet"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { networkId = Mainnet
                    }
            }

    it "parses --node-socket as a filepath" $
      ["--node-socket", "foo.sock"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { nodeSocket = "foo.sock"
                    }
            }

    it "parses --cardano-signing-key option as a filepath" $
      ["--cardano-signing-key", "./alice-cardano.sk"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { cardanoSigningKey = "./alice-cardano.sk"
                    }
            }

    it "parses --cardano-verification-key option as a filepath" $
      ["--cardano-verification-key", "./alice-cardano.vk"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { cardanoVerificationKeys = ["./alice-cardano.vk"]
                    }
            }

    it "parses --ledger-protocol-parameters-file as a filepath" $
      ["--ledger-protocol-parameters", "my-custom-protocol-parameters.json"]
        `shouldParse` Run
          defaultRunOptions
            { ledgerConfig =
                defaultLedgerConfig
                  { cardanoLedgerProtocolParametersFile = "my-custom-protocol-parameters.json"
                  }
            }

    it "parses --start-chain-from as a pair of slot number and block header hash" $ do
      ["--start-chain-from", "1000.0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { startChainFrom =
                        Just $
                          ChainPoint 1000 $
                            fromString "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
                    }
            }

    it "parses --start-chain-from 0 as starting from genesis" $
      ["--start-chain-from", "0"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig = Direct defaultDirectChainConfig{startChainFrom = Just ChainPointAtGenesis}
            }

    prop "parses --hydra-scripts-tx-id as a tx id" $ \txId ->
      ["--hydra-scripts-tx-id", toString $ serialiseToRawBytesHexText txId]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig = Direct defaultDirectChainConfig{hydraScriptsTxId = txId}
            }

    it "switches to offline chain when using --initial-utxo" $
      mconcat
        [ ["--initial-utxo", "some-file"]
        ]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig = Offline defaultOfflineChainConfig{initialUTxOFile = "some-file"}
            }

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
            , ["--mainnet"]
            ]

      xit "does not parse with some missing option (2)" $
        shouldNotParse $
          mconcat
            [ ["publish-scripts"]
            , ["--testnet-magic", "42"]
            , ["--cardano-signing-key", "foo"]
            ]

      xit "does not parse with some missing option (3)" $
        shouldNotParse $
          mconcat
            [ ["publish-scripts"]
            , ["--node-socket", "foo"]
            , ["--cardano-signing-key", "foo"]
            ]

      it "should parse using testnet and all options" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "foo"]
          , ["--testnet-magic", "42"]
          , ["--cardano-signing-key", "bar"]
          ]
          `shouldParse` Publish
            PublishOptions
              { publishNodeSocket = "foo"
              , publishNetworkId = Testnet (NetworkMagic 42)
              , publishSigningKey = "bar"
              }
      it "should parse using mainnet and all options" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "baz"]
          , ["--mainnet"]
          , ["--cardano-signing-key", "crux"]
          ]
          `shouldParse` Publish
            PublishOptions
              { publishNodeSocket = "baz"
              , publishNetworkId = Mainnet
              , publishSigningKey = "crux"
              }

    describe "offline sub-command" $ do
      it "does parse with defaults" $
        ["offline"]
          `shouldParse` Run defaultRunOptions{chainConfig = Offline defaultOfflineChainConfig}

      it "does parse --ledger-genesis" $
        mconcat
          [ ["offline"]
          , ["--ledger-genesis", "some-file"]
          ]
          `shouldParse` Run
            defaultRunOptions
              { chainConfig =
                  Offline
                    defaultOfflineChainConfig{ledgerGenesisFile = Just "some-file"}
              }

      it "does parse --initial-utxo" $
        mconcat
          [ ["offline"]
          , ["--initial-utxo", "some-file"]
          ]
          `shouldParse` Run
            defaultRunOptions
              { chainConfig =
                  Offline
                    defaultOfflineChainConfig{initialUTxOFile = "some-file"}
              }

    describe "gen-hydra-keys sub-command" $ do
      it "should be able to parse gen-hydra-keys sub-command" $
        mconcat
          [ ["gen-hydra-key"]
          , ["--output-file", "foo"]
          ]
          `shouldParse` GenHydraKey GenerateKeyPair{outputFile = "foo"}

      it "should parse gen-hydra-keys without the output-file flag using default file name" $
        ["gen-hydra-key"] `shouldParse` GenHydraKey GenerateKeyPair{outputFile = "hydra-key"}

    roundtripAndGoldenSpecs (Proxy @RunOptions)

    prop "roundtrip parsing & printing" $
      forAll arbitrary canRoundtripRunOptionsAndPrettyPrinting

canRoundtripRunOptionsAndPrettyPrinting :: RunOptions -> Property
canRoundtripRunOptionsAndPrettyPrinting opts =
  let args = toArgs opts
   in counterexample ("args:  " <> show args) $
        case parseHydraCommandFromArgs args of
          Success cmd -> cmd === Run opts
          err -> property False & counterexample ("error : " <> show err)

shouldParse :: HasCallStack => [String] -> Command -> Expectation
shouldParse args cmd =
  case parseHydraCommandFromArgs args of
    Success a -> a `shouldBe` cmd
    err -> failure (show err)

shouldNotParse :: HasCallStack => [String] -> Expectation
shouldNotParse args =
  case parseHydraCommandFromArgs args of
    Success a -> failure $ "Unexpected successful parse to " <> show a
    Failure _ -> pure ()
    CompletionInvoked _ -> failure "Unexpected completion invocation"
