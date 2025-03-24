module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (
  ChainPoint (..),
  NetworkId (..),
  TxId,
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
  defaultRunOptions,
  outputFile,
  parseHydraCommandFromArgs,
  renderFailure,
  toArgs,
  validateRunOptions,
 )
import Hydra.Tx.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Tx.DepositDeadline (DepositDeadline (..))
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

    it "parses --listen option given valid IPv4 and IPv6 addresses and ports" $ do
      ["--listen", "127.0.0.1:5001"]
        `shouldParse` Run defaultRunOptions{listen = Host "127.0.0.1" 5001}
      -- TODO: Restore with https://hackage.haskell.org/package/IPv6Addr-2.0.6/docs/Text-IPv6Addr.html
      -- ["--listen", "2001:db8:11e:c00::101:5001"]
      --   `shouldParse` Run defaultRunOptions{listen = Host "2001:db8:11e:c00::101" 5001}
      ["--listen", "0.0.0.0:5001"]
        `shouldParse` Run defaultRunOptions{listen = Host "0.0.0.0" 5001}
      shouldNotParse ["--listen", "0.0.0"]
    -- shouldNotParse ["--listen", "2001:db8:11e:c00:101"]

    it "parses --advertise option given valid IPv4 and IPv6 addresses and ports" $ do
      ["--advertise", "127.0.0.1:5001"]
        `shouldParse` Run defaultRunOptions{advertise = Just $ Host "127.0.0.1" 5001}
      -- TODO: Restore with https://hackage.haskell.org/package/IPv6Addr-2.0.6/docs/Text-IPv6Addr.html
      -- ["--advertise", "2001:db8:11e:c00::101:5001"]
      --   `shouldParse` Run defaultRunOptions{advertise = Just $ Host "2001:db8:11e:c00::101" 5001}
      ["--advertise", "0.0.0.0:5001"]
        `shouldParse` Run defaultRunOptions{advertise = Just $ Host "0.0.0.0" 5001}
      shouldNotParse ["--advertise", "0.0.0"]
    -- shouldNotParse ["--advertise", "2001:db8:11e:c00:101"]

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
    it "parses --deposit-deadline option as a number of seconds" $ do
      shouldNotParse ["--deposit-deadline", "abc"]
      shouldNotParse ["--deposit-deadline", "s"]
      shouldNotParse ["--deposit-deadline", "-1"]
      shouldNotParse ["--deposit-deadline", "0s"]
      shouldNotParse ["--deposit-deadline", "00s"]
      ["--deposit-deadline", "1s"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { depositDeadline = UnsafeDepositDeadline 1
                    }
            }
      ["--deposit-deadline", "300s"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Direct
                  defaultDirectChainConfig
                    { depositDeadline = UnsafeDepositDeadline 300
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

    prop "parses --hydra-scripts-tx-id as a tx id" $ \(txIds :: NonEmpty TxId) -> do
      let lineToParse = intercalate "," $ toString . serialiseToRawBytesHexText <$> toList txIds
      ["--hydra-scripts-tx-id", lineToParse]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig = Direct defaultDirectChainConfig{hydraScriptsTxId = toList txIds}
            }

    it "switches to offline mode when using --offline-head-seed and --initial-utxo" $
      mconcat
        [ ["--offline-head-seed", "0100"]
        , ["--initial-utxo", "some-file"]
        ]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Offline
                  OfflineChainConfig
                    { offlineHeadSeed = "\01\00"
                    , initialUTxOFile = "some-file"
                    , ledgerGenesisFile = Nothing
                    }
            }

    it "requires --offline-head-seed and --initial-utxo for offline mode" $ do
      shouldNotParse ["--offline-head-seed", "not-hex"]
      shouldNotParse ["--initial-utxo", "utxo.json"]

    it "parses --ledger-genesis in offline mode" $
      mconcat
        [ ["--offline-head-seed", "0001"]
        , ["--initial-utxo", "some-file"]
        , ["--ledger-genesis", "genesis-file"]
        ]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                Offline
                  OfflineChainConfig
                    { offlineHeadSeed = "\00\01"
                    , initialUTxOFile = "some-file"
                    , ledgerGenesisFile = Just "genesis-file"
                    }
            }

    describe "publish-scripts sub-command" $ do
      it "parses without any options" $
        [ "publish-scripts"
        ]
          `shouldParse` Publish
            (PublishOptions $ Direct defaultDirectChainConfig)

      it "parses with some missing option (1)" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "foo"]
          , ["--mainnet"]
          ]
          `shouldParse` Publish
            ( PublishOptions $
                Direct defaultDirectChainConfig{nodeSocket = "foo", networkId = Mainnet}
            )

      it "parses with some missing option (2)" $
        mconcat
          [ ["publish-scripts"]
          , ["--testnet-magic", "42"]
          , ["--cardano-signing-key", "foo"]
          ]
          `shouldParse` Publish
            (PublishOptions $ Direct defaultDirectChainConfig{cardanoSigningKey = "foo", networkId = Testnet (NetworkMagic 42)})

      it "parses with some missing option (3)" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "foo"]
          , ["--cardano-signing-key", "foo"]
          ]
          `shouldParse` Publish
            (PublishOptions $ Direct defaultDirectChainConfig{nodeSocket = "foo", cardanoSigningKey = "foo"})

      it "should parse using testnet and all options" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "foo"]
          , ["--testnet-magic", "42"]
          , ["--cardano-signing-key", "bar"]
          ]
          `shouldParse` Publish
            ( PublishOptions $
                Direct
                  defaultDirectChainConfig
                    { nodeSocket = "foo"
                    , networkId = Testnet (NetworkMagic 42)
                    , cardanoSigningKey = "bar"
                    }
            )
      it "should parse using mainnet and all options" $
        mconcat
          [ ["publish-scripts"]
          , ["--node-socket", "baz"]
          , ["--mainnet"]
          , ["--cardano-signing-key", "crux"]
          ]
          `shouldParse` Publish
            ( PublishOptions $
                Direct
                  defaultDirectChainConfig
                    { nodeSocket = "baz"
                    , networkId = Mainnet
                    , cardanoSigningKey = "crux"
                    }
            )

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
