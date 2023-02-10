{-# LANGUAGE TypeApplications #-}

module Hydra.OptionsSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (ChainPoint (..), NetworkId (..), serialiseToRawBytesHexText, unsafeDeserialiseFromRawBytesBase16)
import Hydra.Chain.Direct (NetworkMagic (..))
import Hydra.ContestationPeriod (ContestationPeriod (UnsafeContestationPeriod))
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host (Host), NodeId (NodeId))
import Hydra.Options (
  ChainConfig (..),
  Command (..),
  InvalidOptions (..),
  LedgerConfig (..),
  ParserResult (..),
  PublishOptions (..),
  RunOptions (..),
  defaultChainConfig,
  defaultLedgerConfig,
  maximumNumberOfParties,
  parseHydraCommandFromArgs,
  toArgs,
  validateRunOptions,
 )
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.QuickCheck (Property, chooseEnum, counterexample, forAll, property, vectorOf, (===))

spec :: Spec
spec = parallel $
  describe "Hydra Node RunOptions" $ do
    let defaultNodeOptions = ["--node-id", "node-id-1", "--testnet-magic", "42"]
        genKeyString = vectorOf 10 $ chooseEnum ('a', 'z')
        genCardanoAndHydraKeys f1 f2 = flip generateWith 42 $ do
          cks <- replicateM (f1 maximumNumberOfParties) genKeyString
          hks <- replicateM (f2 maximumNumberOfParties) genKeyString
          pure (cks, hks)

    it ("validateRunOptions: using more than " <> show maximumNumberOfParties <> " parties should error out") $ do
      let (cardanoKeys, hydraKeys) = genCardanoAndHydraKeys (+ 2) (+ 1)
          chainCfg = (chainConfig defaultRunOptions){cardanoVerificationKeys = cardanoKeys}
      validateRunOptions (defaultRunOptions{hydraVerificationKeys = hydraKeys, chainConfig = chainCfg})
        `shouldBe` Left MaximumNumberOfPartiesExceeded
    it "validateRunOptions: loaded cardano keys needs to match with the hydra keys length" $ do
      let (cardanoKeys, hydraKeys) = genCardanoAndHydraKeys (subtract 2) (subtract 1)
          chainCfg = (chainConfig defaultRunOptions){cardanoVerificationKeys = cardanoKeys}
      validateRunOptions (defaultRunOptions{hydraVerificationKeys = hydraKeys, chainConfig = chainCfg})
        `shouldBe` Left CardanoAndHydraKeysMissmatch

    it "parses with node-id and testnet-magic set" $
      defaultNodeOptions `shouldParse` Run defaultRunOptions

    it "parses --host option given valid IPv4 and IPv6 addresses" $ do
      (defaultNodeOptions <> ["--host", "127.0.0.1"])
        `shouldParse` Run defaultRunOptions{host = "127.0.0.1"}
      (defaultNodeOptions <> ["--host", "2001:db8:11e:c00::101"])
        `shouldParse` Run defaultRunOptions{host = "2001:db8:11e:c00::101"}
      (defaultNodeOptions <> ["--host", "0.0.0.0"])
        `shouldParse` Run defaultRunOptions{host = "0.0.0.0"}
      shouldNotParse ["--host", "0.0.0"]
      shouldNotParse ["--host", "2001:db8:11e:c00:101"]

    it "parses --port option given valid port number" $ do
      (defaultNodeOptions <> ["--port", "12345"])
        `shouldParse` Run defaultRunOptions{port = 12345}
      shouldNotParse ["--port", "123456"]
      (defaultNodeOptions <> ["--port", "0"])
        `shouldParse` Run defaultRunOptions{port = 0}
      shouldNotParse ["--port", "-42"]

    -- TODO(SN): Move thes examples rather into a 'instance Read Host' test and
    -- only check for correct format / wiring here using a single test case This
    -- became evident when realizing that the 'hydra-tui' is also relying on this
    -- Read instance for parsing, but in a different command line flag.
    it "parses --peer `<host>:<port>` option" $ do
      (defaultNodeOptions <> ["--peer", "1.2.3.4:4567"])
        `shouldParse` Run defaultRunOptions{peers = [Host "1.2.3.4" 4567]}
      (defaultNodeOptions <> ["--peer", "1.2.3.4:4567", "--peer", "1.2.3.5:4568"])
        `shouldParse` Run defaultRunOptions{peers = [Host "1.2.3.4" 4567, Host "1.2.3.5" 4568]}
      (defaultNodeOptions <> ["--peer", "foo.com:4567"])
        `shouldParse` Run defaultRunOptions{peers = [Host "foo.com" 4567]}
      shouldNotParse ["--peer", "foo.com:456789"]
    it "does parse --peer given ipv6 addresses" $ do
      pendingWith "we do not support it"
      (defaultNodeOptions <> ["--peer", ":::1:4567"])
        `shouldParse` Run defaultRunOptions{peers = [Host ":::1" 4567]}

    it "parses --monitoring-port option given valid port number" $ do
      (defaultNodeOptions <> [])
        `shouldParse` Run defaultRunOptions{monitoringPort = Nothing}
      (defaultNodeOptions <> ["--monitoring-port", "12345"])
        `shouldParse` Run defaultRunOptions{monitoringPort = Just 12345}
      (defaultNodeOptions <> ["--monitoring-port", "65535"])
        `shouldParse` Run defaultRunOptions{monitoringPort = Just 65535}

    it "parses --version flag as a parse error" $
      shouldNotParse ["--version"]

    it "parses --hydra-verification-key option as a filepath" $ do
      (defaultNodeOptions <> ["--hydra-verification-key", "./alice.vk"])
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["./alice.vk"]}
      (defaultNodeOptions <> ["--hydra-verification-key", "/foo"])
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["/foo"]}
      (defaultNodeOptions <> ["--hydra-verification-key", "bar"])
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["bar"]}
      (defaultNodeOptions <> ["--hydra-verification-key", "alice.vk", "--hydra-verification-key", "bob.vk"])
        `shouldParse` Run defaultRunOptions{hydraVerificationKeys = ["alice.vk", "bob.vk"]}

    it "parses --hydra-signing-key option as a filepath" $
      (defaultNodeOptions <> ["--hydra-signing-key", "./alice.sk"])
        `shouldParse` Run defaultRunOptions{hydraSigningKey = "./alice.sk"}

    it "parses --testnet-magic option as a number" $ do
      shouldNotParse ["--testnet-magic", "abc"]
      ["--node-id", "node-id-1", "--testnet-magic", "0"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 0)
                  }
            }
      ["--node-id", "node-id-1", "--testnet-magic", "-1"] -- Word32 overflow expected
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 4294967295)
                  }
            }
      ["--node-id", "node-id-1", "--testnet-magic", "123"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Testnet (NetworkMagic 123)
                  }
            }

    it "parses --mainnet option" $ do
      ["--node-id", "node-id-1", "--mainnet"]
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { networkId = Mainnet
                  }
            }

    it "parses --contestation-period option as a number of seconds" $ do
      shouldNotParse ["--contestation-period", "abc"]
      shouldNotParse ["--contestation-period", "s"]
      shouldNotParse ["--contestation-period", "0"]
      shouldNotParse ["--contestation-period", "0s"]
      shouldNotParse ["--contestation-period", "00s"]
      (defaultNodeOptions <> ["--contestation-period", "60s"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { contestationPeriod = UnsafeContestationPeriod 60
                  }
            }
      (defaultNodeOptions <> ["--contestation-period", "300s"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { contestationPeriod = UnsafeContestationPeriod 300
                  }
            }

    it "parses --mainnet flag" $ do
      shouldNotParse ["--mainnet"]

    it "parses --node-socket as a filepath" $
      (defaultNodeOptions <> ["--node-socket", "foo.sock"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { nodeSocket = "foo.sock"
                  }
            }

    it "parses --cardano-signing-key option as a filepath" $
      (defaultNodeOptions <> ["--cardano-signing-key", "./alice-cardano.sk"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { cardanoSigningKey = "./alice-cardano.sk"
                  }
            }

    it "parses --cardano-verification-key option as a filepath" $
      (defaultNodeOptions <> ["--cardano-verification-key", "./alice-cardano.vk"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig =
                defaultChainConfig
                  { cardanoVerificationKeys = ["./alice-cardano.vk"]
                  }
            }

    it "parses --ledger-genesis-file as a filepath" $
      (defaultNodeOptions <> ["--ledger-genesis", "my-custom-genesis.json"])
        `shouldParse` Run
          defaultRunOptions
            { ledgerConfig =
                defaultLedgerConfig
                  { cardanoLedgerGenesisFile = "my-custom-genesis.json"
                  }
            }

    it "parses --ledger-protocol-parameters-file as a filepath" $
      (defaultNodeOptions <> ["--ledger-protocol-parameters", "my-custom-protocol-parameters.json"])
        `shouldParse` Run
          defaultRunOptions
            { ledgerConfig =
                defaultLedgerConfig
                  { cardanoLedgerProtocolParametersFile = "my-custom-protocol-parameters.json"
                  }
            }

    it "parses --start-chain-from as a pair of slot number and block header hash" $
      (defaultNodeOptions <> ["--start-chain-from", "1000.0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"])
        `shouldParse` Run
          defaultRunOptions
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

    it "parses --start-chain-from 0 as starting from genesis" $
      (defaultNodeOptions <> ["--start-chain-from", "0"])
        `shouldParse` Run
          defaultRunOptions
            { chainConfig = defaultChainConfig{startChainFrom = Just (ChainPointAtGenesis)}
            }

    prop "parses --hydra-scripts-tx-id as a tx id" $ \txId ->
      (defaultNodeOptions <> ["--hydra-scripts-tx-id", toString $ serialiseToRawBytesHexText txId])
        `shouldParse` Run
          defaultRunOptions
            { hydraScriptsTxId = txId
            }

    roundtripAndGoldenSpecs (Proxy @RunOptions)

    prop "roundtrip parsing & printing" $
      forAll arbitrary canRoundtripRunOptionsAndPrettyPrinting

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
            , ["--testnet-magic", "42"]
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

      it "should parse with all options" $
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

canRoundtripRunOptionsAndPrettyPrinting :: RunOptions -> Property
canRoundtripRunOptionsAndPrettyPrinting opts =
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
defaultRunOptions :: RunOptions
defaultRunOptions =
  RunOptions
    { verbosity = Verbose "HydraNode"
    , nodeId = NodeId "node-id-1"
    , host = "127.0.0.1"
    , port = 5001
    , peers = []
    , apiHost = "127.0.0.1"
    , apiPort = 4001
    , monitoringPort = Nothing
    , hydraSigningKey = "hydra.sk"
    , hydraVerificationKeys = []
    , hydraScriptsTxId = "0101010101010101010101010101010101010101010101010101010101010101"
    , persistenceDir = "./"
    , chainConfig = defaultChainConfig
    , ledgerConfig = defaultLedgerConfig
    }
