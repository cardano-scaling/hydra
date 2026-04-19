module Hydra.ConfigSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Config (loadConfig)
import Hydra.Logging (Verbosity (..))
import Hydra.Network (Host (..))
import Hydra.Options (
  CardanoChainConfig (..),
  ChainBackendOptions (..),
  ChainConfig (..),
  Command (..),
  DirectOptions (..),
  LedgerConfig (..),
  OfflineChainConfig (..),
  RunOptions (..),
  defaultCardanoChainConfig,
  defaultRunOptions,
  parseHydraCommandFromArgsWith,
 )
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads a minimal YAML file and uses defaults" $ do
      withYaml "{}\n" $ \path -> do
        opts <- loadConfig path
        opts `shouldBe` defaultRunOptions

    it "parses node-id" $ do
      withYaml "node-id: my-node\n" $ \path -> do
        opts <- loadConfig path
        nodeId opts `shouldBe` "my-node"

    it "parses listen as HOST:PORT string" $ do
      withYaml "listen: \"127.0.0.1:9001\"\n" $ \path -> do
        opts <- loadConfig path
        listen opts `shouldBe` Host "127.0.0.1" 9001

    it "parses peers list (plain string format)" $ do
      withYaml "peers:\n  - \"peer1:5001\"\n  - \"peer2:5002\"\n" $ \path -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]

    it "parses peers list with cardano-verification-key" $ do
      let yaml =
            "peers:\n\
            \  - address: \"peer1:5001\"\n\
            \    cardano-verification-key: peer1.vk\n\
            \  - address: \"peer2:5002\"\n\
            \    cardano-verification-key: peer2.vk\n"
      withYaml yaml $ \path -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]
        case chainConfig opts of
          Cardano cfg -> cardanoVerificationKeys cfg `shouldBe` ["peer1.vk", "peer2.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses peers with mixed formats" $ do
      let yaml =
            "peers:\n\
            \  - \"peer1:5001\"\n\
            \  - address: \"peer2:5002\"\n\
            \    cardano-verification-key: peer2.vk\n"
      withYaml yaml $ \path -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]
        case chainConfig opts of
          Cardano cfg -> cardanoVerificationKeys cfg `shouldBe` ["peer2.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses api-host and api-port" $ do
      withYaml "api-host: \"0.0.0.0\"\napi-port: 9000\n" $ \path -> do
        opts <- loadConfig path
        (show (apiHost opts) :: String) `shouldBe` "0.0.0.0"
        apiPort opts `shouldBe` 9000

    it "parses quiet flag" $ do
      withYaml "quiet: true\n" $ \path -> do
        opts <- loadConfig path
        verbosity opts `shouldBe` Quiet

    it "parses hydra-signing-key and hydra-verification-keys" $ do
      withYaml "hydra-signing-key: my.sk\nhydra-verification-keys:\n  - peer.vk\n" $ \path -> do
        opts <- loadConfig path
        hydraSigningKey opts `shouldBe` "my.sk"
        hydraVerificationKeys opts `shouldBe` ["peer.vk"]

    it "parses persistence-dir" $ do
      withYaml "persistence-dir: /some/path\n" $ \path -> do
        opts <- loadConfig path
        persistenceDir opts `shouldBe` "/some/path"

    it "parses ledger-protocol-parameters" $ do
      withYaml "ledger-protocol-parameters: my-params.json\n" $ \path -> do
        opts <- loadConfig path
        ledgerConfig opts `shouldBe` CardanoLedgerConfig "my-params.json"

    it "parses chain with direct backend options" $ do
      let yaml =
            "chain:\n\
            \  mode: cardano\n\
            \  cardano-signing-key: cardano.sk\n\
            \  backend:\n\
            \    mode: direct\n\
            \    testnet-magic: 2\n\
            \    node-socket: node.socket\n"
      withYaml yaml $ \path -> do
        opts <- loadConfig path
        case chainConfig opts of
          Cardano cfg -> do
            cardanoSigningKey cfg `shouldBe` "cardano.sk"
            case chainBackendOptions cfg of
              Direct DirectOptions{networkId, nodeSocket} -> do
                networkId `shouldBe` Testnet (NetworkMagic 2)
                nodeSocket `shouldBe` "node.socket"
              other -> expectationFailure $ "Expected Direct, got: " <> show other
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses offline chain config" $ do
      -- Use a known valid hex string for the head seed (32 bytes = 64 hex chars)
      let seed = replicate 64 '0'
          yaml = "chain:\n  mode: offline\n  offline-head-seed: \"" <> seed <> "\"\n"
      withYaml yaml $ \path -> do
        opts <- loadConfig path
        case chainConfig opts of
          Offline OfflineChainConfig{initialUTxOFile, ledgerGenesisFile} -> do
            initialUTxOFile `shouldBe` "utxo.json"
            ledgerGenesisFile `shouldBe` Nothing
          other -> expectationFailure $ "Expected Offline chain, got: " <> show other

    it "fails with helpful error on invalid chain mode" $ do
      withYaml "chain:\n  mode: unknown\n" $ \path -> do
        loadConfig path `shouldThrow` anyException

    it "fails with helpful error on invalid HOST:PORT format" $ do
      withYaml "listen: \"not-a-host\"\n" $ \path -> do
        loadConfig path `shouldThrow` anyException

    it "defaults to cardano chain config when chain section is absent" $ do
      withYaml "{}\n" $ \path -> do
        opts <- loadConfig path
        case chainConfig opts of
          Cardano cfg -> cfg `shouldBe` defaultCardanoChainConfig
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

  describe "RunOptions Semigroup (config file <> CLI overrides)" $ do
    it "CLI flag overrides YAML value" $ do
      withYaml "node-id: from-yaml\n" $ \path -> do
        base <- loadConfig path
        cli <- parseRunOptions ["--node-id", "from-cli"]
        nodeId (base <> cli) `shouldBe` "from-cli"

    it "YAML value wins when CLI leaves field at default" $ do
      withYaml "node-id: from-yaml\n" $ \path -> do
        base <- loadConfig path
        cli <- parseRunOptions []
        nodeId (base <> cli) `shouldBe` "from-yaml"

    it "CLI hydra-scripts-tx-id merges into YAML chain config" $ do
      let yaml =
            "chain:\n\
            \  mode: cardano\n\
            \  cardano-signing-key: yaml.sk\n"
      withYaml yaml $ \path -> do
        base <- loadConfig path
        let txId = replicate 64 '0'
        cli <- parseRunOptions ["--hydra-scripts-tx-id", txId]
        let merged = base <> cli
        case chainConfig merged of
          Cardano cfg -> do
            cardanoSigningKey cfg `shouldBe` "yaml.sk"
            hydraScriptsTxId cfg `shouldNotBe` []
          other -> expectationFailure $ "Expected Cardano, got: " <> show other

-- | Parse args as 'RunOptions', or fail the test.
parseRunOptions :: [String] -> IO RunOptions
parseRunOptions args = do
  cmd <- parseHydraCommandFromArgsWith args
  case cmd of
    Run opts -> pure opts
    other -> expectationFailure ("Expected Run, got: " <> show other) >> error "Fail test"

-- | Write YAML content to a temporary file and run the action with its path.
withYaml :: String -> (FilePath -> IO a) -> IO a
withYaml content action =
  withSystemTempFile "hydra-config-.yaml" $ \path h -> do
    hPutStr h content
    hClose h
    action path
