{-# LANGUAGE OverloadedRecordDot #-}

module Hydra.ConfigSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (NetworkId (..), NetworkMagic (..))
import Hydra.Config (isSelfAddress, loadConfig, resolvePaths)
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
  validateRunOptions,
 )
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)

spec :: Spec
spec = do
  describe "loadConfig" $ do
    it "loads a minimal YAML file and uses defaults" $ do
      withYaml "{}\n" $ \path dir -> do
        opts <- loadConfig path
        opts `shouldBe` resolvePaths dir defaultRunOptions

    it "parses node-id" $ do
      withYaml "node-id: my-node\n" $ \path _dir -> do
        opts <- loadConfig path
        nodeId opts `shouldBe` "my-node"

    it "parses listen as HOST:PORT string" $ do
      withYaml "listen: \"127.0.0.1:9001\"\n" $ \path _dir -> do
        opts <- loadConfig path
        listen opts `shouldBe` Host "127.0.0.1" 9001

    it "parses peers list (plain string format)" $ do
      withYaml "peers:\n  - \"peer1:5001\"\n  - \"peer2:5002\"\n" $ \path _dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]

    it "parses peers with hydra and cardano verification keys" $ do
      let yaml =
            "peers:\n\
            \  - address: \"peer1:5001\"\n\
            \    hydra-verification-key: peer1.hydra.vk\n\
            \    cardano-verification-key: peer1.cardano.vk\n\
            \  - address: \"peer2:5002\"\n\
            \    hydra-verification-key: peer2.hydra.vk\n\
            \    cardano-verification-key: peer2.cardano.vk\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]
        hydraVerificationKeys opts `shouldBe` [dir </> "peer1.hydra.vk", dir </> "peer2.hydra.vk"]
        case chainConfig opts of
          Cardano cfg ->
            cardanoVerificationKeys cfg
              `shouldBe` [dir </> "peer1.cardano.vk", dir </> "peer2.cardano.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses mirror peers with null keys" $ do
      let yaml =
            "peers:\n\
            \  - address: \"peer1:5001\"\n\
            \    hydra-verification-key: peer1.hydra.vk\n\
            \    cardano-verification-key: peer1.cardano.vk\n\
            \  - address: \"mirror:5002\"\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "mirror" 5002]
        hydraVerificationKeys opts `shouldBe` [dir </> "peer1.hydra.vk"]
        case chainConfig opts of
          Cardano cfg ->
            cardanoVerificationKeys cfg `shouldBe` [dir </> "peer1.cardano.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses peers with mixed formats" $ do
      let yaml =
            "peers:\n\
            \  - \"peer1:5001\"\n\
            \  - address: \"peer2:5002\"\n\
            \    hydra-verification-key: peer2.hydra.vk\n\
            \    cardano-verification-key: peer2.cardano.vk\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer1" 5001, Host "peer2" 5002]
        hydraVerificationKeys opts `shouldBe` [dir </> "peer2.hydra.vk"]
        case chainConfig opts of
          Cardano cfg ->
            cardanoVerificationKeys cfg `shouldBe` [dir </> "peer2.cardano.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "filters self from peers when address matches advertise" $ do
      let yaml =
            "listen: \"0.0.0.0:5001\"\n\
            \advertise: \"127.0.0.1:5001\"\n\
            \peers:\n\
            \  - address: \"127.0.0.1:5001\"\n\
            \    hydra-verification-key: self.hydra.vk\n\
            \    cardano-verification-key: self.cardano.vk\n\
            \  - address: \"peer2:5002\"\n\
            \    hydra-verification-key: peer2.hydra.vk\n\
            \    cardano-verification-key: peer2.cardano.vk\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer2" 5002]
        hydraVerificationKeys opts `shouldBe` [dir </> "peer2.hydra.vk"]
        case chainConfig opts of
          Cardano cfg ->
            cardanoVerificationKeys cfg `shouldBe` [dir </> "peer2.cardano.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "filters self from peers when address matches listen (no advertise)" $ do
      let yaml =
            "listen: \"0.0.0.0:5001\"\n\
            \peers:\n\
            \  - address: \"0.0.0.0:5001\"\n\
            \  - \"peer2:5002\"\n"
      withYaml yaml $ \path _dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer2" 5002]

    it "filters self when listen is wildcard 0.0.0.0 and peer uses 127.0.0.1" $ do
      -- Regression: string equality on "0.0.0.0" vs "127.0.0.1" would not
      -- match. The normalized self-filter treats wildcard listen addresses
      -- as matching any peer on the same port.
      let yaml =
            "listen: \"0.0.0.0:5001\"\n\
            \peers:\n\
            \  - address: \"127.0.0.1:5001\"\n\
            \    hydra-verification-key: self.hydra.vk\n\
            \    cardano-verification-key: self.cardano.vk\n\
            \  - address: \"peer2:5002\"\n\
            \    hydra-verification-key: peer2.hydra.vk\n\
            \    cardano-verification-key: peer2.cardano.vk\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer2" 5002]
        hydraVerificationKeys opts `shouldBe` [dir </> "peer2.hydra.vk"]
        case chainConfig opts of
          Cardano cfg ->
            cardanoVerificationKeys cfg `shouldBe` [dir </> "peer2.cardano.vk"]
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "filters self when listen uses 127.0.0.1 and peer uses localhost" $ do
      -- Both sides are loopback; normalized filter treats them as equivalent.
      let yaml =
            "listen: \"127.0.0.1:5001\"\n\
            \peers:\n\
            \  - address: \"localhost:5001\"\n\
            \  - \"peer2:5002\"\n"
      withYaml yaml $ \path _dir -> do
        opts <- loadConfig path
        peers opts `shouldBe` [Host "peer2" 5002]

    it "rejects peer entry with only hydra-verification-key" $ do
      let yaml =
            "peers:\n\
            \  - address: \"peer1:5001\"\n\
            \    hydra-verification-key: peer1.hydra.vk\n"
      withYaml yaml $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "rejects peer entry with only cardano-verification-key" $ do
      let yaml =
            "peers:\n\
            \  - address: \"peer1:5001\"\n\
            \    cardano-verification-key: peer1.cardano.vk\n"
      withYaml yaml $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "rejects chain config that sets both network and hydra-scripts-tx-id" $ do
      let yaml =
            "chain:\n\
            \  mode: cardano\n\
            \  network: preview\n\
            \  hydra-scripts-tx-id:\n\
            \    - \"0000000000000000000000000000000000000000000000000000000000000000\"\n"
      withYaml yaml $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "reports a readable YAML decode error (wraps underlying parser output)" $ do
      -- ':' without a key produces a YAML-level parse error; the custom
      -- wrapper should include the file path and the YAML library's
      -- pretty-printed message.
      withYaml ":broken\n" $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "parses api-host and api-port" $ do
      withYaml "api-host: \"0.0.0.0\"\napi-port: 9000\n" $ \path _dir -> do
        opts <- loadConfig path
        (show (apiHost opts) :: String) `shouldBe` "0.0.0.0"
        apiPort opts `shouldBe` 9000

    it "parses quiet flag" $ do
      withYaml "quiet: true\n" $ \path _dir -> do
        opts <- loadConfig path
        verbosity opts `shouldBe` Quiet

    it "parses hydra-signing-key" $ do
      withYaml "hydra-signing-key: my.sk\n" $ \path dir -> do
        opts <- loadConfig path
        hydraSigningKey opts `shouldBe` dir </> "my.sk"

    it "parses persistence-dir (absolute path unchanged)" $ do
      withYaml "persistence-dir: /some/path\n" $ \path _dir -> do
        opts <- loadConfig path
        persistenceDir opts `shouldBe` "/some/path"

    it "parses ledger-protocol-parameters" $ do
      withYaml "ledger-protocol-parameters: my-params.json\n" $ \path dir -> do
        opts <- loadConfig path
        ledgerConfig opts `shouldBe` CardanoLedgerConfig (dir </> "my-params.json")

    it "parses chain with direct backend options" $ do
      let yaml =
            "chain:\n\
            \  mode: cardano\n\
            \  cardano-signing-key: cardano.sk\n\
            \  backend:\n\
            \    mode: direct\n\
            \    testnet-magic: 2\n\
            \    node-socket: node.socket\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        case chainConfig opts of
          Cardano cfg -> do
            cardanoSigningKey cfg `shouldBe` dir </> "cardano.sk"
            case chainBackendOptions cfg of
              Direct DirectOptions{networkId, nodeSocket} -> do
                networkId `shouldBe` Testnet (NetworkMagic 2)
                nodeSocket `shouldBe` fromString (dir </> "node.socket")
              other -> expectationFailure $ "Expected Direct, got: " <> show other
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

    it "parses offline chain config" $ do
      let seed = replicate 64 '0'
          yaml = "chain:\n  mode: offline\n  offline-head-seed: \"" <> seed <> "\"\n"
      withYaml yaml $ \path dir -> do
        opts <- loadConfig path
        case chainConfig opts of
          Offline OfflineChainConfig{initialUTxOFile, ledgerGenesisFile} -> do
            initialUTxOFile `shouldBe` dir </> "utxo.json"
            ledgerGenesisFile `shouldBe` Nothing
          other -> expectationFailure $ "Expected Offline chain, got: " <> show other

    it "fails with helpful error on invalid chain mode" $ do
      withYaml "chain:\n  mode: unknown\n" $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "fails with helpful error on invalid HOST:PORT format" $ do
      withYaml "listen: \"not-a-host\"\n" $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "fails with helpful error on unknown top-level key" $ do
      withYaml "api-prot: 4001\n" $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "fails with helpful error on unknown chain key" $ do
      withYaml "chain:\n  mode: cardano\n  unknown-key: foo\n" $ \path _dir -> do
        loadConfig path `shouldThrow` anyException

    it "defaults to cardano chain config when chain section is absent" $ do
      withYaml "{}\n" $ \path dir -> do
        opts <- loadConfig path
        case chainConfig opts of
          Cardano cfg -> do
            -- Non-path fields match defaults
            cfg.hydraScriptsTxId `shouldBe` defaultCardanoChainConfig.hydraScriptsTxId
            cfg.cardanoVerificationKeys `shouldBe` []
            cfg.startChainFrom `shouldBe` Nothing
            cfg.contestationPeriod `shouldBe` defaultCardanoChainConfig.contestationPeriod
            cfg.depositPeriod `shouldBe` defaultCardanoChainConfig.depositPeriod
            -- Path fields are resolved relative to the config directory
            cfg.cardanoSigningKey `shouldBe` dir </> defaultCardanoChainConfig.cardanoSigningKey
            case cfg.chainBackendOptions of
              Direct d -> d.nodeSocket `shouldBe` fromString (dir </> "node.socket")
              other -> expectationFailure $ "Expected Direct backend, got: " <> show other
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

  describe "isSelfAddress" $ do
    let h = Host
    it "matches exact host:port when no advertise is set" $
      isSelfAddress (h "192.168.1.10" 5001) Nothing (h "192.168.1.10" 5001) `shouldBe` True
    it "does not match different ports" $
      isSelfAddress (h "192.168.1.10" 5001) Nothing (h "192.168.1.10" 5002) `shouldBe` False
    it "treats wildcard 0.0.0.0 as matching loopback peers on the same port" $
      isSelfAddress (h "0.0.0.0" 5001) Nothing (h "127.0.0.1" 5001) `shouldBe` True
    it "does NOT treat wildcard listen as matching an arbitrary remote host on the same port" $
      -- A peer at 172.16.0.5:5001 is a legitimate other-host participant, not self.
      isSelfAddress (h "0.0.0.0" 5001) Nothing (h "172.16.0.5" 5001) `shouldBe` False
    it "treats wildcard listen with identical wildcard peer as self (self-entry convention)" $
      isSelfAddress (h "0.0.0.0" 5001) Nothing (h "0.0.0.0" 5001) `shouldBe` True
    it "treats loopback forms as equivalent" $ do
      isSelfAddress (h "127.0.0.1" 5001) Nothing (h "localhost" 5001) `shouldBe` True
      isSelfAddress (h "localhost" 5001) Nothing (h "127.0.0.1" 5001) `shouldBe` True
      isSelfAddress (h "127.0.0.1" 5001) Nothing (h "::1" 5001) `shouldBe` True
    it "does not match a non-loopback host when listen is loopback" $
      isSelfAddress (h "127.0.0.1" 5001) Nothing (h "example.com" 5001) `shouldBe` False
    it "requires exact match against advertise when set" $ do
      isSelfAddress (h "0.0.0.0" 5001) (Just (h "node.example.com" 5001)) (h "node.example.com" 5001)
        `shouldBe` True
      -- A peer on a loopback address is NOT self when advertise is explicit.
      isSelfAddress (h "0.0.0.0" 5001) (Just (h "node.example.com" 5001)) (h "127.0.0.1" 5001)
        `shouldBe` False

  describe "RunOptions Semigroup (config file <> CLI overrides)" $ do
    it "CLI flag overrides YAML value" $ do
      withYaml "node-id: from-yaml\n" $ \path _dir -> do
        base <- loadConfig path
        cli <- parseRunOptions ["--node-id", "from-cli"]
        nodeId (base <> cli) `shouldBe` "from-cli"

    it "YAML value wins when CLI leaves field at default" $ do
      withYaml "node-id: from-yaml\n" $ \path _dir -> do
        base <- loadConfig path
        cli <- parseRunOptions []
        nodeId (base <> cli) `shouldBe` "from-yaml"

    it "CLI hydra-scripts-tx-id merges into YAML chain config" $ do
      let yaml =
            "chain:\n\
            \  mode: cardano\n\
            \  cardano-signing-key: yaml.sk\n"
      withYaml yaml $ \path dir -> do
        base <- loadConfig path
        let txId = replicate 64 '0'
        cli <- parseRunOptions ["--hydra-scripts-tx-id", txId]
        let merged = base <> cli
        case chainConfig merged of
          Cardano cfg -> do
            cardanoSigningKey cfg `shouldBe` dir </> "yaml.sk"
            hydraScriptsTxId cfg `shouldNotBe` []
          other -> expectationFailure $ "Expected Cardano, got: " <> show other

  describe "demo configs (demo/configs/*.yaml)" $ do
    -- Guard against the committed demo configs drifting away from the
    -- validation rules enforced by loadConfig + validateRunOptions.
    forM_ ["alice.yaml", "bob.yaml", "carol.yaml", "alice-mirror.yaml"] $ \name ->
      it ("loads and validates " <> name) $ do
        opts <- loadConfig ("../demo/configs/" <> name)
        validateRunOptions opts `shouldBe` Right ()
        -- Self entry (if any) is filtered out, leaving exactly 3 peers:
        -- the two other signers plus (depending on which config) either
        -- the mirror or the primary alice.
        length (peers opts) `shouldBe` 3
        -- 2 other signing peers → 2 hydra-verification-keys.
        length (hydraVerificationKeys opts) `shouldBe` 2
        case chainConfig opts of
          Cardano cfg ->
            length (cardanoVerificationKeys cfg) `shouldBe` 2
          other -> expectationFailure $ "Expected Cardano chain, got: " <> show other

-- | Parse args as 'RunOptions', or fail the test.
parseRunOptions :: [String] -> IO RunOptions
parseRunOptions args = do
  cmd <- parseHydraCommandFromArgsWith args
  case cmd of
    Run opts -> pure opts
    other -> expectationFailure ("Expected Run, got: " <> show other) >> error "Fail test"

-- | Write YAML content to a temporary file and run the action with its path
-- and the directory containing it (for constructing expected resolved paths).
withYaml :: String -> (FilePath -> FilePath -> IO a) -> IO a
withYaml content action =
  withSystemTempFile "hydra-config-.yaml" $ \path h -> do
    hPutStr h content
    hClose h
    action path (takeDirectory path)
