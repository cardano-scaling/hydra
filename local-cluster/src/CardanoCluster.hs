{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module CardanoCluster where

import Hydra.Prelude

import Cardano.Api (
  AsType (..),
  HasTextEnvelope,
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentKey,
  SigningKey (PaymentSigningKey),
  VerificationKey,
  readFileTextEnvelope,
  serialiseToRawBytes,
 )
import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import CardanoClient (buildAddress)
import CardanoNode (
  CardanoNodeArgs (..),
  CardanoNodeConfig (..),
  NodeId,
  NodeLog,
  Port,
  PortsConfig (..),
  RunningNode (..),
  addField,
  defaultCardanoNodeArgs,
  unsafeDecodeJsonFile,
  withCardanoNode,
 )
import Control.Lens ((%~))
import Control.Tracer (Tracer, traceWith)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Data.ByteString.Base16 (encodeBase16)
import qualified Hydra.Chain.Direct.Util as Cardano
import System.Directory (
  copyFile,
  createDirectoryIfMissing,
  doesFileExist,
 )
import System.FilePath ((<.>), (</>))
import System.Posix.Files (
  ownerReadMode,
  setFileMode,
 )
import Test.Network.Ports (randomUnusedTCPPort, randomUnusedTCPPorts)

data RunningCluster = RunningCluster ClusterConfig [RunningNode]

-- | TODO: This is hard-coded and must match what's in the genesis file, so
-- ideally, we want to either:
--
-- - overwrite the genesis configuration with the `ClusterConfig`
-- - pull the network id from the genesis configuration
defaultNetworkId :: NetworkId
defaultNetworkId = Testnet (NetworkMagic 42)

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
  { parentStateDirectory :: FilePath
  , networkId :: NetworkId
  }

testClusterConfig :: FilePath -> ClusterConfig
testClusterConfig tmp = ClusterConfig tmp defaultNetworkId

readFileTextEnvelopeThrow ::
  HasTextEnvelope a =>
  AsType a ->
  FilePath ->
  IO a
readFileTextEnvelopeThrow asType =
  either (fail . show) pure <=< readFileTextEnvelope asType

asSigningKey :: AsType (SigningKey PaymentKey)
asSigningKey = AsSigningKey AsPaymentKey

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig{parentStateDirectory} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3

  withBFTNode tr cfgA [] $ \nodeA -> do
    withBFTNode tr cfgB [] $ \nodeB -> do
      withBFTNode tr cfgC [] $ \nodeC -> do
        let nodes = [nodeA, nodeB, nodeC]
        action (RunningCluster cfg nodes)

keysFor :: String -> IO (Cardano.VerificationKey, Cardano.SigningKey)
keysFor actor = do
  PaymentSigningKey sk <-
    readFileTextEnvelopeThrow
      asSigningKey
      (signingKeyPathFor actor)
  let vk = deriveVerKeyDSIGN sk
  pure (vk, sk)

signingKeyPathFor :: String -> FilePath
signingKeyPathFor actor = "config" </> "credentials" </> actor <.> "sk"

verificationKeyPathFor :: String -> FilePath
verificationKeyPathFor actor = "config" </> "credentials" </> actor <.> "vk"

withBFTNode ::
  Tracer IO ClusterLog ->
  CardanoNodeConfig ->
  [VerificationKey PaymentKey] ->
  (RunningNode -> IO ()) ->
  IO ()
withBFTNode clusterTracer cfg initialFunds action = do
  createDirectoryIfMissing False (stateDirectory cfg)

  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    forM
      [ dlgCertFilename nid
      , signKeyFilename nid
      , vrfKeyFilename nid
      , kesKeyFilename nid
      , opCertFilename nid
      ]
      (copyCredential (stateDirectory cfg))

  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert
          , nodeSignKeyFile = Just signKey
          , nodeVrfKeyFile = Just vrfKey
          , nodeKesKeyFile = Just kesKey
          , nodeOpCertFile = Just opCert
          , nodePort = Just (ours (ports cfg))
          }

  copyFile
    ("config" </> "cardano-node.json")
    (stateDirectory cfg </> nodeConfigFile args)

  copyFile
    ("config" </> "genesis-byron.json")
    (stateDirectory cfg </> nodeByronGenesisFile args)

  addFundsToGenesisShelley (stateDirectory cfg </> nodeShelleyGenesisFile args)

  copyFile
    ("config" </> "genesis-alonzo.json")
    (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  withCardanoNode nodeTracer cfg args $ \rn -> do
    traceWith clusterTracer $ MsgNodeStarting cfg
    waitForSocket rn
    action rn
 where
  dlgCertFilename i = "delegation-cert.00" <> show (i - 1) <> ".json"
  signKeyFilename i = "delegate-keys.00" <> show (i - 1) <> ".key"
  vrfKeyFilename i = "delegate" <> show i <> ".vrf.skey"
  kesKeyFilename i = "delegate" <> show i <> ".kes.skey"
  opCertFilename i = "opcert" <> show i <> ".cert"

  addFundsToGenesisShelley file = do
    genesisJson <- unsafeDecodeJsonFile @Aeson.Value ("config" </> "genesis-shelley.json")
    let updatedJson = genesisJson & key "initialFunds" %~ updateInitialFunds
    Aeson.encodeFile file updatedJson

  copyCredential parentDir file = do
    let source = "config" </> "credentials" </> file
    let destination = parentDir </> file
    copyFile source destination
    setFileMode destination ownerReadMode
    pure destination

  nid = nodeId cfg

  nodeTracer = contramap (MsgFromNode nid) clusterTracer

  updateInitialFunds :: Aeson.Value -> Aeson.Value
  updateInitialFunds zero =
    foldr
      (\(k, v) -> addField k v)
      zero
      (mkInitialFundsEntry <$> initialFunds)

  mkInitialFundsEntry :: VerificationKey PaymentKey -> (Text, Word)
  mkInitialFundsEntry vk =
    let addr = buildAddress vk defaultNetworkId
        bytes = serialiseToRawBytes addr
     in (encodeBase16 bytes, 10_000_000_000_000)

  waitForSocket :: RunningNode -> IO ()
  waitForSocket node@(RunningNode _ socket) = do
    unlessM (doesFileExist socket) $ do
      threadDelay 0.1
      waitForSocket node

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart = do
  addUTCTime 1 <$> getCurrentTime

makeNodesConfig ::
  FilePath ->
  UTCTime ->
  [Port] ->
  (CardanoNodeConfig, CardanoNodeConfig, CardanoNodeConfig)
makeNodesConfig stateDirectory systemStart [a, b, c] =
  ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart (PortsConfig a [b, c])
  , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart (PortsConfig b [a, c])
  , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart (PortsConfig c [a, b])
  )
makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

newNodeConfig ::
  FilePath ->
  IO CardanoNodeConfig
newNodeConfig stateDirectory = do
  nodePort <- randomUnusedTCPPort
  systemStart <- initSystemStart
  pure $
    CardanoNodeConfig
      { nodeId = 1
      , stateDirectory
      , systemStart
      , ports = PortsConfig nodePort []
      }

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
