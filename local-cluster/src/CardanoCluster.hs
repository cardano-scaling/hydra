module CardanoCluster where

import Hydra.Prelude

import CardanoNode (
  CardanoNodeArgs (..),
  CardanoNodeConfig (..),
  NodeId,
  NodeLog,
  Port,
  PortsConfig (..),
  RunningNode,
  defaultCardanoNodeArgs,
  withCardanoNode,
 )
import Control.Tracer (Tracer, traceWith)
import Test.Network.Ports (randomUnusedTCPPorts)
import System.Directory (copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.Files (
  ownerReadMode,
  setFileMode,
 )

data RunningCluster = RunningCluster ClusterConfig [RunningNode]

-- | Configuration parameters for the cluster.
newtype ClusterConfig = ClusterConfig {parentStateDirectory :: FilePath}

withCluster ::
  Tracer IO ClusterLog -> ClusterConfig -> (RunningCluster -> IO ()) -> IO ()
withCluster tr cfg@ClusterConfig{parentStateDirectory} action = do
  systemStart <- initSystemStart
  (cfgA, cfgB, cfgC) <-
    makeNodesConfig parentStateDirectory systemStart
      <$> randomUnusedTCPPorts 3
  withBFTNode tr cfgA $ \nodeA -> do
    withBFTNode tr cfgB $ \nodeB -> do
      withBFTNode tr cfgC $ \nodeC -> do
        action (RunningCluster cfg [nodeA, nodeB, nodeC])

withBFTNode :: Tracer IO ClusterLog -> CardanoNodeConfig -> (RunningNode -> IO ()) -> IO ()
withBFTNode clusterTracer cfg action = do
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

  copyFile
    ("config" </> "genesis-shelley.json")
    (stateDirectory cfg </> nodeShelleyGenesisFile args)

  copyFile
    ("config" </> "genesis-alonzo.json")
    (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  withCardanoNode nodeTracer cfg args $ \rn -> do
    traceWith clusterTracer $ MsgNodeStarting cfg
    action rn
 where
  dlgCertFilename i = "delegation-cert.00" <> show (i - 1) <> ".json"
  signKeyFilename i = "delegate-keys.00" <> show (i - 1) <> ".key"
  vrfKeyFilename i = "delegate" <> show i <> ".vrf.skey"
  kesKeyFilename i = "delegate" <> show i <> ".kes.skey"
  opCertFilename i = "opcert" <> show i <> ".cert"

  copyCredential parentDir file = do
    let source = "config" </> "credentials" </> file
    let destination = parentDir </> file
    copyFile source destination
    setFileMode destination ownerReadMode
    pure destination

  nid = nodeId cfg

  nodeTracer = contramap (MsgFromNode nid) clusterTracer

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
  ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart $
      PortsConfig a [b, c]
  , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart $
      PortsConfig b [a, c]
  , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart $
      PortsConfig c [a, b]
  )
makeNodesConfig _ _ _ = error "we only support topology for 3 nodes"

--
-- Logging
--

data ClusterLog
  = MsgFromNode NodeId NodeLog
  | MsgNodeStarting CardanoNodeConfig
  deriving (Show)
