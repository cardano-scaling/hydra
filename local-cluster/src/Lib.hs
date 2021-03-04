module Lib where

import Cardano.Prelude

import Data.Time.Clock (
    UTCTime,
    addUTCTime,
    getCurrentTime,
 )
import Logging (
    HasSeverityAnnotation (..),
    Severity (..),
    Tracer,
 )
import Node (
    CardanoNodeArgs (..),
    CardanoNodeConfig (..),
    Port,
    PortsConfig (..),
    RunningNode,
    defaultCardanoNodeArgs,
    withCardanoNode,
 )
import Ports (
    randomUnusedTCPPorts,
 )
import System.Directory (
    copyFile,
    createDirectoryIfMissing,
 )
import System.FilePath (
    (</>),
 )

data RunningCluster = RunningCluster ClusterConfig [PortsConfig]

-- | Configuration parameters for the cluster.
data ClusterConfig = ClusterConfig
    { parentStateDirectory :: FilePath
    }

withCluster ::
    Tracer IO ClusterLog ->
    ClusterConfig ->
    (RunningCluster -> IO ()) ->
    IO ()
withCluster _tracer cfg@ClusterConfig{parentStateDirectory} action = do
    systemStart <- initSystemStart
    (cfgA, cfgB, cfgC) <- makeNodesConfig parentStateDirectory systemStart <$> randomUnusedTCPPorts 3
    withBFTNode cfgA $ \_ -> do
        withBFTNode cfgB $ \_ -> do
            withBFTNode cfgC $ \_ -> do
                action (RunningCluster cfg (fmap ports [cfgA, cfgB, cfgC]))

withBFTNode ::
    CardanoNodeConfig ->
    (RunningNode -> IO ()) ->
    IO ()
withBFTNode cfg action = do
    createDirectoryIfMissing False (stateDirectory cfg)

    [dlgCert, signKey, vrfKey, kesKey, opCert] <-
        forM
            [ dlgCertFilename (nodeId cfg)
            , signKeyFilename (nodeId cfg)
            , vrfKeyFilename (nodeId cfg)
            , kesKeyFilename (nodeId cfg)
            , opCertFilename (nodeId cfg)
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

    withCardanoNode cfg args action
  where
    dlgCertFilename id = "delegation-cert.00" <> show (id - 1) <> ".json"
    signKeyFilename id = "delegate-keys.00" <> show (id - 1) <> ".key"
    vrfKeyFilename id = "delegate" <> show id <> ".vrf.skey"
    kesKeyFilename id = "delegate" <> show id <> ".kes.skey"
    opCertFilename id = "opcert" <> show id <> ".cert"

    copyCredential parentDir file = do
        let source = "config" </> "credentials" </> file
        let destination = parentDir </> file
        destination <$ copyFile source destination

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart = do
    addUTCTime 1 <$> getCurrentTime

makeNodesConfig :: FilePath -> UTCTime -> [Port] -> (CardanoNodeConfig, CardanoNodeConfig, CardanoNodeConfig)
makeNodesConfig stateDirectory systemStart [a, b, c] =
    ( CardanoNodeConfig 1 (stateDirectory </> "node-1") systemStart $ PortsConfig a [b, c]
    , CardanoNodeConfig 2 (stateDirectory </> "node-2") systemStart $ PortsConfig b [a, c]
    , CardanoNodeConfig 3 (stateDirectory </> "node-3") systemStart $ PortsConfig c [a, b]
    )
makeNodesConfig _ _ _ = panic "we only support topology for 3 nodes"

--
-- Logging
--

data ClusterLog = ClusterLog deriving (Show)

instance HasSeverityAnnotation ClusterLog where
    getSeverityAnnotation = \case
        ClusterLog -> Info
