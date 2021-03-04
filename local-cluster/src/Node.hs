{-# LANGUAGE TypeApplications #-}

module Node where

import Cardano.Prelude

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    (.=),
 )
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (
    UTCTime,
 )
import Data.Time.Clock.POSIX (
    posixSecondsToUTCTime,
    utcTimeToPOSIXSeconds,
 )
import System.FilePath (
    (</>),
 )
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    proc,
    withCreateProcess,
 )
import qualified Prelude

type Port = Int

newtype RunningNode = RunningNode ProcessHandle

-- | Configuration parameters for a single node of the cluster
data CardanoNodeConfig = CardanoNodeConfig
    { -- | An identifier for the node
      nodeId :: Int
    , -- | Parent state directory in which create a state directory for the cluster
      stateDirectory :: FilePath
    , -- | Blockchain start time
      systemStart :: UTCTime
    , -- | A list of port
      ports :: PortsConfig
    }

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
    { nodeSocket :: FilePath
    , nodeConfigFile :: FilePath
    , nodeByronGenesisFile :: FilePath
    , nodeShelleyGenesisFile :: FilePath
    , nodeTopologyFile :: FilePath
    , nodeDatabaseDir :: FilePath
    , nodeDlgCertFile :: Maybe FilePath
    , nodeSignKeyFile :: Maybe FilePath
    , nodeOpCertFile :: Maybe FilePath
    , nodeKesKeyFile :: Maybe FilePath
    , nodeVrfKeyFile :: Maybe FilePath
    , nodePort :: Maybe Port
    }

defaultCardanoNodeArgs :: CardanoNodeArgs
defaultCardanoNodeArgs =
    CardanoNodeArgs
        { nodeSocket = "node.socket"
        , nodeConfigFile = "configuration.json"
        , nodeByronGenesisFile = "genesis-byron.json"
        , nodeShelleyGenesisFile = "genesis-shelley.json"
        , nodeTopologyFile = "topology.json"
        , nodeDatabaseDir = "db"
        , nodeDlgCertFile = Nothing
        , nodeSignKeyFile = Nothing
        , nodeOpCertFile = Nothing
        , nodeKesKeyFile = Nothing
        , nodeVrfKeyFile = Nothing
        , nodePort = Nothing
        }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully connected topology.
data PortsConfig = PortsConfig
    { -- | Our node TCP port.
      ours :: Port
    , -- | Other peers TCP ports.
      peers :: [Port]
    }

withCardanoNode :: CardanoNodeConfig -> CardanoNodeArgs -> (RunningNode -> IO a) -> IO a
withCardanoNode cfg args action = do
    generateEnvironment
    let process = cardanoNodeProcess (Just $ stateDirectory cfg) args
    print (cmdspec process)
    withCreateProcess process $ \_stdin _stdout _stderr h ->
        action (RunningNode h)
  where
    generateEnvironment = do
        refreshSystemStart cfg args
        let topology = mkTopology $ peers $ ports cfg
        Aeson.encodeFile (stateDirectory cfg </> nodeTopologyFile args) topology

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args =
    (proc "cardano-node" strArgs){cwd}
  where
    strArgs =
        "run" :
        mconcat
            [ ["--config", nodeConfigFile args]
            , ["--topology", nodeTopologyFile args]
            , ["--database-path", nodeDatabaseDir args]
            , ["--socket-path", nodeSocket args]
            , opt "--port" (show <$> nodePort args)
            , opt "--byron-signing-key" (nodeSignKeyFile args)
            , opt "--byron-delegation-certificate" (nodeDlgCertFile args)
            , opt "--shelley-operational-certificate" (nodeOpCertFile args)
            , opt "--shelley-kes-key" (nodeKesKeyFile args)
            , opt "--shelley-vrf-key" (nodeVrfKeyFile args)
            ]

    opt :: a -> Maybe a -> [a]
    opt arg = \case
        Nothing -> []
        Just val -> [arg, val]

-- | Re-generate configuration and genesis files with fresh system start times.
refreshSystemStart :: CardanoNodeConfig -> CardanoNodeArgs -> IO ()
refreshSystemStart cfg args = do
    let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart cfg
    byronGenesis <-
        unsafeDecodeJsonFile (stateDirectory cfg </> nodeByronGenesisFile args)
            <&> addField "startTime" startTime

    let systemStartUTC = posixSecondsToUTCTime . fromRational . toRational $ startTime
    shelleyGenesis <-
        unsafeDecodeJsonFile (stateDirectory cfg </> nodeShelleyGenesisFile args)
            <&> addField "systemStart" systemStartUTC

    config <-
        unsafeDecodeJsonFile (stateDirectory cfg </> nodeConfigFile args)
            >>= pure . addField "ByronGenesisFile" (nodeByronGenesisFile args)
            >>= pure . addField "ShelleyGenesisFile" (nodeShelleyGenesisFile args)

    Aeson.encodeFile (stateDirectory cfg </> nodeByronGenesisFile args) byronGenesis
    Aeson.encodeFile (stateDirectory cfg </> nodeShelleyGenesisFile args) shelleyGenesis
    Aeson.encodeFile (stateDirectory cfg </> nodeConfigFile args) config

-- | Generate a topology file from a list of peers.
mkTopology :: [Port] -> Aeson.Value
mkTopology peers = do
    Aeson.object ["Producers" .= map encodePeer peers]
  where
    encodePeer :: Int -> Aeson.Value
    encodePeer port =
        Aeson.object
            [ "addr" .= ("127.0.0.1" :: Prelude.String)
            , "port" .= port
            , "valency" .= (1 :: Int)
            ]

--
-- Helpers
--

addField :: ToJSON a => Text -> a -> Aeson.Value -> Aeson.Value
addField k v = withObject (HM.insert k (toJSON v))

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
withObject fn = \case
    Aeson.Object m -> Aeson.Object (fn m)
    x -> x

unsafeDecodeJsonFile :: FromJSON a => FilePath -> IO a
unsafeDecodeJsonFile = Aeson.eitherDecodeFileStrict >=> either Prelude.fail pure
