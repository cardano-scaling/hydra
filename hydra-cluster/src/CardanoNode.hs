{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module CardanoNode where

import Hydra.Prelude

import Cardano.Slotting.Time (RelativeTime (getRelativeTime), diffRelativeTime, toRelativeTime)
import CardanoClient (QueryPoint (QueryTip), queryEraHistory, querySystemStart, queryTipSlotNo)
import Control.Lens ((^?!))
import Control.Tracer (Tracer, traceWith)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Lens (key, _Number)
import Data.Fixed (Centi)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Hydra.Cardano.Api (AsType (AsPaymentKey), NetworkId, PaymentKey, SigningKey, VerificationKey, generateSigningKey, getProgress, getVerificationKey)
import qualified Hydra.Cardano.Api as Api
import Hydra.Cluster.Fixture (KnownNetwork (Testnet, VasilTestnet), defaultNetworkId, knownNetworkId)
import Hydra.Cluster.Util (readConfigFile)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.Posix (ownerReadMode, setFileMode)
import System.Process (
  CreateProcess (..),
  StdStream (UseHandle),
  proc,
  readProcess,
  withCreateProcess,
 )
import Test.Hydra.Prelude
import Test.Network.Ports (randomUnusedTCPPort)

type Port = Int

newtype NodeId = NodeId Int
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)

data RunningNode = RunningNode
  { nodeId :: NodeId
  , nodeSocket :: FilePath
  , networkId :: NetworkId
  }

-- | Configuration parameters for a single node of the cluster
-- TODO: this is only really used by `withCardanoNodeDevnet` -> rename?
data CardanoNodeConfig = CardanoNodeConfig
  { -- | An identifier for the node
    nodeId :: NodeId
  , -- | Parent state directory in which create a state directory for the cluster
    stateDirectory :: FilePath
  , -- | Blockchain start time
    systemStart :: UTCTime
  , -- | A list of port
    ports :: PortsConfig
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newNodeConfig :: FilePath -> IO CardanoNodeConfig
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

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: FilePath
  , nodeConfigFile :: FilePath
  , nodeByronGenesisFile :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile :: FilePath
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
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
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
-- fully sockected topology.
data PortsConfig = PortsConfig
  { -- | Our node TCP port.
    ours :: Port
  , -- | Other peers TCP ports.
    peers :: [Port]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  readProcess "cardano-node" ["--version"] ""

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnet ::
  Tracer IO NodeLog ->
  CardanoNodeConfig ->
  (RunningNode -> IO ()) ->
  IO ()
withCardanoNodeDevnet tracer cfg action = do
  createDirectoryIfMissing True (stateDirectory cfg </> dirname)

  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    forM
      [ dlgCertFilename
      , signKeyFilename
      , vrfKeyFilename
      , kesKeyFilename
      , opCertFilename
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

  readConfigFile "cardano-node.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeConfigFile args)

  readConfigFile "genesis-byron.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeByronGenesisFile args)

  readConfigFile "genesis-shelley.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeShelleyGenesisFile args)

  readConfigFile "genesis-alonzo.json"
    >>= writeFileBS
      (stateDirectory cfg </> nodeAlonzoGenesisFile args)

  generateEnvironment args

  withCardanoNode tracer networkId cfg args $ \rn -> do
    traceWith tracer MsgNodeIsReady
    action rn
 where
  CardanoNodeConfig{nodeId} = cfg

  dirname = "stake-pool-" <> show nodeId

  -- NOTE: This needs to match what's in config/genesis-shelley.json
  networkId = defaultNetworkId

  dlgCertFilename =
    dirname </> "byron-delegation.cert"
  signKeyFilename =
    dirname </> "byron-delegate.key"
  vrfKeyFilename =
    dirname </> "vrf.skey"
  kesKeyFilename =
    dirname </> "kes.skey"
  opCertFilename =
    dirname </> "opcert.cert"

  copyCredential parentDir file = do
    bs <- readConfigFile ("credentials" </> file)
    let destination = parentDir </> file
    unlessM (doesFileExist destination) $
      writeFileBS destination bs
    setFileMode destination ownerReadMode
    pure destination

  generateEnvironment args = do
    refreshSystemStart cfg args
    let topology = mkTopology $ peers $ ports cfg
    Aeson.encodeFile (stateDirectory cfg </> nodeTopologyFile args) topology

-- | Run a cardano-node as normal network participant on a known network.
withCardanoNodeOnKnownNetwork ::
  Tracer IO NodeLog ->
  FilePath ->
  KnownNetwork ->
  (RunningNode -> IO ()) ->
  IO ()
withCardanoNodeOnKnownNetwork tracer workDir knownNetwork action = do
  networkId <- readNetworkId
  config <- newNodeConfig workDir
  copyKnownNetworkFiles
  withCardanoNode tracer networkId config args $ \node -> do
    waitForFullySynchronized tracer knownNetwork node
    traceWith tracer MsgNodeIsReady
    action node
 where
  args =
    defaultCardanoNodeArgs
      { nodeConfigFile = "cardano-node/config.json"
      , nodeTopologyFile = "cardano-node/topology.json"
      , nodeByronGenesisFile = "genesis/byron.json"
      , nodeShelleyGenesisFile = "genesis/shelley.json"
      , nodeAlonzoGenesisFile = "genesis/alonzo.json"
      }

  -- Read 'NetworkId' from shelley genesis, failing if on mainnet or not able to
  -- find the network magic.
  readNetworkId = do
    shelleyGenesis :: Aeson.Value <- unsafeDecodeJson =<< readConfigFile (knownNetworkPath </> "genesis" </> "shelley.json")
    when (shelleyGenesis ^?! key "networkId" == "Mainnet") $
      fail "Mainnet not supported yet"
    let magic = shelleyGenesis ^?! key "networkMagic" . _Number
    pure $ Api.Testnet (Api.NetworkMagic $ truncate magic)

  copyKnownNetworkFiles = do
    createDirectoryIfMissing True $ workDir </> "cardano-node"
    readConfigFile (knownNetworkPath </> "cardano-node" </> "config.json")
      >>= writeFileBS (workDir </> "cardano-node" </> "config.json")
    readConfigFile (knownNetworkPath </> "cardano-node" </> "topology.json")
      >>= writeFileBS (workDir </> "cardano-node" </> "topology.json")
    createDirectoryIfMissing True $ workDir </> "genesis"
    readConfigFile (knownNetworkPath </> "genesis" </> "byron.json")
      >>= writeFileBS (workDir </> "genesis" </> "byron.json")
    readConfigFile (knownNetworkPath </> "genesis" </> "shelley.json")
      >>= writeFileBS (workDir </> "genesis" </> "shelley.json")
    readConfigFile (knownNetworkPath </> "genesis" </> "alonzo.json")
      >>= writeFileBS (workDir </> "genesis" </> "alonzo.json")

  -- Folder name in config/cardano-configurations/network
  knownNetworkName = case knownNetwork of
    Testnet -> "testnet"
    VasilTestnet -> "vasil-dev"

  knownNetworkPath =
    "cardano-configurations" </> "network" </> knownNetworkName

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  Tracer IO NodeLog ->
  KnownNetwork ->
  RunningNode ->
  IO ()
waitForFullySynchronized tracer knownNetwork RunningNode{nodeSocket} = do
  systemStart <- querySystemStart networkId nodeSocket QueryTip
  check systemStart
 where
  networkId = knownNetworkId knownNetwork

  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- queryEraHistory networkId nodeSocket QueryTip
    tipSlotNo <- queryTipSlotNo networkId nodeSocket
    (tipTime, _slotLength) <- either throwIO pure $ getProgress tipSlotNo eraHistory
    let timeDifference = diffRelativeTime targetTime tipTime
    let percentDone = realToFrac (100.0 * getRelativeTime tipTime / getRelativeTime targetTime)
    traceWith tracer $ MsgSynchronizing{percentDone}
    if timeDifference < 20 -- TODO: derive from known network and block times
      then pure ()
      else threadDelay 3 >> check systemStart

withCardanoNode ::
  Tracer IO NodeLog ->
  NetworkId ->
  CardanoNodeConfig ->
  CardanoNodeArgs ->
  (RunningNode -> IO ()) ->
  IO ()
withCardanoNode tr networkId config@CardanoNodeConfig{stateDirectory, nodeId} args@CardanoNodeArgs{nodeSocket} action = do
  traceWith tr $ MsgUsingStateDirectory stateDirectory
  traceWith tr $ MsgNodeCmdSpec (show $ cmdspec process)
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \_stdin _stdout _stderr processHandle ->
        race_
          (checkProcessHasNotDied ("cardano-node-" <> show nodeId) processHandle)
          waitForNode
          `finally` cleanupSocketFile
 where
  process = cardanoNodeProcess (Just stateDirectory) args

  logFilePath = stateDirectory </> "logs" </> "cardano-node-" <> show nodeId <.> "log"

  socketPath = stateDirectory </> nodeSocket

  waitForNode = do
    let rn = RunningNode{nodeId, nodeSocket = socketPath, networkId}
    traceWith tr $ MsgNodeStarting config
    waitForSocket rn
    traceWith tr $ MsgSocketIsReady socketPath
    action rn

  cleanupSocketFile =
    whenM (doesFileExist socketPath) $
      removeFile socketPath

-- | Wait for the node socket file to become available.
waitForSocket :: RunningNode -> IO ()
waitForSocket node@RunningNode{nodeSocket} =
  unlessM (doesFileExist nodeSocket) $ do
    threadDelay 0.1
    waitForSocket node

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args =
  (proc "cardano-node" strArgs){cwd}
 where
  CardanoNodeArgs
    { nodeConfigFile
    , nodeTopologyFile
    , nodeDatabaseDir
    , nodeSocket
    , nodePort
    , nodeSignKeyFile
    , nodeDlgCertFile
    , nodeOpCertFile
    , nodeKesKeyFile
    , nodeVrfKeyFile
    } = args

  strArgs =
    "run" :
    mconcat
      [ ["--config", nodeConfigFile]
      , ["--topology", nodeTopologyFile]
      , ["--database-path", nodeDatabaseDir]
      , ["--socket-path", nodeSocket]
      , opt "--port" (show <$> nodePort)
      , opt "--byron-signing-key" nodeSignKeyFile
      , opt "--byron-delegation-certificate" nodeDlgCertFile
      , opt "--shelley-operational-certificate" nodeOpCertFile
      , opt "--shelley-kes-key" nodeKesKeyFile
      , opt "--shelley-vrf-key" nodeVrfKeyFile
      ]

  opt :: a -> Maybe a -> [a]
  opt arg = \case
    Nothing -> []
    Just val -> [arg, val]

-- | Initialize the system start time to now (modulo a small offset needed to
-- give time to the system to bootstrap correctly).
initSystemStart :: IO UTCTime
initSystemStart =
  addUTCTime 1 <$> getCurrentTime

-- | Re-generate configuration and genesis files with fresh system start times.
refreshSystemStart :: CardanoNodeConfig -> CardanoNodeArgs -> IO ()
refreshSystemStart cfg args = do
  let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart cfg
  byronGenesis <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeByronGenesisFile args)
      <&> addField "startTime" startTime

  let systemStartUTC =
        posixSecondsToUTCTime . fromRational . toRational $ startTime
  shelleyGenesis <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeShelleyGenesisFile args)
      <&> addField "systemStart" systemStartUTC

  config <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeConfigFile args)
      <&> addField "ByronGenesisFile" (nodeByronGenesisFile args)
      <&> addField "ShelleyGenesisFile" (nodeShelleyGenesisFile args)

  Aeson.encodeFile
    (stateDirectory cfg </> nodeByronGenesisFile args)
    byronGenesis
  Aeson.encodeFile
    (stateDirectory cfg </> nodeShelleyGenesisFile args)
    shelleyGenesis
  Aeson.encodeFile (stateDirectory cfg </> nodeConfigFile args) config

-- | Generate a topology file from a list of peers.
mkTopology :: [Port] -> Aeson.Value
mkTopology peers =
  Aeson.object ["Producers" .= map encodePeer peers]
 where
  encodePeer :: Int -> Aeson.Value
  encodePeer port =
    Aeson.object
      ["addr" .= ("127.0.0.1" :: Text), "port" .= port, "valency" .= (1 :: Int)]

generateCardanoKey :: IO (VerificationKey PaymentKey, SigningKey PaymentKey)
generateCardanoKey = do
  sk <- generateSigningKey AsPaymentKey
  pure (getVerificationKey sk, sk)

data ProcessHasExited = ProcessHasExited Text ExitCode
  deriving (Show)

instance Exception ProcessHasExited

-- Logging

data NodeLog
  = MsgUsingStateDirectory FilePath
  | MsgNodeCmdSpec Text
  | MsgCLI [Text]
  | MsgCLIStatus Text Text
  | MsgCLIRetry Text
  | MsgCLIRetryResult Text Int
  | MsgNodeStarting CardanoNodeConfig
  | MsgSocketIsReady FilePath
  | MsgSynchronizing {percentDone :: Centi}
  | MsgNodeIsReady
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--
-- Helpers
--

addField :: ToJSON a => Aeson.Key -> a -> Aeson.Value -> Aeson.Value
addField k v = withObject (Aeson.KeyMap.insert k (toJSON v))

-- | Do something with an a JSON object. Fails if the given JSON value isn't an
-- object.
withObject :: (Aeson.Object -> Aeson.Object) -> Aeson.Value -> Aeson.Value
withObject fn = \case
  Aeson.Object m -> Aeson.Object (fn m)
  x -> x

unsafeDecodeJson :: FromJSON a => ByteString -> IO a
unsafeDecodeJson = either fail pure . Aeson.eitherDecodeStrict

unsafeDecodeJsonFile :: FromJSON a => FilePath -> IO a
unsafeDecodeJsonFile = Aeson.eitherDecodeFileStrict >=> either fail pure
