{-# LANGUAGE DuplicateRecordFields #-}

module CardanoNode where

import Hydra.Prelude

import Test.Network.Ports qualified as Ports

import Cardano.Slotting.Time (diffRelativeTime, getRelativeTime, toRelativeTime)
import CardanoClient (QueryPoint (QueryTip))
import Control.Lens ((?~), (^?!))
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (Value (String), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey, key, _Number)
import Data.Aeson.Types qualified as Aeson
import Data.Fixed (Centi)
import Data.List qualified as List
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as Vector
import Hydra.Cardano.Api (
  Coin,
  File (..),
  NetworkId,
  NetworkMagic (..),
  SocketPath,
  TxId (..),
  UTxO,
  getProgress,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.Backend (ChainBackend (..))
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Blockfrost (runBlockfrostBackend)
import Hydra.Chain.CardanoClient (computeBlockTime)
import Hydra.Chain.Direct (runDirectBackend)
import Hydra.Cluster.Faucet (FaucetLog, publishOrReuseHydraScripts)
import Hydra.Cluster.Fixture qualified as Fixture
import Hydra.Cluster.Mithril (MithrilLog, downloadLatestSnapshotTo)
import Hydra.Cluster.Options (Options)
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Options (BlockfrostOptions (..), ChainBackendOptions (..), DirectOptions (..), defaultBlockfrostOptions)
import Hydra.Options qualified as Options
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequestThrow)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesFileExist,
  getCurrentDirectory,
  removeFile,
 )
import System.Exit (ExitCode (..))
import System.FilePath (
  takeDirectory,
  (</>),
 )
import System.Posix (ownerReadMode, setFileMode)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe, UseHandle),
  proc,
  readProcess,
  withCreateProcess,
 )
import Test.Hydra.Prelude hiding (Blockfrost)
import Test.Hydra.Prelude qualified as TestPrelude

data HydraNodeLog
  = HydraNodeCommandSpec {cmd :: Text}
  | NodeStarted {nodeId :: Int}
  | SentMessage {nodeId :: Int, message :: Aeson.Value}
  | StartWaiting {nodeIds :: [Int], messages :: [Aeson.Value]}
  | ReceivedMessage {nodeId :: Int, message :: Aeson.Value}
  | EndWaiting {nodeId :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data EndToEndLog
  = ClusterOptions {options :: Options}
  | FromCardanoNode NodeLog
  | FromFaucet FaucetLog
  | FromHydraNode HydraNodeLog
  | FromMithril MithrilLog
  | StartingFunds {actor :: String, utxo :: UTxO}
  | RefueledFunds {actor :: String, refuelingAmount :: Coin, utxo :: UTxO}
  | RemainingFunds {actor :: String, utxo :: UTxO}
  | PublishedHydraScriptsAt {hydraScriptsTxId :: [TxId]}
  | UsingHydraScriptsAt {hydraScriptsTxId :: [TxId]}
  | CreatedKey {keyPath :: FilePath}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data NodeLog
  = MsgNodeCmdSpec {cmd :: Text}
  | MsgCLI [Text]
  | MsgCLIStatus Text Text
  | MsgCLIRetry Text
  | MsgCLIRetryResult Text Int
  | MsgNodeStarting {stateDirectory :: FilePath}
  | MsgSocketIsReady SocketPath
  | MsgSynchronizing {percentDone :: Centi, timeDifference :: NominalDiffTime, blockTime :: NominalDiffTime, tipTime :: NominalDiffTime, targetTime :: NominalDiffTime}
  | MsgQueryGenesisParametersFailed {err :: Text}
  | MsgPortBindRetry {failedPort :: Port, remainingAttempts :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

type Port = Int

newtype NodeId = NodeId Int
  deriving newtype (Eq, Show, Num)

-- | Configuration parameters for a single node devnet
data DevnetConfig = DevnetConfig
  { stateDirectory :: FilePath
  -- ^ Parent state directory
  , systemStart :: UTCTime
  -- ^ Blockchain start time
  , ports :: PortsConfig
  -- ^ A list of port
  }
  deriving stock (Eq, Show, Generic)

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: FilePath
  , nodeConfigFile :: FilePath
  , nodeByronGenesisFile :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile :: FilePath
  , nodeConwayGenesisFile :: FilePath
  , nodeDijkstraGenesisFile :: FilePath
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
    , nodeConfigFile = "cardano-node.json"
    , nodeByronGenesisFile = "genesis-byron.json"
    , nodeShelleyGenesisFile = "genesis-shelley.json"
    , nodeAlonzoGenesisFile = "genesis-alonzo.json"
    , nodeConwayGenesisFile = "genesis-conway.json"
    , nodeDijkstraGenesisFile = "genesis-dijkstra.json"
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
  { ours :: Port
  -- ^ Our node TCP port.
  , peers :: [Port]
  -- ^ Other peers TCP ports.
  }
  deriving stock (Show, Eq, Generic)

getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  readProcess "cardano-node" ["--version"] ""

-- | Tries to find an communicate with an existing cardano-node running in given
-- work directory. NOTE: This is using the default node socket name as defined
-- by 'defaultCardanoNodeArgs'.
findRunningCardanoNode :: Tracer IO NodeLog -> FilePath -> Fixture.KnownNetwork -> IO (Maybe (NominalDiffTime, DirectOptions))
findRunningCardanoNode tracer workDir knownNetwork = do
  findRunningCardanoNode' tracer knownNetworkId socketPath
 where
  knownNetworkId = Fixture.toNetworkId knownNetwork

  socketPath = File $ workDir </> nodeSocket

  CardanoNodeArgs{nodeSocket} = defaultCardanoNodeArgs

-- | Tries to find an communicate with an existing cardano-node running in given
-- network id and socket path.
findRunningCardanoNode' :: Tracer IO NodeLog -> NetworkId -> SocketPath -> IO (Maybe (NominalDiffTime, DirectOptions))
findRunningCardanoNode' tracer networkId nodeSocket = do
  let opts = DirectOptions{networkId, nodeSocket}
  try (runDirectBackend opts getBlockTime) >>= \case
    Left (e :: SomeException) ->
      traceWith tracer MsgQueryGenesisParametersFailed{err = show e} $> Nothing
    Right blockTime ->
      pure $ Just (blockTime, opts)

-- | Run a backend action using the given 'ChainBackendOptions'.
runBackend :: ChainBackendOptions -> (forall m. (ChainBackend m, MonadIO m, MonadThrow m, MonadCatch m) => m a) -> IO a
runBackend opts action = case opts of
  Options.Direct directOpts -> runDirectBackend directOpts action
  Options.Blockfrost blockfrostOpts -> runBlockfrostBackend blockfrostOpts action

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
--
-- 'randomUnusedTCPPorts' must close its sentinel socket before returning the
-- port, leaving a small window in which another process can grab the same
-- ephemeral port before cardano-node binds it (see the NOTE in
-- 'Test.Network.Ports'). When that happens cardano-node dies on startup with
-- @bind: Address already in use@. Detect that one specific failure and retry
-- with a freshly allocated port; everything else is rethrown unchanged.
withCardanoNodeDevnet ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (NominalDiffTime -> DirectOptions -> IO a) ->
  IO a
withCardanoNodeDevnet tracer stateDirectory action = do
  args <- setupCardanoDevnet stateDirectory
  go args portBindMaxAttempts
 where
  go _ 0 =
    fail "withCardanoNodeDevnet: exhausted retries waiting for a free P2P port"
  go args attemptsLeft = do
    -- Allocate a free port for the cardano-node P2P listener so concurrent
    -- devnets don't all try to bind 3001 (the cardano-node default).
    [p] <- Ports.randomUnusedTCPPorts 1
    let args' = args{nodePort = Just p}
    result <- try $ withCardanoNode tracer stateDirectory args' action
    case result of
      Right a -> pure a
      Left (e :: SomeException)
        | isPortInUse e -> do
            traceWith tracer MsgPortBindRetry{failedPort = p, remainingAttempts = attemptsLeft - 1}
            go args (attemptsLeft - 1)
        | otherwise -> throwIO e

  isPortInUse :: SomeException -> Bool
  isPortInUse e =
    let msg = displayException e
     in "Address already in use" `List.isInfixOf` msg
          || "bind: resource busy" `List.isInfixOf` msg

portBindMaxAttempts :: Int
portBindMaxAttempts = 5

withBlockfrostBackend ::
  Tracer IO EndToEndLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (NominalDiffTime -> ChainBackendOptions -> IO a) ->
  IO a
withBlockfrostBackend _tracer stateDirectory action = do
  args <- setupCardanoDevnet stateDirectory
  shelleyGenesis <- readFileBS >=> unsafeDecodeJson $ stateDirectory </> nodeShelleyGenesisFile args
  bfProjectPath <- findFileStartingAtDirectory 3 Backend.blockfrostProjectPath
  let opts = Options.Blockfrost defaultBlockfrostOptions{projectPath = bfProjectPath}
  -- We need to make sure somehow that, before we start our blockfrost tests,
  -- doing queries will give us updated information on some UTxO. There is no
  -- way to definitely know if this information is correct since it might be
  -- outdated. We just try to wait for sufficient amount of time before
  -- starting another BF related test.
  delay <- runBackend opts getQueryDelay
  threadDelay $ realToFrac delay
  action (getShelleyGenesisBlockTime shelleyGenesis) opts

-- | Find the given file in the current directory or its parents.
--
-- This function starts from the current working directory and checks if the specified file exists there.
-- If not found, it recursively checks the parent directories up to the given maximum depth.
findFileStartingAtDirectory :: Int -> FilePath -> IO FilePath
findFileStartingAtDirectory maxDepth fileName = do
  cwd <- getCurrentDirectory
  findInDir maxDepth cwd
 where
  findInDir :: Int -> FilePath -> IO FilePath
  findInDir depth dir = do
    let path = dir </> fileName
    exists <- doesFileExist path
    if exists
      then pure path
      else
        if depth <= 0
          then error $ "Could not locate the Blockfrost project file at " <> pack dir <> " or " <> show depth <> " above."
          else do
            let parent = ".." </> takeDirectory dir
            if parent == dir
              then error "Reached root directory without finding the Blockfrost project file."
              else findInDir (depth - 1) parent

withBackend ::
  forall a.
  Tracer IO EndToEndLog ->
  FilePath ->
  (NominalDiffTime -> ChainBackendOptions -> IO a) ->
  IO a
withBackend tracer stateDirectory action = do
  getHydraNetwork >>= \case
    LocalDevnet -> withCardanoNodeDevnet (contramap FromCardanoNode tracer) stateDirectory $ \bt opts -> action bt (Direct opts)
    Preview -> withNode Fixture.Preview action
    Preproduction -> withNode Fixture.Preproduction action
    Mainnet -> withNode Fixture.Mainnet action
    TestPrelude.Blockfrost -> withBlockfrostBackend tracer stateDirectory action
 where
  withNode network action' = do
    nodeDir <- fromMaybe stateDirectory <$> lookupEnv "HYDRA_WORK_DIR"
    createDirectoryIfMissing True nodeDir
    let syncAndRun blockTime opts = do
          waitForFullySynchronized (contramap FromCardanoNode tracer) (Direct opts)
          action' blockTime (Direct opts)
    findRunningCardanoNode (contramap FromCardanoNode tracer) nodeDir network >>= \case
      Just (blockTime, opts) ->
        syncAndRun blockTime opts
      Nothing -> do
        let dbDir = nodeDir </> "db"
        dbExists <- doesDirectoryExist dbDir
        unless dbExists $ do
          downloadLatestSnapshotTo (contramap FromMithril tracer) network nodeDir
        withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) nodeDir network syncAndRun

-- | Like 'withBackend', but also publishes (or reuses cached) Hydra scripts.
-- On public testnets the cache file is stored in the persistent 'HYDRA_WORK_DIR',
-- so scripts are published once and reused across test runs. On local devnet,
-- the per-test 'stateDirectory' is used, so scripts are always published fresh.
withHydraScriptsAndBackendRunning ::
  forall a.
  Tracer IO EndToEndLog ->
  FilePath ->
  (ChainBackendOptions -> [TxId] -> IO a) ->
  IO a
withHydraScriptsAndBackendRunning tracer stateDirectory action = do
  getHydraNetwork >>= \case
    LocalDevnet -> withCardanoNodeDevnet (contramap FromCardanoNode tracer) stateDirectory $ \_ opts -> do
      txIds <- publishOrReuseHydraScripts (Direct opts) Fixture.Faucet stateDirectory
      action (Direct opts) txIds
    Preview -> withPublicTestnetNode Fixture.Preview
    Preproduction -> withPublicTestnetNode Fixture.Preproduction
    Mainnet -> withPublicTestnetNode Fixture.Mainnet
    TestPrelude.Blockfrost -> withBlockfrostBackend tracer stateDirectory $ \_ opts -> do
      txIds <- publishOrReuseHydraScripts opts Fixture.Faucet stateDirectory
      action opts txIds
 where
  withPublicTestnetNode network = do
    nodeDir <- fromMaybe stateDirectory <$> lookupEnv "HYDRA_WORK_DIR"
    createDirectoryIfMissing True nodeDir
    let syncPublishAndRun _ opts = do
          waitForFullySynchronized (contramap FromCardanoNode tracer) (Direct opts)
          txIds <- publishOrReuseHydraScripts (Direct opts) Fixture.Faucet nodeDir
          action (Direct opts) txIds
    findRunningCardanoNode (contramap FromCardanoNode tracer) nodeDir network >>= \case
      Just (blockTime, opts) ->
        syncPublishAndRun blockTime opts
      Nothing -> do
        let dbDir = nodeDir </> "db"
        dbExists <- doesDirectoryExist dbDir
        unless dbExists $ do
          downloadLatestSnapshotTo (contramap FromMithril tracer) network nodeDir
        withCardanoNodeOnKnownNetwork (contramap FromCardanoNode tracer) nodeDir network syncPublishAndRun

-- | Run a cardano-node as normal network participant on a known network.
withCardanoNodeOnKnownNetwork ::
  Tracer IO NodeLog ->
  -- | State directory in which node db & logs are persisted.
  FilePath ->
  -- | A well-known Cardano network to connect to.
  Fixture.KnownNetwork ->
  (NominalDiffTime -> DirectOptions -> IO a) ->
  IO a
withCardanoNodeOnKnownNetwork tracer stateDirectory knownNetwork action = do
  copyKnownNetworkFiles
  withCardanoNode tracer stateDirectory args action
 where
  args =
    defaultCardanoNodeArgs
      { nodeConfigFile = "config.json"
      , nodeTopologyFile = "topology.json"
      , nodeByronGenesisFile = "byron-genesis.json"
      , nodeShelleyGenesisFile = "shelley-genesis.json"
      , nodeAlonzoGenesisFile = "alonzo-genesis.json"
      , nodeConwayGenesisFile = "conway-genesis.json"
      , nodeDijkstraGenesisFile = "dijkstra-genesis.json"
      }

  -- Copy/download configuration files for a known network
  copyKnownNetworkFiles = do
    forM_
      [ "config.json"
      , "topology.json"
      , "byron-genesis.json"
      , "shelley-genesis.json"
      , "alonzo-genesis.json"
      , "conway-genesis.json"
      , "peer-snapshot.json"
      ]
      $ \fn -> do
        createDirectoryIfMissing True $ stateDirectory </> takeDirectory fn
        fetchConfigFile (knownNetworkPath </> fn)
          >>= writeFileBS (stateDirectory </> fn)
    when (knownNetwork `elem` [Fixture.Mainnet, Fixture.Preview]) $ do
      forM_ ["checkpoints.json"] $
        \fn -> do
          createDirectoryIfMissing True $ stateDirectory </> takeDirectory fn
          fetchConfigFile (knownNetworkPath </> fn)
            >>= writeFileBS (stateDirectory </> fn)

  knownNetworkPath =
    knownNetworkConfigBaseURL </> knownNetworkName

  -- Base path on remote
  knownNetworkConfigBaseURL = "https://book.world.dev.cardano.org"

  -- Network name on remote
  knownNetworkName = case knownNetwork of
    Fixture.Preview -> "environments/preview"
    Fixture.Preproduction -> "environments/preprod"
    Fixture.Mainnet -> "environments/mainnet"
    -- NOTE: Here we map blockfrost networks to cardano ones since we expect to find actor keys
    -- in known locations when running smoke-tests.
    Fixture.BlockfrostPreview -> "environments/preview"
    Fixture.BlockfrostPreprod -> "environments/preprod"
    Fixture.BlockfrostMainnet -> "environments/mainnet"

  fetchConfigFile :: String -> IO ByteString
  fetchConfigFile path =
    parseRequestThrow path >>= httpBS <&> getResponseBody

-- | Setup the cardano-node to run a local devnet producing blocks. This copies
-- the appropriate files and prepares 'CardanoNodeArgs' for 'withCardanoNode'.
setupCardanoDevnet :: FilePath -> IO CardanoNodeArgs
setupCardanoDevnet stateDirectory = do
  createDirectoryIfMissing True stateDirectory
  [dlgCert, signKey, vrfKey, kesKey, opCert] <-
    mapM
      copyDevnetCredential
      [ "byron-delegation.cert"
      , "byron-delegate.key"
      , "vrf.skey"
      , "kes.skey"
      , "opcert.cert"
      ]
  let args =
        defaultCardanoNodeArgs
          { nodeDlgCertFile = Just dlgCert
          , nodeSignKeyFile = Just signKey
          , nodeVrfKeyFile = Just vrfKey
          , nodeKesKeyFile = Just kesKey
          , nodeOpCertFile = Just opCert
          }
  copyDevnetFiles args
  refreshSystemStart stateDirectory args
  writeTopology [] args
  pure args
 where
  copyDevnetCredential file = do
    let destination = stateDirectory </> file
    unlessM (doesFileExist destination) $
      readConfigFile ("devnet" </> file)
        >>= writeFileBS destination
    setFileMode destination ownerReadMode
    pure file

  copyDevnetFiles args = do
    readConfigFile ("devnet" </> "cardano-node.json")
      >>= writeFileBS
        (stateDirectory </> nodeConfigFile args)
    readConfigFile ("devnet" </> "genesis-byron.json")
      >>= writeFileBS
        (stateDirectory </> nodeByronGenesisFile args)
    readConfigFile ("devnet" </> "genesis-shelley.json")
      >>= writeFileBS
        (stateDirectory </> nodeShelleyGenesisFile args)
    readConfigFile ("devnet" </> "genesis-alonzo.json")
      >>= writeFileBS
        (stateDirectory </> nodeAlonzoGenesisFile args)
    readConfigFile ("devnet" </> "genesis-conway.json")
      >>= writeFileBS
        (stateDirectory </> nodeConwayGenesisFile args)
    readConfigFile ("devnet" </> "genesis-dijkstra.json")
      >>= writeFileBS
        (stateDirectory </> nodeDijkstraGenesisFile args)

  writeTopology peers args =
    Aeson.encodeFile (stateDirectory </> nodeTopologyFile args) $
      mkTopology peers

withCardanoNode ::
  Tracer IO NodeLog ->
  FilePath ->
  CardanoNodeArgs ->
  (NominalDiffTime -> DirectOptions -> IO a) ->
  IO a
withCardanoNode tr stateDirectory args action = do
  traceWith tr $ MsgNodeCmdSpec (show $ cmdspec process)
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = CreatePipe, close_fds = True} $
      \_stdin _stdout mError processHandle ->
        (`finally` cleanupSocketFile) $
          raceLabelled
            ("check-cardano-node-process-not-died", checkProcessHasNotDied "cardano-node" processHandle mError)
            ("wait-for-node", waitForNode)
            <&> either absurd id
 where
  CardanoNodeArgs{nodeSocket} = args

  process = cardanoNodeProcess (Just stateDirectory) args

  logFilePath = stateDirectory </> "logs" </> "cardano-node.log"

  socketPath = stateDirectory </> nodeSocket

  waitForNode = do
    let nodeSocketPath = File socketPath
    traceWith tr $ MsgNodeStarting{stateDirectory}
    waitForSocket nodeSocketPath
    traceWith tr $ MsgSocketIsReady nodeSocketPath
    shelleyGenesis <- readShelleyGenesisJSON $ stateDirectory </> nodeShelleyGenesisFile args
    action (getShelleyGenesisBlockTime shelleyGenesis) DirectOptions{networkId = getShelleyGenesisNetworkId shelleyGenesis, nodeSocket = File (stateDirectory </> nodeSocket)}

  cleanupSocketFile =
    whenM (doesFileExist socketPath) $
      removeFile socketPath

  readShelleyGenesisJSON = readFileBS >=> unsafeDecodeJson

  -- Read 'NetworkId' from shelley genesis JSON file
  getShelleyGenesisNetworkId :: Value -> NetworkId
  getShelleyGenesisNetworkId json = do
    if json ^?! key "networkId" == "Mainnet"
      then Api.Mainnet
      else do
        let magic = json ^?! key "networkMagic" . _Number
        Api.Testnet (Api.NetworkMagic $ truncate magic)

-- Read expected time between blocks from shelley genesis
getShelleyGenesisBlockTime :: Value -> NominalDiffTime
getShelleyGenesisBlockTime json = do
  let slotLength = json ^?! key "slotLength" . _Number
  let activeSlotsCoeff = json ^?! key "activeSlotsCoeff" . _Number
  computeBlockTime (realToFrac slotLength) (toRational activeSlotsCoeff)

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  Tracer IO NodeLog ->
  ChainBackendOptions ->
  IO ()
waitForFullySynchronized tracer opts = do
  systemStart <- runBackend opts $ querySystemStart QueryTip
  check systemStart
 where
  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- runBackend opts $ queryEraHistory QueryTip
    tipSlotNo <- fromMaybe 0 . Api.chainPointToSlotNo <$> runBackend opts queryTip
    (tipTime, _slotLength) <- either throwIO pure $ getProgress tipSlotNo eraHistory
    let timeDifference = diffRelativeTime targetTime tipTime
    let percentDone = realToFrac (100.0 * getRelativeTime tipTime / getRelativeTime targetTime)
    blockTime <- runBackend opts getBlockTime
    traceWith tracer $ MsgSynchronizing{percentDone, blockTime, tipTime = getRelativeTime tipTime, targetTime = getRelativeTime targetTime, timeDifference}
    if timeDifference < 20 * blockTime
      then pure ()
      else threadDelay 3 >> check systemStart

-- | Wait for the node socket file to become available.
waitForSocket :: SocketPath -> IO ()
waitForSocket socketPath =
  unlessM (doesFileExist $ unFile socketPath) $ do
    threadDelay 0.1
    waitForSocket socketPath

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
    "run"
      : mconcat
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
refreshSystemStart ::
  -- | Working directory in which paths of 'CardanoNodeArgs' are resolved.
  FilePath ->
  CardanoNodeArgs ->
  IO ()
refreshSystemStart stateDirectory args = do
  systemStart <- initSystemStart
  let startTime = round @_ @Int $ utcTimeToPOSIXSeconds systemStart
  byronGenesis <-
    unsafeDecodeJsonFile @Aeson.Value (stateDirectory </> nodeByronGenesisFile args)
      <&> atKey "startTime" ?~ toJSON startTime

  let systemStartUTC =
        posixSecondsToUTCTime . fromRational . toRational $ startTime
  shelleyGenesis <-
    unsafeDecodeJsonFile @Aeson.Value (stateDirectory </> nodeShelleyGenesisFile args)
      <&> atKey "systemStart" ?~ toJSON systemStartUTC

  config <-
    unsafeDecodeJsonFile @Aeson.Value (stateDirectory </> nodeConfigFile args)
      <&> (atKey "ByronGenesisFile" ?~ toJSON (Text.pack $ nodeByronGenesisFile args))
        . (atKey "ShelleyGenesisFile" ?~ String (Text.pack $ nodeShelleyGenesisFile args))

  Aeson.encodeFile
    (stateDirectory </> nodeByronGenesisFile args)
    byronGenesis
  Aeson.encodeFile
    (stateDirectory </> nodeShelleyGenesisFile args)
    shelleyGenesis
  Aeson.encodeFile (stateDirectory </> nodeConfigFile args) config

-- | Generate a topology file from a list of peers.
mkTopology :: [Port] -> Aeson.Value
mkTopology peers =
  let bootstrapPeers = map encodePeer peers
   in Aeson.object
        [ "bootstrapPeers"
            .= if null bootstrapPeers
              then Nothing
              else Just bootstrapPeers
        , "localRoots"
            .= Aeson.Array
              ( Vector.fromList
                  [ Aeson.object
                      [ "accessPoints" .= Aeson.emptyArray
                      , "advertise" .= False
                      , "trustable" .= False
                      , "valency" .= (1 :: Natural)
                      ]
                  ]
              )
        , "publicRoots"
            .= Aeson.Array
              ( Vector.fromList
                  [ Aeson.object
                      [ "accessPoints" .= Aeson.emptyArray
                      , "advertise" .= False
                      ]
                  ]
              )
        , "peerSnapshotFile" .= Aeson.Null
        ]
 where
  encodePeer :: Int -> Aeson.Value
  encodePeer port =
    Aeson.object
      ["addr" .= ("127.0.0.1" :: Text), "port" .= port, "valency" .= (1 :: Int)]

data ProcessHasExited = ProcessHasExited Text ExitCode
  deriving stock (Show)

-- | Cardano-cli wrapper to query protocol parameters. While we have also client
-- functions in Hydra.Chain.CardanoClient and Hydra.Cluster.CardanoClient,
-- sometimes we deliberately want to use the cardano-cli to ensure
-- compatibility.
cliQueryProtocolParameters :: SocketPath -> NetworkId -> IO Value
cliQueryProtocolParameters nodeSocket networkId = do
  out <- readProcess cmd args ""
  unsafeDecodeJson $ fromString out
 where
  cmd = "cardano-cli"

  args =
    [ "conway"
    , "query"
    , "protocol-parameters"
    , "--socket-path"
    , unFile nodeSocket
    ]
      <> case networkId of
        Api.Mainnet -> ["--mainnet"]
        Api.Testnet (NetworkMagic magic) -> ["--testnet-magic", show magic]
      <> [ "--out-file"
         , "/dev/stdout"
         ]

--
-- Helpers
--

unsafeDecodeJson :: FromJSON a => ByteString -> IO a
unsafeDecodeJson = either fail pure . Aeson.eitherDecodeStrict

unsafeDecodeJsonFile :: FromJSON a => FilePath -> IO a
unsafeDecodeJsonFile = Aeson.eitherDecodeFileStrict >=> either fail pure
