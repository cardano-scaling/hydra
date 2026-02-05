{-# LANGUAGE DuplicateRecordFields #-}

module CardanoNode where

import Hydra.Prelude

import Cardano.Slotting.Time (diffRelativeTime, getRelativeTime, toRelativeTime)
import CardanoClient (QueryPoint (QueryTip))
import Control.Lens ((?~), (^?!))
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (Value (String), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey, key, _Number)
import Data.Aeson.Types qualified as Aeson
import Data.Fixed (Centi)
import Data.Text (pack)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Vector qualified as Vector
import Hydra.Cardano.Api (
  File (..),
  NetworkId,
  NetworkMagic (..),
  SocketPath,
  getProgress,
 )
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.Backend (ChainBackend)
import Hydra.Chain.Backend qualified as Backend
import Hydra.Chain.Blockfrost (BlockfrostBackend (..))
import Hydra.Chain.Direct (DirectBackend (..))
import Hydra.Cluster.Faucet (delayBF)
import Hydra.Cluster.Fixture (KnownNetwork (..), toNetworkId)
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Options (BlockfrostOptions (..), DirectOptions (..), defaultBlockfrostOptions)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequestThrow)
import System.Directory (
  createDirectoryIfMissing,
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
import Test.Hydra.Prelude

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
findRunningCardanoNode :: Tracer IO NodeLog -> FilePath -> KnownNetwork -> IO (Maybe (NominalDiffTime, DirectBackend))
findRunningCardanoNode tracer workDir knownNetwork = do
  findRunningCardanoNode' tracer knownNetworkId socketPath
 where
  knownNetworkId = toNetworkId knownNetwork

  socketPath = File $ workDir </> nodeSocket

  CardanoNodeArgs{nodeSocket} = defaultCardanoNodeArgs

-- | Tries to find an communicate with an existing cardano-node running in given
-- network id and socket path.
findRunningCardanoNode' :: Tracer IO NodeLog -> NetworkId -> SocketPath -> IO (Maybe (NominalDiffTime, DirectBackend))
findRunningCardanoNode' tracer networkId nodeSocket = do
  let backend = DirectBackend $ DirectOptions{networkId, nodeSocket}
  try (Backend.getBlockTime backend) >>= \case
    Left (e :: SomeException) ->
      traceWith tracer MsgQueryGenesisParametersFailed{err = show e} $> Nothing
    Right blockTime ->
      pure $ Just (blockTime, backend)

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnet ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (NominalDiffTime -> DirectBackend -> IO a) ->
  IO a
withCardanoNodeDevnet tracer stateDirectory action = do
  args <- setupCardanoDevnet stateDirectory
  withCardanoNode tracer stateDirectory args action

withBlockfrostBackend ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (NominalDiffTime -> BlockfrostBackend -> IO a) ->
  IO a
withBlockfrostBackend _tracer stateDirectory action = do
  args <- setupCardanoDevnet stateDirectory
  shelleyGenesis <- readFileBS >=> unsafeDecodeJson $ stateDirectory </> nodeShelleyGenesisFile args
  bfProjectPath <- findFileStartingAtDirectory 3 Backend.blockfrostProjectPath
  let backend = BlockfrostBackend $ defaultBlockfrostOptions{projectPath = bfProjectPath}
  -- We need to make sure somehow that, before we start our blockfrost tests,
  -- doing queries will give us updated information on some UTxO. There is no
  -- way to definitely know if this information is correct since it might be
  -- outdated. We just try to wait for sufficient amount of time before
  -- starting another BF related test.
  delayBF backend
  action (getShelleyGenesisBlockTime shelleyGenesis) backend

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
  Tracer IO NodeLog ->
  FilePath ->
  (forall backend. ChainBackend backend => NominalDiffTime -> backend -> IO a) ->
  IO a
withBackend tracer stateDirectory action = do
  getHydraTestnet >>= \case
    LocalDevnet -> withCardanoNodeDevnet tracer stateDirectory action
    PreviewTestnet -> withCardanoNodeOnKnownNetwork tracer stateDirectory Preview action
    PreproductionTestnet -> withCardanoNodeOnKnownNetwork tracer stateDirectory Preproduction action
    MainnetTesting -> withCardanoNodeOnKnownNetwork tracer stateDirectory Mainnet action
    BlockfrostTesting -> withBlockfrostBackend tracer stateDirectory action

-- | Run a cardano-node as normal network participant on a known network.
withCardanoNodeOnKnownNetwork ::
  Tracer IO NodeLog ->
  -- | State directory in which node db & logs are persisted.
  FilePath ->
  -- | A well-known Cardano network to connect to.
  KnownNetwork ->
  (NominalDiffTime -> DirectBackend -> IO a) ->
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
    when (knownNetwork `elem` [Mainnet, Preview]) $ do
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
    Preview -> "environments-pre/preview"
    Preproduction -> "environments-pre/preprod"
    Mainnet -> "environments/mainnet"
    -- NOTE: Here we map blockfrost networks to cardano ones since we expect to find actor keys
    -- in known locations when running smoke-tests.
    BlockfrostPreview -> "environments-pre/preview"
    BlockfrostPreprod -> "environments-pre/preprod"
    BlockfrostMainnet -> "environments/mainnet"

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
  (NominalDiffTime -> DirectBackend -> IO a) ->
  IO a
withCardanoNode tr stateDirectory args action = do
  traceWith tr $ MsgNodeCmdSpec (show $ cmdspec process)
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = CreatePipe} $
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
    action (getShelleyGenesisBlockTime shelleyGenesis) (DirectBackend $ DirectOptions{networkId = getShelleyGenesisNetworkId shelleyGenesis, nodeSocket = File (stateDirectory </> nodeSocket)})

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

-- | Compute the block time (expected time between blocks) given a slot length
-- as diff time and active slot coefficient.
computeBlockTime :: NominalDiffTime -> Rational -> NominalDiffTime
computeBlockTime slotLength activeSlotsCoeff =
  slotLength / realToFrac activeSlotsCoeff

-- | Wait until the node is fully caught up with the network. This can take a
-- while!
waitForFullySynchronized ::
  ChainBackend backend =>
  Tracer IO NodeLog ->
  backend ->
  IO ()
waitForFullySynchronized tracer backend = do
  systemStart <- Backend.querySystemStart backend QueryTip
  check systemStart
 where
  check systemStart = do
    targetTime <- toRelativeTime systemStart <$> getCurrentTime
    eraHistory <- Backend.queryEraHistory backend QueryTip
    tipSlotNo <- fromMaybe 0 . Api.chainPointToSlotNo <$> Backend.queryTip backend
    (tipTime, _slotLength) <- either throwIO pure $ getProgress tipSlotNo eraHistory
    let timeDifference = diffRelativeTime targetTime tipTime
    let percentDone = realToFrac (100.0 * getRelativeTime tipTime / getRelativeTime targetTime)
    blockTime <- Backend.getBlockTime backend
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
