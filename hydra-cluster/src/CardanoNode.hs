{-# LANGUAGE DuplicateRecordFields #-}

module CardanoNode where

import Hydra.Prelude

import CardanoClient (NodeLog (..), RunningNode (..), waitForFullySynchronized)
import Control.Lens ((?~), (^?!))
import Control.Tracer (Tracer, traceWith)
import Data.Aeson (Value (String), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey, key, _Number)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Hydra.Cardano.Api (AsType (AsPaymentKey), File (..), NetworkId, PaymentKey, SigningKey, SocketPath, VerificationKey, generateSigningKey, getVerificationKey)
import Hydra.Cardano.Api qualified as Api
import Hydra.Chain.CardanoClient (QueryPoint (QueryTip), queryProtocolParameters)
import Hydra.Cluster.Fixture (
  KnownNetwork (Mainnet, Preproduction, Preview),
  defaultNetworkId,
 )
import Hydra.Cluster.Util (readConfigFile)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequestThrow)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.Posix (ownerReadMode, setFileMode)
import System.Process (
  CreateProcess (..),
  StdStream (CreatePipe, UseHandle),
  proc,
  readProcess,
  withCreateProcess,
 )
import Test.Hydra.Prelude

type Port = Int

newtype NodeId = NodeId Int
  deriving newtype (Eq, Show, Num, ToJSON, FromJSON)

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
  deriving anyclass (ToJSON, FromJSON)

-- | Arguments given to the 'cardano-node' command-line to run a node.
data CardanoNodeArgs = CardanoNodeArgs
  { nodeSocket :: FilePath
  , nodeConfigFile :: FilePath
  , nodeByronGenesisFile :: FilePath
  , nodeShelleyGenesisFile :: FilePath
  , nodeAlonzoGenesisFile :: FilePath
  , nodeConwayGenesisFile :: FilePath
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
  deriving anyclass (ToJSON, FromJSON)

getCardanoNodeVersion :: IO String
getCardanoNodeVersion =
  readProcess "cardano-node" ["--version"] ""

-- | Start a single cardano-node devnet using the config from config/ and
-- credentials from config/credentials/. Only the 'Faucet' actor will receive
-- "initialFunds". Use 'seedFromFaucet' to distribute funds other wallets.
withCardanoNodeDevnet ::
  Tracer IO NodeLog ->
  -- | State directory in which credentials, db & logs are persisted.
  FilePath ->
  (RunningNode -> IO a) ->
  IO a
withCardanoNodeDevnet tracer stateDirectory action = do
  args <- setupCardanoDevnet stateDirectory
  withCardanoNode tracer stateDirectory args networkId $ \nodeSocket -> do
    traceWith tracer MsgNodeIsReady
    pparams <- queryProtocolParameters networkId nodeSocket QueryTip
    let rn =
          RunningNode
            { nodeSocket
            , networkId
            , pparams
            }
    action rn
 where
  -- NOTE: This needs to match what's in config/genesis-shelley.json
  networkId = defaultNetworkId

-- | Run a cardano-node as normal network participant on a known network.
withCardanoNodeOnKnownNetwork ::
  Tracer IO NodeLog ->
  -- | State directory in which node db & logs are persisted.
  FilePath ->
  -- | A well-known Cardano network to connect to.
  KnownNetwork ->
  (RunningNode -> IO ()) ->
  IO ()
withCardanoNodeOnKnownNetwork tracer workDir knownNetwork action = do
  copyKnownNetworkFiles
  networkId <- readNetworkId
  withCardanoNode tracer workDir args networkId $ \nodeSocket -> do
    traceWith tracer MsgNodeIsReady
    pparams <- queryProtocolParameters networkId nodeSocket QueryTip
    let rn =
          RunningNode
            { nodeSocket
            , networkId
            , pparams
            }
    action rn
 where
  args =
    defaultCardanoNodeArgs
      { nodeConfigFile = "config.json"
      , nodeTopologyFile = "topology.json"
      , nodeByronGenesisFile = "byron-genesis.json"
      , nodeShelleyGenesisFile = "shelley-genesis.json"
      , nodeAlonzoGenesisFile = "alonzo-genesis.json"
      , nodeConwayGenesisFile = "conway-genesis.json"
      }

  -- Read 'NetworkId' from shelley genesis
  readNetworkId = do
    shelleyGenesis :: Aeson.Value <- unsafeDecodeJson =<< readFileBS (workDir </> "shelley-genesis.json")
    if shelleyGenesis ^?! key "networkId" == "Mainnet"
      then pure Api.Mainnet
      else do
        let magic = shelleyGenesis ^?! key "networkMagic" . _Number
        pure $ Api.Testnet (Api.NetworkMagic $ truncate magic)

  -- Copy/download configuration files for a known network
  copyKnownNetworkFiles =
    forM_
      [ "config.json"
      , "topology.json"
      , "byron-genesis.json"
      , "shelley-genesis.json"
      , "alonzo-genesis.json"
      , "conway-genesis.json"
      ]
      $ \fn ->
        unlessM (doesFileExist $ workDir </> fn) $ do
          createDirectoryIfMissing True $ workDir </> takeDirectory fn
          fetchConfigFile (knownNetworkPath </> fn)
            >>= writeFileBS (workDir </> fn)

  knownNetworkPath =
    knownNetworkConfigBaseURL </> knownNetworkName

  -- Base path on remote
  knownNetworkConfigBaseURL = "https://book.world.dev.cardano.org/environments"

  -- Network name on remote
  knownNetworkName = case knownNetwork of
    Preview -> "preview"
    Preproduction -> "preprod"
    Mainnet -> "mainnet"

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

  writeTopology peers args =
    Aeson.encodeFile (stateDirectory </> nodeTopologyFile args) $
      mkTopology peers

-- | Modify the cardano-node configuration to fork into conway at given era
-- number.
forkIntoConwayInEpoch :: FilePath -> CardanoNodeArgs -> Natural -> IO ()
forkIntoConwayInEpoch stateDirectory args n = do
  config <-
    unsafeDecodeJsonFile @Aeson.Value (stateDirectory </> nodeConfigFile args)
      <&> atKey "TestConwayHardForkAtEpoch" ?~ toJSON n
  Aeson.encodeFile
    (stateDirectory </> nodeConfigFile args)
    config

withCardanoNode ::
  Tracer IO NodeLog ->
  FilePath ->
  CardanoNodeArgs ->
  NetworkId ->
  (SocketPath -> IO a) ->
  IO a
withCardanoNode tr stateDirectory args@CardanoNodeArgs{nodeSocket} networkId action = do
  traceWith tr $ MsgNodeCmdSpec (show $ cmdspec process)
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = CreatePipe} $
      \_stdin _stdout mError processHandle -> do
        let errorHandle = fromMaybe (error "Should not happen™") mError
            runningNonde = checkProcessHasNotDied "cardano-node" processHandle errorHandle
        ( race runningNonde waitForNode >>= \case
            Left{} -> error "should never been reached"
            Right a -> pure a
          )
          `finally` cleanupSocketFile
 where
  process = cardanoNodeProcess (Just stateDirectory) args

  logFilePath = stateDirectory </> "logs" </> "cardano-node.log"

  socketPath = stateDirectory </> nodeSocket

  waitForNode = do
    let nodeSocketPath = File socketPath
    traceWith tr $ MsgNodeStarting{stateDirectory}
    waitForSocket nodeSocketPath
    traceWith tr $ MsgSocketIsReady $ unFile nodeSocketPath
    -- we wait for synchronization since otherwise we will receive a query
    -- exception when trying to obtain pparams and the era is not the one we
    -- expect.
    _ <- waitForFullySynchronized tr networkId nodeSocketPath
    action nodeSocketPath

  cleanupSocketFile =
    whenM (doesFileExist socketPath) $
      removeFile socketPath

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
  deriving stock (Show)

instance Exception ProcessHasExited

--
-- Helpers
--

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
