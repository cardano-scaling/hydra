{-# LANGUAGE TypeApplications #-}

module Node where

import Cardano.Prelude

import Data.Aeson ((.=), FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text    -- REVIEW(SN): not in prelude?
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Logging
  (HasSeverityAnnotation(..), Severity(Debug, Error, Info), Tracer, traceWith)
import qualified Prelude
import System.FilePath ((</>))
import System.Process
  (CreateProcess(..), ProcessHandle, proc, withCreateProcess)

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
    ports  :: PortsConfig
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
  , nodePort   :: Maybe Port
  }

defaultCardanoNodeArgs :: CardanoNodeArgs
defaultCardanoNodeArgs = CardanoNodeArgs
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
  , nodePort   = Nothing
  }

-- | Configuration of ports from the perspective of a peer in the context of a
-- fully sockected topology.
data PortsConfig = PortsConfig
  { -- | Our node TCP port.
    ours  :: Port
  , -- | Other peers TCP ports.
    peers :: [Port]
  }

withCardanoNode
  :: CardanoNodeConfig -> CardanoNodeArgs -> (RunningNode -> IO a) -> IO a
withCardanoNode cfg args action = do
  generateEnvironment
  let process = cardanoNodeProcess (Just $ stateDirectory cfg) args
  print (cmdspec process)
  withCreateProcess process
    $ \_stdin _stdout _stderr h ->
      -- Wait for socket and pass it to action
                                   action (RunningNode h)
 where
  generateEnvironment = do
    refreshSystemStart cfg args
    let topology = mkTopology $ peers $ ports cfg
    Aeson.encodeFile (stateDirectory cfg </> nodeTopologyFile args) topology

-- | Generate command-line arguments for launching @cardano-node@.
cardanoNodeProcess :: Maybe FilePath -> CardanoNodeArgs -> CreateProcess
cardanoNodeProcess cwd args = (proc "cardano-node" strArgs) { cwd }
 where
  strArgs = "run" : mconcat
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
    Nothing  -> []
    Just val -> [arg, val]

-- | Re-generate configuration and genesis files with fresh system start times.
refreshSystemStart :: CardanoNodeConfig -> CardanoNodeArgs -> IO ()
refreshSystemStart cfg args = do
  let startTime = round @_ @Int . utcTimeToPOSIXSeconds $ systemStart cfg
  byronGenesis <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeByronGenesisFile args)
      <&> addField "startTime" startTime

  let
    systemStartUTC =
      posixSecondsToUTCTime . fromRational . toRational $ startTime
  shelleyGenesis <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeShelleyGenesisFile args)
      <&> addField "systemStart" systemStartUTC

  config <-
    unsafeDecodeJsonFile (stateDirectory cfg </> nodeConfigFile args)
    >>= pure
    .   addField "ByronGenesisFile" (nodeByronGenesisFile args)
    >>= pure
    .   addField "ShelleyGenesisFile" (nodeShelleyGenesisFile args)

  Aeson.encodeFile
    (stateDirectory cfg </> nodeByronGenesisFile args)
    byronGenesis
  Aeson.encodeFile
    (stateDirectory cfg </> nodeShelleyGenesisFile args)
    shelleyGenesis
  Aeson.encodeFile (stateDirectory cfg </> nodeConfigFile args) config

-- | Generate a topology file from a list of peers.
mkTopology :: [Port] -> Aeson.Value
mkTopology peers = do
  Aeson.object ["Producers" .= map encodePeer peers]
 where
  encodePeer :: Int -> Aeson.Value
  encodePeer port = Aeson.object
    [ "addr" .= ("127.0.0.1" :: Prelude.String)
    , "port" .= port
    , "valency" .= (1 :: Int)
    ]


-- | Make a 'CreateProcess' for running @cardano-cli@. The program must be on
-- the @PATH@, as normal. Sets @CARDANO_NODE_SOCKET_PATH@ for the subprocess, if
-- a 'CardanoNodeConn' is provided.
cliCreateProcess
  :: Tracer IO NodeLog -- ^ for logging the command
  -> FilePath -- ^ cardano node socket path
  -> [Text] -- ^ command-line arguments
  -> IO CreateProcess
cliCreateProcess tr sock args = do
  traceWith tr (MsgCLI args)
  let socketEnv = ("CARDANO_NODE_SOCKET_PATH", sock)
  let cp = proc "cardano-cli" $ fmap Text.unpack args
  pure $ cp { env = Just (socketEnv : fromMaybe [] (env cp)) }

-- | Query a cardano node tip with retrying.
cliQueryTip
  :: Tracer IO NodeLog
  -> FilePath -- ^ cardano node socket path
  -> IO ()
cliQueryTip tr sock = do
  let msg = "Checking for usable socket file " <> Text.pack sock
  -- TODO: check whether querying the tip works just as well.
  cliRetry tr msg =<< cliCreateProcess
    tr
    sock
    [ "query"
    , "tip"
    , "--mainnet"
      --, "--testnet-magic", "764824073"
    , "--cardano-mode"
    ]
  traceWith tr $ MsgSocketIsReady sock

-- | Runs a @cardano-cli@ command and retries for up to 30 seconds if the
-- command failed.
--
-- Assumes @cardano-cli@ is available in @PATH@.
cliRetry
  :: Tracer IO NodeLog
  -> Text -- ^ message to print before running command
  -> CreateProcess
  -> IO ()
cliRetry _tr _msg _cp = do
  panic "not implemented"
 --  (st, _, _) <- retrying pol (const isFail) (const cmd)
 --  traceWith tr $ MsgCLIStatus msg st
 --  case st of
 --    ExitSuccess -> pure ()
 --    ExitFailure _ ->
 --      throwIO $ ProcessHasExited ("cardano-cli failed: " <> BL8.unpack err) st
 -- where
 --  cmd = do
 --    traceWith tr $ MsgCLIRetry msg
 --    (st, out, err) <- readProcess processConfig
 --    case st of
 --      ExitSuccess -> pure ()
 --      ExitFailure code -> traceWith tr (MsgCLIRetryResult msg code err)
 --    pure (st, out, err)
 --  isFail (st, _, _) = pure (st /= ExitSuccess)
 --  pol = limitRetriesByCumulativeDelay 30_000_000 $ constantDelay 1_000_000

-- Logging

data NodeLog =
    MsgCLI [Text]
    | MsgCLIStatus Text ExitCode
    | MsgCLIRetry Text
    | MsgCLIRetryResult Text Int
    | MsgSocketIsReady FilePath
    deriving (Show)

instance HasSeverityAnnotation NodeLog where
  getSeverityAnnotation = \case
    MsgCLI{} -> Debug
    MsgCLIStatus _ ExitSuccess -> Debug
    MsgCLIStatus _ (ExitFailure _) -> Error
    MsgCLIRetry _ -> Info
    MsgCLIRetryResult{} -> Info
    MsgSocketIsReady{} -> Info

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
unsafeDecodeJsonFile =
  Aeson.eitherDecodeFileStrict >=> either Prelude.fail pure
