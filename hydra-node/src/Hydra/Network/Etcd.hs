{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

-- | Implements a Hydra network component using [etcd](https://etcd.io/).
--
-- While implementing a basic broadcast protocol over a distributed key-value
-- store is quite an overkill, the [Raft](https://raft.github.io/) consensus of
-- etcd provides our application with a crash-recovery fault-tolerant "atomic
-- broadcast" out-of-the-box. As a nice side-effect, the network layer becomes
-- very introspectable, while it would also support features like TLS or service
-- discovery.
--
-- The component installs, starts and configures an `etcd` instance and connects
-- to it using a GRPC client. We can only write and read from the cluster while
-- connected to the majority cluster.
--
-- Broadcasting is implemented using @put@ to some well-known key, while message
-- delivery is done by using @watch@ on the same 'msg' prefix. We keep a last known
-- revision, also stored on disk, to start 'watch' with that revision (+1) and
-- only deliver messages that were not seen before. In case we are not connected
-- to our 'etcd' instance or not enough peers (= on a minority cluster), we
-- retry sending, but also store messages to broadcast in a 'PersistentQueue',
-- which makes the node resilient against crashes while sending. TODO: Is this
-- needed? performance limitation?
--
-- Connectivity and compatibility with other nodes on the cluster is tracked
-- using the key-value service as well:
--
--   * network connectivity is determined by being able to fetch the member list
--   * peer connectivity is tracked (best effort, not authorized) using an entry
--     at 'alive-\<advertise\>' keys with individual leases and repeated keep-alives
--   * each node compare-and-swaps its `version` into a key of the same name to
--     check compatibility (not updatable)
--
-- Note that the etcd cluster is configured to compact revisions down to 1000
-- every 5 minutes. This prevents infinite growth of the key-value store, but
-- also limits how long a node can be disconnected without missing out. 1000
-- should be more than enough for our use-case as the Hydra protocol will not
-- advance unless all participants are present.
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Cardano.Crypto.Hash (SHA256, hashToStringAsHex, hashWithSerialiser)
import Control.Concurrent.Class.MonadSTM (
  newTVarIO,
  swapTVar,
  writeTVar,
 )
import Control.Exception (IOException)
import Control.Lens ((^.), (^..), (^?))
import Data.Aeson (decodeFileStrict', encodeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens qualified as Aeson
import Data.Aeson.Types (Value)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List ((\\))
import Data.Map qualified as Map
import Data.Text qualified as T
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (
  Connectivity (..),
  Host (..),
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  NetworkConfiguration (..),
  ProtocolVersion,
  WhichEtcd (..),
 )
import Hydra.Node.EmbedTH (embedExecutable)
import Hydra.PersistentQueue (
  PersistentQueue (..),
  newPersistentQueue,
  peekPersistentQueue,
  popPersistentQueue,
  writePersistentQueue,
 )
import Network.GRPC.Client (
  Address (..),
  ConnParams (..),
  Connection,
  ReconnectPolicy (..),
  ReconnectTo (ReconnectToOriginal),
  Server (..),
  rpc,
  withConnection,
 )
import Network.GRPC.Client.StreamType.IO (biDiStreaming, nonStreaming)
import Network.GRPC.Common (GrpcError (..), GrpcException (..), HTTP2Settings (..), NextElem (..), def, defaultHTTP2Settings)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Proto (..), Protobuf, defMessage, (.~))
import Network.GRPC.Etcd (
  Compare'CompareResult (..),
  Compare'CompareTarget (..),
  KV,
  Lease,
  Watch,
 )
import System.Directory (createDirectoryIfMissing)
import System.Environment.Blank (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode, setFileMode)
import System.Process (interruptProcessGroupOf)
import System.Process.Typed (
  Process,
  ProcessConfig,
  createPipe,
  getStderr,
  proc,
  setCreateGroup,
  setEnv,
  setStderr,
  startProcess,
  stopProcess,
  unsafeProcessHandle,
  waitExitCode,
 )
import UnliftIO (readTVarIO)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg, Eq msg) =>
  Tracer IO EtcdLog ->
  ProtocolVersion ->
  -- TODO: check if all of these needed?
  NetworkConfiguration ->
  NetworkComponent IO msg msg ()
withEtcdNetwork tracer protocolVersion config callback action = do
  etcdBinPath <- getEtcdBinary persistenceDir whichEtcd
  -- TODO: fail if cluster config / members do not match --peer
  -- configuration? That would be similar to the 'acks' persistence
  -- bailing out on loading.
  envVars <- Map.fromList <$> getEnvironment
  withProcessInterrupt (etcdCmd etcdBinPath envVars) $ \p -> do
    race_ (waitExitCode p >>= \ec -> fail $ "Sub-process etcd exited with: " <> show ec) $ do
      race_ (traceStderr p callback) $ do
        -- XXX: cleanup reconnecting through policy if other threads fail
        doneVar <- newTVarIO False
        -- NOTE: The connection to the server is set up asynchronously; the
        -- first rpc call will block until the connection has been established.
        withConnection (connParams doneVar) grpcServer $ \conn -> do
          -- REVIEW: checkVersion blocks if used on main thread - why?
          withAsync (checkVersion tracer conn protocolVersion callback) $ \_ -> do
            race_ (pollConnectivity tracer conn advertise callback) $
              race_ (waitMessages tracer conn persistenceDir callback) $ do
                queue <- newPersistentQueue (persistenceDir </> "pending-broadcast") 100
                race_ (broadcastMessages tracer conn advertise queue) $ do
                  action
                    Network
                      { broadcast = writePersistentQueue queue
                      }
                  atomically (writeTVar doneVar True)
 where
  connParams doneVar =
    def
      { connReconnectPolicy = reconnectPolicy doneVar
      , -- NOTE: Not rate limit pings to our trusted, local etcd node. See
        -- comment on 'http2OverridePingRateLimit'.
        connHTTP2Settings = defaultHTTP2Settings{http2OverridePingRateLimit = Just maxBound}
      }

  reconnectPolicy doneVar = ReconnectAfter ReconnectToOriginal $ do
    done <- readTVarIO doneVar
    if done
      then pure DontReconnect
      else do
        threadDelay 1
        traceWith tracer Reconnecting
        pure $ reconnectPolicy doneVar

  clientHost = Host{hostname = "127.0.0.1", port = clientPort}

  grpcServer =
    ServerInsecure $
      Address
        { addressHost = toString $ hostname clientHost
        , addressPort = port clientHost
        , addressAuthority = Nothing
        }

  -- NOTE: Offset client port by the same amount as configured 'port' is offset
  -- from the default '5001'. This will result in the default client port 2379
  -- be used by default still.
  clientPort = 2379 + port listen - 5001

  traceStderr p NetworkCallback{onConnectivity} =
    forever $ do
      bs <- BS.hGetLine (getStderr p)
      case Aeson.eitherDecodeStrict bs of
        Left err -> traceWith tracer FailedToDecodeLog{log = decodeUtf8 bs, reason = show err}
        Right v -> do
          let expectedClusterMismatch = do
                level' <- bs ^? Aeson.key "level" . Aeson.nonNull
                msg' <- bs ^? Aeson.key "msg" . Aeson.nonNull
                pure (level', msg')
          case expectedClusterMismatch of
            Just (Aeson.String "error", Aeson.String "request sent was ignored due to cluster ID mismatch") ->
              onConnectivity ClusterIDMismatch{clusterPeers = T.pack clusterPeers}
            _ -> traceWith tracer $ EtcdLog{etcd = v}

  -- XXX: Could use TLS to secure peer connections
  -- XXX: Could use discovery to simplify configuration
  -- NOTE: Configured using guides: https://etcd.io/docs/v3.5/op-guide
  etcdCmd etcdBinPath envVars =
    -- NOTE: We map prefers the left; so we need to mappend default at the end.
    setEnv (Map.toList $ envVars <> defaultEnv)
      . setCreateGroup True -- Prevents interrupt of main process when we send SIGINT to etcd
      . setStderr createPipe
      . proc etcdBinPath
      $ concat
        [ -- NOTE: Must be used in clusterPeers
          ["--name", show advertise]
        , ["--data-dir", persistenceDir </> "etcd" </> hashToStringAsHex (hashWithSerialiser @SHA256 toCBOR $ BS8.pack clusterPeers)]
        , ["--listen-peer-urls", httpUrl listen]
        , ["--initial-advertise-peer-urls", httpUrl advertise]
        , ["--listen-client-urls", httpUrl clientHost]
        , -- Pick a random port for http api (and use above only for grpc)
          ["--listen-client-http-urls", "http://localhost:0"]
        , -- Client access only on configured 'host' interface.
          ["--advertise-client-urls", httpUrl clientHost]
        , -- XXX: Could use unique initial-cluster-tokens to isolate clusters
          ["--initial-cluster-token", "hydra-network-1"]
        , ["--initial-cluster", clusterPeers]
        ]

  defaultEnv :: Map.Map String String
  defaultEnv =
    -- Keep up to 1000 revisions. See also:
    -- https://etcd.io/docs/v3.5/op-guide/maintenance/#auto-compaction
    Map.fromList
      [ ("ETCD_AUTO_COMPACTION_MODE", "revision")
      , ("ETCD_AUTO_COMPACTION_RETENTION", "1000")
      ]

  -- NOTE: Building a canonical list of labels from the advertised addresses
  clusterPeers =
    intercalate ","
      . map (\h -> show h <> "=" <> httpUrl h)
      $ (advertise : peers)

  httpUrl (Host h p) = "http://" <> toString h <> ":" <> show p

  NetworkConfiguration{persistenceDir, listen, advertise, peers, whichEtcd} = config

-- | Return the path of the etcd binary. Will either install it first, or just
-- assume there is one available on the system path.
getEtcdBinary :: FilePath -> WhichEtcd -> IO FilePath
getEtcdBinary _ SystemEtcd = pure "etcd"
getEtcdBinary persistenceDir EmbeddedEtcd =
  let path = persistenceDir </> "bin" </> "etcd"
   in installEtcd path >> pure path

-- | Install the embedded 'etcd' binary to given file path.
installEtcd :: FilePath -> IO ()
installEtcd fp = do
  createDirectoryIfMissing True (takeDirectory fp)
  BS.writeFile fp $(embedExecutable "etcd")
  setFileMode fp (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)

-- | Check and write version on etcd cluster. This will retry until we are on a
-- majority cluster and succeed. If the version does not match a corresponding
-- 'Connectivity' message is sent via 'NetworkCallback'.
checkVersion ::
  Tracer IO EtcdLog ->
  Connection ->
  ProtocolVersion ->
  NetworkCallback msg IO ->
  IO ()
checkVersion tracer conn ourVersion NetworkCallback{onConnectivity} = do
  -- Get or write our version into kv store
  res <-
    nonStreaming conn (rpc @(Protobuf KV "txn")) $
      defMessage
        & #compare .~ [versionExists]
        & #success .~ [getVersion]
        & #failure .~ [putVersion]

  -- Check version if version was already present
  if res ^. #succeeded
    then forM_ (res ^.. #responses . traverse . #responseRange . #kvs . traverse) $ \kv ->
      case decodeFull' $ kv ^. #value of
        Left err -> do
          traceWith tracer $
            FailedToDecodeValue
              { key = decodeUtf8 $ kv ^. #key
              , value = encodeBase16 $ kv ^. #value
              , reason = show err
              }
          onConnectivity VersionMismatch{ourVersion, theirVersion = Nothing}
        Right theirVersion ->
          unless (theirVersion == ourVersion) $
            onConnectivity VersionMismatch{ourVersion, theirVersion = Just theirVersion}
    else
      traceWith tracer $ MatchingProtocolVersion{version = ourVersion}
 where
  versionKey = "version"

  -- exists = create_revision of key 'version' > 0
  versionExists =
    defMessage
      & #result .~ Proto Compare'GREATER
      & #target .~ Proto Compare'VERSION
      & #key .~ versionKey
      & #version .~ 0

  getVersion =
    defMessage & #requestRange .~ (defMessage & #key .~ versionKey)

  putVersion =
    defMessage
      & #requestPut
        .~ ( defMessage
              & #key .~ versionKey
              & #value .~ serialize' ourVersion
           )

-- | Broadcast messages from a queue to the etcd cluster.
--
-- TODO: retrying on failure even needed?
-- Retries on failure to 'putMessage' in case we are on a minority cluster.
broadcastMessages ::
  (ToCBOR msg, Eq msg) =>
  Tracer IO EtcdLog ->
  Connection ->
  -- | Used to identify sender.
  Host ->
  PersistentQueue IO msg ->
  IO ()
broadcastMessages tracer conn ourHost queue =
  withGrpcContext "broadcastMessages" . forever $ do
    msg <- peekPersistentQueue queue
    (putMessage conn ourHost msg >> popPersistentQueue queue msg)
      `catch` \case
        GrpcException{grpcError, grpcErrorMessage}
          | grpcError == GrpcUnavailable || grpcError == GrpcDeadlineExceeded -> do
              traceWith tracer $ BroadcastFailed{reason = fromMaybe "unknown" grpcErrorMessage}
              threadDelay 1
        e -> throwIO e

-- | Broadcast a message to the etcd cluster.
putMessage ::
  ToCBOR msg =>
  Connection ->
  -- | Used to identify sender.
  Host ->
  msg ->
  IO ()
putMessage conn ourHost msg =
  void $ nonStreaming conn (rpc @(Protobuf KV "put")) req
 where
  req =
    defMessage
      & #key .~ key
      & #value .~ serialize' msg

  key = encodeUtf8 @Text $ "msg-" <> show ourHost

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  FromCBOR msg =>
  Tracer IO EtcdLog ->
  Connection ->
  FilePath ->
  NetworkCallback msg IO ->
  IO ()
waitMessages tracer conn directory NetworkCallback{deliver} = do
  revision <- getLastKnownRevision directory
  withGrpcContext "waitMessages" . forever $ do
    -- NOTE: We have not observed the watch (subscription) fail even when peers
    -- leave and we end up on a minority cluster.
    biDiStreaming conn (rpc @(Protobuf Watch "watch")) $ \send recv -> do
      -- NOTE: Request all keys starting with 'msg'. See also section KeyRanges
      -- in https://etcd.io/docs/v3.5/learning/api/#key-value-api
      let watchRequest =
            defMessage
              & #key .~ "msg"
              & #rangeEnd .~ "msh" -- NOTE: g+1 to query prefixes
              & #startRevision .~ fromIntegral (revision + 1)
      send . NextElem $ defMessage & #createRequest .~ watchRequest
      whileNext_ recv process
    -- Wait before re-trying
    threadDelay 1
 where
  process res = do
    let revision = fromIntegral $ res ^. #header . #revision
    putLastKnownRevision directory revision
    forM_ (res ^. #events) $ \event -> do
      let value = event ^. #kv . #value
      case decodeFull' value of
        Left err ->
          traceWith
            tracer
            FailedToDecodeValue
              { key = decodeUtf8 $ event ^. #kv . #key
              , value = encodeBase16 value
              , reason = show err
              }
        Right msg -> deliver msg

getLastKnownRevision :: MonadIO m => FilePath -> m Natural
getLastKnownRevision directory = do
  liftIO $
    try (decodeFileStrict' $ directory </> "last-known-revision") >>= \case
      Right rev -> do
        pure $ fromMaybe 1 rev
      Left (e :: IOException)
        | isDoesNotExistError e -> pure 1
        | otherwise -> do
            fail $ "Failed to load last known revision: " <> show e

putLastKnownRevision :: MonadIO m => FilePath -> Natural -> m ()
putLastKnownRevision directory rev = do
  liftIO $ encodeFile (directory </> "last-known-revision") rev

-- | Write a well-known key to indicate being alive, keep it alive using a lease
-- and poll other peers entries to yield connectivity events. While doing so,
-- overall network connectivity is determined from the ability to read/write to
-- the cluster.
pollConnectivity ::
  Tracer IO EtcdLog ->
  Connection ->
  -- | Local host
  Host ->
  NetworkCallback msg IO ->
  IO ()
pollConnectivity tracer conn advertise NetworkCallback{onConnectivity} = do
  seenAliveVar <- newTVarIO []
  withGrpcContext "pollConnectivity" $
    forever . handle (onGrpcException seenAliveVar) $ do
      leaseId <- createLease
      -- If we can create a lease, we are connected
      onConnectivity NetworkConnected
      -- Write our alive key using lease
      writeAlive leaseId
      traceWith tracer CreatedLease{leaseId}
      withKeepAlive leaseId $ \keepAlive ->
        forever $ do
          -- Keep our lease alive
          ttlRemaining <- keepAlive
          when (ttlRemaining < 1) $
            traceWith tracer LowLeaseTTL{ttlRemaining}
          -- Determine alive peers
          alive <- getAlive
          let othersAlive = alive \\ [advertise]
          seenAlive <- atomically $ swapTVar seenAliveVar othersAlive
          forM_ (othersAlive \\ seenAlive) $ onConnectivity . PeerConnected
          forM_ (seenAlive \\ othersAlive) $ onConnectivity . PeerDisconnected
          -- Wait roughly ttl / 2
          threadDelay (ttlRemaining / 2)
 where
  onGrpcException seenAliveVar GrpcException{grpcError}
    | grpcError == GrpcUnavailable || grpcError == GrpcDeadlineExceeded = do
        onConnectivity NetworkDisconnected
        atomically $ writeTVar seenAliveVar []
        threadDelay 1
  onGrpcException _ e = throwIO e

  createLease = withGrpcContext "createLease" $ do
    leaseResponse <-
      nonStreaming conn (rpc @(Protobuf Lease "leaseGrant")) $
        defMessage & #ttl .~ 3
    pure $ leaseResponse ^. #id

  withKeepAlive leaseId action = do
    biDiStreaming conn (rpc @(Protobuf Lease "leaseKeepAlive")) $ \send recv -> do
      void . action $ do
        send $ NextElem $ defMessage & #id .~ leaseId
        recv >>= \case
          NextElem res -> pure . fromIntegral $ res ^. #ttl
          NoNextElem -> do
            traceWith tracer NoKeepAliveResponse
            pure 0

  writeAlive leaseId = withGrpcContext "writeAlive" $ do
    void . nonStreaming conn (rpc @(Protobuf KV "put")) $
      defMessage
        & #key .~ "alive-" <> show advertise
        & #value .~ serialize' advertise
        & #lease .~ leaseId

  getAlive = withGrpcContext "getAlive" $ do
    res <-
      nonStreaming conn (rpc @(Protobuf KV "range")) $
        defMessage
          & #key .~ "alive"
          & #rangeEnd .~ "alivf" -- NOTE: e+1 to query prefixes
    flip mapMaybeM (res ^.. #kvs . traverse) $ \kv -> do
      let value = kv ^. #value
      case decodeFull' value of
        Left err -> do
          traceWith
            tracer
            FailedToDecodeValue
              { key = decodeUtf8 $ kv ^. #key
              , value = encodeBase16 value
              , reason = show err
              }
          pure Nothing
        Right x -> pure $ Just x

-- | Add context to the 'grpcErrorMessage' of any 'GrpcException' raised.
withGrpcContext :: MonadCatch m => Text -> m a -> m a
withGrpcContext context action =
  action `catch` \e@GrpcException{grpcErrorMessage} ->
    throwIO
      e
        { grpcErrorMessage =
            case grpcErrorMessage of
              Nothing -> Just context
              Just msg -> Just $ context <> ": " <> msg
        }

-- | Like 'withProcessTerm', but sends first SIGINT and only SIGTERM if not
-- stopped within 5 seconds.
withProcessInterrupt ::
  (MonadIO m, MonadThrow m) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessInterrupt config =
  bracket (startProcess $ config & setCreateGroup True) signalAndStopProcess
 where
  signalAndStopProcess :: MonadIO m => Process stdin stdout stderr -> m ()
  signalAndStopProcess p = liftIO $ do
    interruptProcessGroupOf (unsafeProcessHandle p)
    race_
      (void $ waitExitCode p)
      (threadDelay 5 >> stopProcess p)

-- * Tracing

data EtcdLog
  = EtcdLog {etcd :: Value}
  | Reconnecting
  | BroadcastFailed {reason :: Text}
  | FailedToDecodeLog {log :: Text, reason :: Text}
  | FailedToDecodeValue {key :: Text, value :: Text, reason :: Text}
  | CreatedLease {leaseId :: Int64}
  | LowLeaseTTL {ttlRemaining :: DiffTime}
  | NoKeepAliveResponse
  | MatchingProtocolVersion {version :: ProtocolVersion}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
