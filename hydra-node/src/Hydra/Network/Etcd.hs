{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- The component starts and configures an `etcd` instance and connects to it
-- using a GRPC client. We can only write and read from the cluster while
-- connected to the majority cluster.
--
-- Broadcasting is implemented using @put@ to some well-known key, while
-- message delivery is done by using @watch@ on the same key. We keep a last
-- known revision, also stored on disk, to start 'watch' with that revision (+1)
-- and only deliver messages that were not seen before. In case we are not
-- connected to our 'etcd' instance or not enough peers (= on a minority
-- cluster), we retry sending, but also store messages to broadcast in a
-- 'PersistentQueue', which makes the node resilient against crashes while
-- sending. TODO: Is this needed? performance limitation?
--
-- Connectivity and compatibility with other nodes on the cluster is tracked
-- using the key-value service as well:
--
--   * network connectivity is determined by being able to fetch the member list
--   * peer connectivity is tracked (best effort, not authorized) using an entry
--     at 'alive-\<node id\>' keys with individual leases and repeated keep-alives
--   * each node compare-and-swaps its `version` into a key of the same name to
--     check compatibility (not updatable)
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  newTBQueueIO,
  newTVarIO,
  peekTBQueue,
  readTBQueue,
  swapTVar,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (IOException)
import Control.Lens ((^.), (^..))
import Data.Aeson (decodeFileStrict', encodeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Value)
import Data.ByteString qualified as BS
import Data.List ((\\))
import Data.List qualified as List
import Data.Text (splitOn)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (
  Connectivity (..),
  Host (..),
  HydraHandshakeRefused (..),
  HydraVersionedProtocolNumber (MkHydraVersionedProtocolNumber),
  KnownHydraVersions (..),
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  NetworkConfiguration (..),
  PortNumber,
  hydraVersionedProtocolNumber,
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
import Network.GRPC.Common (GrpcError (..), GrpcException (..), NextElem (..), def)
import Network.GRPC.Common.NextElem (whileNext_)
import Network.GRPC.Common.Protobuf (Protobuf, defMessage, (.~))
import Network.GRPC.Etcd (KV, Lease, Watch)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (
  createPipe,
  getStderr,
  proc,
  setStderr,
  stopProcess,
  waitExitCode,
  withProcessTerm,
 )
import UnliftIO (readTVarIO)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg, Eq msg) =>
  Tracer IO EtcdLog ->
  HydraVersionedProtocolNumber ->
  -- TODO: check if all of these needed?
  NetworkConfiguration ->
  NetworkComponent IO msg msg ()
withEtcdNetwork tracer protocolVersion config callback action = do
  -- TODO: fail if cluster config / members do not match --peer
  -- configuration? That would be similar to the 'acks' persistence
  -- bailing out on loading.
  withProcessTerm etcdCmd $ \p -> do
    -- Ensure the sub-process is also stopped when we get asked to terminate.
    _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
    -- TODO: error handling (also kill threads)
    race_ (waitExitCode p >>= \ec -> fail $ "etcd exited with: " <> show ec) $ do
      race_ (traceStderr p) $ do
        -- TODO: cleanup reconnecting through policy if other threads fail
        doneVar <- newTVarIO False
        -- NOTE: The connection to the server is set up asynchronously; the
        -- first rpc call will block until the connection has been established.
        withConnection (connParams doneVar) server $ \conn -> do
          (`finally` atomically (writeTVar doneVar True)) $
            race_ (pollConnectivity conn localHost callback) $ do
              race_ (waitMessages conn protocolVersion persistenceDir callback) $ do
                queue <- newPersistentQueue (persistenceDir </> "pending-broadcast") 100
                race_ (broadcastMessages conn protocolVersion port queue) $
                  action
                    Network
                      { broadcast = writePersistentQueue queue
                      }
 where
  connParams doneVar = def{connReconnectPolicy = reconnectPolicy doneVar}

  reconnectPolicy doneVar = ReconnectAfter ReconnectToOriginal $ do
    done <- readTVarIO doneVar
    if done
      then pure DontReconnect
      else do
        threadDelay 1
        traceWith tracer Reconnecting
        pure $ reconnectPolicy doneVar

  server =
    ServerInsecure $
      Address
        { addressHost = show host
        , addressPort = clientPort
        , addressAuthority = Nothing
        }

  -- NOTE: Offset client port by the same amount as configured 'port' is offset
  -- from the default '5001'. This will result in the default client port 2379
  -- be used by default still.
  clientPort = 2379 + port - 5001

  traceStderr p =
    forever $ do
      bs <- BS.hGetLine (getStderr p)
      case Aeson.eitherDecodeStrict bs of
        Left err -> fail $ "Failed to decode etcd log: " <> show err
        Right v -> traceWith tracer $ EtcdLog{etcd = v}

  -- XXX: Could use TLS to secure peer connections
  -- XXX: Could use discovery to simplify configuration
  -- NOTE: Configured using guides: https://etcd.io/docs/v3.5/op-guide
  -- TODO: "Running http and grpc server on single port. This is not recommended for production."
  etcdCmd =
    setStderr createPipe $
      proc "etcd" $
        concat
          [ -- NOTE: Must be usedin clusterPeers
            ["--name", show localHost]
          , ["--data-dir", persistenceDir </> "etcd"]
          , ["--listen-peer-urls", httpUrl localHost]
          , ["--initial-advertise-peer-urls", httpUrl localHost]
          , ["--listen-client-urls", clientUrl]
          , -- Client access only on configured 'host' interface.
            ["--advertise-client-urls", clientUrl]
          , -- XXX: could use unique initial-cluster-tokens to isolate clusters
            ["--initial-cluster-token", "hydra-network-1"]
          , ["--initial-cluster", clusterPeers]
          -- TODO: auto-compaction? prevent infinite growth of revisions? e.g.
          -- auto-compaction-mode=revision --auto-compaction-retention=1000 to keep 1000 revisions
          ]

  clientUrl = httpUrl Host{hostname = show host, port = clientPort}

  -- NOTE: Building a canonical list of labels from the advertised hostname+port
  clusterPeers =
    intercalate ","
      . map (\h -> show h <> "=" <> httpUrl h)
      $ (localHost : peers)

  httpUrl (Host h p) = "http://" <> toString h <> ":" <> show p

  localHost = Host{hostname = show host, port}

  NetworkConfiguration{persistenceDir, host, port, peers} = config

-- | Fetch and check version of the Hydra network protocol mapped onto the etcd
-- cluster. See 'putMessage' for how a peer encodes the protocol version into an
-- etcd key.
matchVersion ::
  -- | Key used by the other peer.
  ByteString ->
  -- | Our protocol version to check against
  HydraVersionedProtocolNumber ->
  Maybe HydraHandshakeRefused
matchVersion key ourVersion = do
  case splitOn "-" $ decodeUtf8 key of
    [_prefix, versionText, port] ->
      case parseVersion versionText of
        Just theirVersion
          | ourVersion == theirVersion -> Nothing
          | otherwise ->
              -- TODO: DRY just cases
              Just
                HydraHandshakeRefused
                  { remoteHost = Host "???" $ fromMaybe 0 $ parsePort port
                  , ourVersion
                  , theirVersions = KnownHydraVersions [theirVersion]
                  }
        Nothing ->
          Just
            HydraHandshakeRefused
              { remoteHost = Host "???" $ fromMaybe 0 $ parsePort port
              , ourVersion
              , theirVersions = NoKnownHydraVersions
              }
    _ ->
      Just
        HydraHandshakeRefused
          { remoteHost = Host "???" 0 -- TODO: use optional data type
          , ourVersion
          , theirVersions = NoKnownHydraVersions
          }
 where
  parseVersion = fmap MkHydraVersionedProtocolNumber . readMaybe . toString

  parsePort = readMaybe . toString

-- | Broadcast messages from a queue to the etcd cluster.
--
-- TODO: PutFailed not raised - retrying on failure even needed?
-- Retries on failure to 'putMessage' in case we are on a minority cluster.
broadcastMessages ::
  (ToCBOR msg, Eq msg) =>
  Connection ->
  HydraVersionedProtocolNumber ->
  PortNumber ->
  PersistentQueue IO msg ->
  IO ()
broadcastMessages conn protocolVersion port queue =
  forever $ do
    msg <- peekPersistentQueue queue
    (putMessage conn protocolVersion port msg >> popPersistentQueue queue msg)
      `catch` \PutFailed{reason} -> do
        putTextLn $ "put failed: " <> reason
        threadDelay 1

-- | Broadcast a message to the etcd cluster.
putMessage ::
  ToCBOR msg =>
  Connection ->
  HydraVersionedProtocolNumber ->
  PortNumber ->
  msg ->
  IO ()
putMessage conn protocolVersion port msg =
  void $ nonStreaming conn (rpc @(Protobuf KV "put")) req
 where
  req =
    defMessage
      & #key .~ key
      & #value .~ serialize' msg

  -- TODO: use one key again (after mapping version check)?
  key = encodeUtf8 @Text $ "msg-" <> show (hydraVersionedProtocolNumber protocolVersion) <> "-" <> show port

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  FromCBOR msg =>
  Connection ->
  HydraVersionedProtocolNumber ->
  FilePath ->
  NetworkCallback msg IO ->
  IO ()
waitMessages conn protocolVersion directory NetworkCallback{deliver, onConnectivity} = do
  revision <- getLastKnownRevision directory
  forever $ do
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
    -- Wait before reconnecting TODO: is this even possible?
    threadDelay 1
 where
  process res = do
    -- TODO: error handling if watch canceled
    let revision = fromIntegral $ res ^. #header . #revision
    putLastKnownRevision directory revision
    forM_ (res ^. #events) $ \event -> do
      let key = event ^. #kv . #key
      -- XXX: Check version on every watch event?
      case matchVersion key protocolVersion of
        Nothing ->
          pure ()
        Just HydraHandshakeRefused{remoteHost, ourVersion, theirVersions} ->
          onConnectivity $ HandshakeFailure{remoteHost, ourVersion, theirVersions}

      let value = event ^. #kv . #value
      case decodeFull' value of
        -- TODO: error handling (with actual client)
        Left err -> fail $ "Failed to decode etcd entry: " <> show err
        Right msg -> deliver msg

getLastKnownRevision :: MonadIO m => FilePath -> m Natural
getLastKnownRevision directory = do
  liftIO $
    try (decodeFileStrict' $ directory </> "last-known-revision.json") >>= \case
      Right rev -> do
        pure $ fromMaybe 1 rev
      Left (e :: IOException)
        | isDoesNotExistError e -> pure 1
        | otherwise -> do
            fail $ "Failed to load last known revision: " <> show e

putLastKnownRevision :: MonadIO m => FilePath -> Natural -> m ()
putLastKnownRevision directory rev = do
  liftIO $ encodeFile (directory </> "last-known-revision.json") rev

-- | Write a well-known key to indicate being alive, keep it alive using a lease
-- and poll other peers enries to yield connectivity events. While doing so,
-- overall network connectivity is determined from the ability to read/write to
-- the cluster.
pollConnectivity ::
  Connection ->
  -- | Local host
  Host ->
  NetworkCallback msg IO ->
  IO ()
pollConnectivity conn localHost NetworkCallback{onConnectivity} = do
  seenAliveVar <- newTVarIO []
  forever $ do
    leaseId <- createLease
    -- If we can create a lease, we are connected
    onConnectivity NetworkConnected
    handle onGrpcException $ do
      -- Write our alive key using lease
      writeAlive leaseId
      withKeepAlive leaseId $ \keepAlive ->
        forever $ do
          -- Keep our lease alive
          keepAlive
          -- Determine alive peers
          alive <- getAlive
          let othersAlive = alive \\ [localHost]
          seenAlive <- atomically $ swapTVar seenAliveVar othersAlive
          forM_ (othersAlive \\ seenAlive) $ onConnectivity . PeerConnected
          forM_ (seenAlive \\ othersAlive) $ onConnectivity . PeerDisconnected
          threadDelay 1
 where
  ttl = 3

  onGrpcException GrpcException{grpcError = GrpcUnavailable} = do
    onConnectivity NetworkDisconnected
    threadDelay 1
  onGrpcException e = throwIO e

  -- REVIEW: server can decide ttl?
  createLease = do
    leaseResponse <-
      nonStreaming conn (rpc @(Protobuf Lease "leaseGrant")) $
        defMessage & #ttl .~ ttl
    pure $ leaseResponse ^. #id

  writeAlive leaseId = do
    void . nonStreaming conn (rpc @(Protobuf KV "put")) $
      defMessage
        & #key .~ "alive-" <> show localHost
        & #value .~ serialize' localHost
        & #lease .~ leaseId

  withKeepAlive leaseId action = do
    biDiStreaming conn (rpc @(Protobuf Lease "leaseKeepAlive")) $ \send _recv ->
      void . action $ send $ NextElem (defMessage & #id .~ leaseId)

  getAlive = do
    res <-
      nonStreaming conn (rpc @(Protobuf KV "range")) $
        defMessage
          & #key .~ "alive"
          & #rangeEnd .~ "alivf" -- NOTE: e+1 to query prefixes
    pure $ flip mapMaybe (res ^.. #kvs . traverse . #value) $ \bs ->
      -- XXX: Silently swallow incompatible values. Hard to debug, but failure
      -- to decode here should not crash the component either.
      case decodeFull' bs of
        Left _err -> Nothing
        Right x -> pure x

-- * Low-level etcd api

-- TODO: never raised, remove if not needed in 'putMessage'
newtype PutException = PutFailed {reason :: Text}
  deriving stock (Show)

instance Exception PutException

-- * Persistent queue

data PersistentQueue m a = PersistentQueue
  { queue :: TBQueue m (Natural, a)
  , nextIx :: TVar m Natural
  , directory :: FilePath
  }

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueue ::
  (MonadSTM m, MonadIO m, FromCBOR a, MonadCatch m, MonadFail m) =>
  FilePath ->
  Natural ->
  m (PersistentQueue m a)
newPersistentQueue path capacity = do
  queue <- newTBQueueIO capacity
  highestId <-
    try (loadExisting queue) >>= \case
      Left (_ :: IOException) -> do
        liftIO $ createDirectoryIfMissing True path
        pure 0
      Right highest -> pure highest
  nextIx <- newTVarIO $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path}
 where
  loadExisting queue = do
    paths <- liftIO $ listDirectory path
    case sort $ mapMaybe readMaybe paths of
      [] -> pure 0
      idxs -> do
        forM_ idxs $ \(idx :: Natural) -> do
          bs <- readFileBS (path </> show idx)
          case decodeFull' bs of
            Left err ->
              fail $ "Failed to decode item: " <> show err
            Right item ->
              atomically $ writeTBQueue queue (idx, item)
        pure $ List.last idxs

-- | Write a value to the queue, blocking if the queue is full.
writePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m) => PersistentQueue m a -> a -> m ()
writePersistentQueue PersistentQueue{queue, nextIx, directory} item = do
  next <- atomically $ do
    next <- readTVar nextIx
    modifyTVar' nextIx (+ 1)
    pure next
  writeFileBS (directory </> show next) $ serialize' item
  atomically $ writeTBQueue queue (next, item)

-- | Get the next value from the queue without removing it, blocking if the
-- queue is empty.
peekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m a
peekPersistentQueue PersistentQueue{queue} = do
  snd <$> atomically (peekTBQueue queue)

-- | Remove an element from the queue if it matches the given item. Use
-- 'peekPersistentQueue' to wait for next items before popping it.
popPersistentQueue :: (MonadSTM m, MonadIO m, Eq a) => PersistentQueue m a -> a -> m ()
popPersistentQueue PersistentQueue{queue, directory} item = do
  popped <- atomically $ do
    (ix, next) <- peekTBQueue queue
    if next == item
      then readTBQueue queue $> Just ix
      else pure Nothing
  case popped of
    Nothing -> pure ()
    Just index -> do
      liftIO . removeFile $ directory </> show index

-- * Tracing

data EtcdLog
  = EtcdLog {etcd :: Value}
  | Reconnecting
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)
