{-# LANGUAGE OverloadedStrings #-}

-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
--
-- Broadcasting messages is implemented using 'put's to some well-known key,
-- while message delivery is done using 'watch' on the same key.
--
-- Uses a 'PersistentQueue' which stores messages to broadcast in the
-- persistence dir to be resilient against crashes.
--
-- The last known revision is also stored in the persistence dir to only deliver
-- messages that were not seen before.
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize, serialize')
import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  newTBQueueIO,
  newTVarIO,
  peekTBQueue,
  readTBQueue,
  writeTBQueue,
 )
import Control.Exception (IOException)
import Control.Lens ((^..))
import Data.Aeson (decodeFileStrict', encodeFile, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (_String)
import Data.Aeson.Lens qualified as Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base16.Lazy qualified as LBase16
import Data.ByteString.Base64 qualified as Base64
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
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (
  ExitCode (..),
  ExitCodeException (..),
  byteStringInput,
  createPipe,
  getStderr,
  getStdout,
  nullStream,
  proc,
  readProcessStdout,
  readProcessStdout_,
  runProcess_,
  setStderr,
  setStdin,
  setStdout,
  stopProcess,
  waitExitCode,
  withProcessTerm,
  withProcessWait,
 )

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg, Eq msg) =>
  Tracer IO Value ->
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
    -- TODO: error handling
    race_ (waitExitCode p >>= \ec -> fail $ "etcd exited with: " <> show ec) $ do
      race_ (traceStderr p) $ do
        race_ (pollMembers clientUrl callback) $
          race_ (waitMessages clientUrl protocolVersion persistenceDir callback) $ do
            queue <- newPersistentQueue (persistenceDir </> "pending-broadcast") 100
            race_ (broadcastMessages clientUrl protocolVersion port queue) $
              action
                Network
                  { broadcast = writePersistentQueue queue
                  }
 where
  traceStderr p =
    forever $ do
      bs <- BS.hGetLine (getStderr p)
      case Aeson.eitherDecodeStrict bs of
        Left err -> fail $ "Failed to decode etcd log: " <> show err
        Right v -> traceWith tracer v

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

  -- NOTE: Offset client port by the same amount as configured 'port' is offset
  -- from the default '5001'. This will result in the default client port 2379
  -- be used by default still.
  clientUrl = httpUrl Host{hostname = show host, port = 2379 + port - 5001}

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
-- Retries on failure to 'putMessage' in case we are on a minority cluster.
broadcastMessages ::
  (ToCBOR msg, Eq msg) =>
  String ->
  HydraVersionedProtocolNumber ->
  PortNumber ->
  PersistentQueue IO msg ->
  IO ()
broadcastMessages endpoint protocolVersion port queue =
  forever $ do
    msg <- peekPersistentQueue queue
    (putMessage endpoint protocolVersion port msg >> popPersistentQueue queue msg)
      `catch` \PutFailed{reason} -> do
        putTextLn $ "put failed: " <> reason
        threadDelay 1

-- | Broadcast a message to the etcd cluster.
-- Throws: 'PutException' if message could not be written to cluster.
-- TODO: Create/use a proper client.
putMessage ::
  (ToCBOR msg, MonadIO m, MonadCatch m) =>
  String ->
  HydraVersionedProtocolNumber ->
  PortNumber ->
  msg ->
  m ()
putMessage endpoint protocolVersion port msg = do
  putKey endpoint key hexMsg
 where
  key = "msg-" <> show (hydraVersionedProtocolNumber protocolVersion) <> "-" <> show port

  hexMsg = LBase16.encode $ serialize msg

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  FromCBOR msg =>
  String ->
  HydraVersionedProtocolNumber ->
  FilePath ->
  NetworkCallback msg IO ->
  IO ()
waitMessages endpoint protocolVersion directory NetworkCallback{deliver, onConnectivity} = do
  revision <- getLastKnownRevision directory
  forever $ do
    -- Watch all key value updates
    withProcessWait (cmd revision) process
    -- Wait before reconnecting
    threadDelay 1
 where
  cmd revision =
    setStdout createPipe $
      proc "etcdctl" $
        concat
          [ ["--endpoints", endpoint]
          , ["watch"]
          , ["--prefix", "msg"]
          , ["--rev", show $ revision + 1]
          , ["-w", "json"]
          ]

  process p = do
    bs <- BS.hGetLine (getStdout p)
    -- FIXME: this is blocking if we can connect and nothing happens. Use proper client
    case Aeson.eitherDecodeStrict bs >>= parseEither parseEtcdEntry of
      -- TODO: error handling (with actual client)
      Left err -> putStrLn $ "Failed to parse etcd entry: " <> err <> "\n" <> show bs
      Right EtcdEntry{revision, entries} -> do
        putLastKnownRevision directory revision
        forM_ entries $ \(key, value) -> do
          -- XXX: Check version on every watch event?
          case matchVersion key protocolVersion of
            Nothing ->
              pure ()
            Just HydraHandshakeRefused{remoteHost, ourVersion, theirVersions} ->
              onConnectivity $ HandshakeFailure{remoteHost, ourVersion, theirVersions}

          -- HACK: lenient decoding
          case decodeFull' $ Base16.decodeLenient value of
            -- TODO: error handling (with actual client)
            Left err -> fail $ "Failed to decode etcd entry: " <> show err
            Right msg -> do
              deliver msg
        process p

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

-- | Poll for member list to yield connectivity events every second. Note that
-- the actual list of members is not relevant for connectivity as it just lists
-- who _should_ be connected.
pollMembers ::
  String ->
  NetworkCallback msg IO ->
  IO ()
pollMembers endpoint NetworkCallback{onConnectivity} = do
  forever $ do
    (exitCode, out) <- readProcessStdout cmd
    case exitCode of
      ExitSuccess -> do
        let members = out ^.. Aeson.key "members" . Aeson.values . Aeson.key "name" . _String
        putTextLn $ "Current members: " <> show members
        -- XXX: The member list is not indicating connectivity
        onConnectivity $ Connected "" -- TODO: node id does not matter
      _ -> do
        -- TODO: tracing
        putTextLn $ "Failed to get members: " <> show exitCode
        onConnectivity $ Disconnected "" -- TODO: node id does not matter
        -- Wait before retrying
    threadDelay 1
 where
  cmd =
    setStdout createPipe $
      proc "etcdctl" $
        concat
          [ ["--endpoints", endpoint]
          , ["member", "list"]
          , ["-w", "json"]
          , ["--command-timeout", "1s"]
          ]

-- * Low-level etcd api

-- | Write a value at a key to the etcd cluster.
-- Throws: 'PutException' if value could not be written to cluster.
-- TODO: Create/use a proper client.
putKey ::
  (MonadIO m, MonadCatch m) =>
  String ->
  -- | Key
  String ->
  -- | Value
  LByteString ->
  m ()
putKey endpoint key value =
  handle (throwIO . exitCodeToException) $
    runProcess_ $
      proc "etcdctl" ["--endpoints", endpoint, "put", key]
        & setStdin (byteStringInput value)
        & setStdout nullStream
 where
  exitCodeToException :: ExitCodeException -> PutException
  exitCodeToException ec = PutFailed{reason = decodeUtf8 $ eceStdout ec}

newtype PutException = PutFailed {reason :: Text}
  deriving stock (Show)

instance Exception PutException

data EtcdEntry = EtcdEntry
  { revision :: Natural
  , entries :: [(ByteString, ByteString)]
  }
  deriving (Show)

parseEtcdEntry :: Value -> Parser EtcdEntry
parseEtcdEntry = withObject "EtcdEntry" $ \o -> do
  revision <- o .: "Header" >>= (.: "revision")
  entries <- o .: "Events" >>= mapM parseEvent
  pure EtcdEntry{revision, entries}
 where
  parseEvent = withObject "Event" $ \o ->
    o .: "kv" >>= parseKV

  parseKV = withObject "kv" $ \o -> do
    key <- parseBase64 =<< o .: "key"
    value <- parseBase64 =<< o .: "value"
    pure (key, value)

-- HACK: lenient decoding
parseBase64 :: Text -> Parser ByteString
parseBase64 = either fail pure . Base64.decode . encodeUtf8

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
