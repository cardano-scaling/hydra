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
import Data.Aeson (decodeFileStrict', encodeFile, withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base16.Lazy qualified as LBase16
import Data.ByteString.Base64 qualified as Base64
import Data.List qualified as List
import Data.Text.IO qualified as Text
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (..), Network (..), NetworkCallback (..), NetworkComponent, PortNumber)
import Hydra.Node.Network (NetworkConfiguration (..))
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (
  ExitCodeException (..),
  byteStringInput,
  createPipe,
  getStderr,
  getStdout,
  nullStream,
  proc,
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
  Tracer IO Text ->
  NetworkConfiguration msg ->
  NetworkComponent IO msg msg ()
withEtcdNetwork tracer config callback action = do
  withProcessTerm etcdCmd $ \p -> do
    -- Ensure the sub-process is also stopped when we get asked to terminate.
    _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
    -- TODO: error handling
    race_ (waitExitCode p >>= \ec -> die $ "etcd exited with: " <> show ec) $ do
      race_ (traceStderr p) $ do
        race_ (waitMessages clientUrl persistenceDir callback) $ do
          queue <- newPersistentQueue (persistenceDir </> "pending-broadcast") 100
          race_ (broadcastMessages clientUrl port queue) $
            action
              Network
                { broadcast = writePersistentQueue queue
                }
 where
  traceStderr p =
    forever $
      Text.hGetLine (getStderr p) >>= traceWith tracer

  -- XXX: Could use TLS to secure peer connections
  -- XXX: Could use discovery to simplify configuration
  -- NOTE: Configured using guides: https://etcd.io/docs/v3.5/op-guide
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
          , -- XXX: use unique initial-cluster-tokens to isolate clusters
            ["--initial-cluster-token", "hydra-network-1"]
          , ["--initial-cluster", clusterPeers]
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

-- | Broadcast messages from a queue to the etcd cluster.
--
-- Retries on failure to 'putMessage' in case we are on a minority cluster.
broadcastMessages ::
  (ToCBOR msg, Eq msg) =>
  String ->
  PortNumber ->
  PersistentQueue IO msg ->
  IO ()
broadcastMessages endpoint port queue =
  forever $ do
    msg <- peekPersistentQueue queue
    (putMessage endpoint port msg >> popPersistentQueue queue msg)
      `catch` \PutFailed{reason} -> do
        putTextLn $ "put failed: " <> reason
        threadDelay 1

-- | Broadcast a message to the etcd cluster.
-- Throws: 'PutException' if message could not be written to cluster.
-- TODO: Create/use a proper client.
putMessage ::
  (ToCBOR msg, MonadIO m, MonadCatch m) =>
  String ->
  PortNumber ->
  msg ->
  m ()
putMessage endpoint port msg = do
  putKey endpoint key hexMsg
 where
  key = "msg-" <> show port

  hexMsg = LBase16.encode $ serialize msg

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  FromCBOR msg =>
  String ->
  FilePath ->
  NetworkCallback msg IO ->
  IO ()
waitMessages endpoint directory NetworkCallback{deliver} = do
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
    case Aeson.eitherDecodeStrict bs >>= parseEither parseEtcdEntry of
      Left err -> putStrLn $ "Failed to parse etcd entry: " <> err
      Right EtcdEntry{revision, entries} -> do
        putLastKnownRevision directory revision
        forM_ entries $ \(_, value) ->
          -- HACK: lenient decoding
          case decodeFull' $ Base16.decodeLenient value of
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
