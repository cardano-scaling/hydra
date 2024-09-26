{-# LANGUAGE OverloadedStrings #-}

-- | Implements a Hydra network component via an etcd cluster.
--
-- While this is quite an overkill, the Raft consensus of etcd provides our
-- application with a crash-recovery fault-tolerant "atomic broadcast".
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize)
import Control.Concurrent.Class.MonadSTM (MonadSTM (readTBQueue, unGetTBQueue), newTBQueueIO, writeTBQueue)
import Data.Aeson (withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value, parseEither)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Base16.Lazy qualified as LBase16
import Data.ByteString.Base64 qualified as Base64
import Data.Text.IO qualified as Text
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (..), Network (..), NetworkCallback (..), NetworkComponent, PortNumber)
import Hydra.Node.Network (NetworkConfiguration (..))
import System.FilePath ((</>))
import System.Posix (Handler (Catch), installHandler, sigTERM)
import System.Process.Typed (ExitCodeException (..), byteStringInput, createPipe, getStderr, getStdout, proc, readProcessStdout_, runProcess_, setStderr, setStdin, setStdout, stopProcess, waitExitCode, withProcessTerm, withProcessWait)

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO Text ->
  NetworkConfiguration msg ->
  NetworkComponent IO msg msg ()
withEtcdNetwork tracer config callback action = do
  withProcessTerm etcdCmd $ \p -> do
    -- Ensure the sub-process is also stopped when we get asked to terminate.
    _ <- installHandler sigTERM (Catch $ stopProcess p) Nothing
    -- TODO: error handling
    race_ (waitExitCode p >>= \ec -> die $ "etcd exited with: " <> show ec) $ do
      -- HACK: give etcd a bit time to start up and do leader election etc.
      putStrLn "Give etcd a bit time.."
      threadDelay 2
      race_ (traceStderr p) $
        race_ (waitMessages clientUrl callback) $ do
          queue <- newTBQueueIO 100
          race_ (broadcastMessages clientUrl port queue) $
            action
              Network
                { broadcast = atomically . writeTBQueue queue
                }
 where
  traceStderr p =
    forever $
      Text.hGetLine (getStderr p) >>= traceWith tracer

  -- TODO: use TLS to secure peer connections
  -- TODO: use discovery to simplify configuration
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
--
-- TODO: use a persistent queue
broadcastMessages :: ToCBOR msg => String -> PortNumber -> TBQueue IO msg -> IO ()
broadcastMessages endpoint port queue =
  forever $ do
    msg <- atomically $ readTBQueue queue
    putMessage endpoint port msg
      `catch` \PutFailed{reason} -> do
        atomically $ unGetTBQueue queue msg
        putTextLn $ "put failed: " <> reason
        threadDelay 1

-- | Broadcast a message to the etcd cluster.
-- Throws: 'PutMessageException' if message could not be written to cluster.
-- HACK: Create/use a proper client.
putMessage ::
  (ToCBOR msg, MonadIO m, MonadCatch m) =>
  String ->
  PortNumber ->
  msg ->
  m ()
putMessage endpoint port msg = do
  handle (throwIO . exitCodeToException) $
    runProcess_ $
      proc "etcdctl" ["--endpoints", endpoint, "put", key]
        & setStdin (byteStringInput hexMsg)
 where
  -- FIXME: use different keys per message types?
  key = "foo-" <> show port

  hexMsg = LBase16.encode $ serialize msg

  exitCodeToException :: ExitCodeException -> PutMessageException
  exitCodeToException ec = PutFailed{reason = decodeUtf8 $ eceStdout ec}

newtype PutMessageException = PutFailed {reason :: Text}
  deriving stock (Show)

instance Exception PutMessageException

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  FromCBOR msg =>
  String ->
  NetworkCallback msg IO ->
  IO ()
waitMessages endpoint NetworkCallback{deliver} = do
  forever $ do
    -- Watch all key value updates
    withProcessWait cmd process
    -- Wait before reconnecting
    threadDelay 1
 where
  -- FIXME: different key/prefixes
  key = "foo"

  cmd =
    proc "etcdctl" ["--endpoints", endpoint, "watch", "--prefix", key, "-w", "json"]
      & setStdout createPipe

  process p = do
    bs <- BS.hGetLine (getStdout p)
    case Aeson.eitherDecodeStrict (spy' "got" bs) >>= parseEither parseEtcdEntry of
      Left err -> putStrLn $ "Failed to parse etcd entry: " <> err
      Right e@EtcdEntry{entries} -> do
        print e
        forM_ entries $ \(_, value) ->
          -- HACK: lenient decoding
          case decodeFull' $ Base16.decodeLenient value of
            Left err -> fail $ "Failed to decode etcd entry: " <> show err
            Right msg -> do
              deliver msg
        process p

getKey :: MonadIO m => String -> String -> m EtcdEntry
getKey endpoint key = do
  -- XXX: error handling
  out <- readProcessStdout_ $ proc "etcdctl" ["--endpoints", endpoint, "-w", "json", "get", key]
  case Aeson.eitherDecode out >>= parseEither parseEtcdEntry of
    Left err -> die $ "Failed to parse etcd entry: " <> err
    Right entry -> pure entry

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
