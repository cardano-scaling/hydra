{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework.
-- All Hydra node components implements /Structured logging/ via [contra-tracer](https://hackage.haskell.org/package/contra-tracer)
-- generic logging framework. All logs are output in [JSON](https://www.json.org/json-en.html).
module Hydra.Logging (
  -- * Tracer
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
  ToObject (..),
  TracingVerbosity (..),

  -- * Using it
  Verbosity (..),
  Envelope (..),
  withTracer,
  withTracerStdout,
  withTracerLogType,
  withTracerOutputTo,
  withTracerLogFile,
  showLogsOnFailure,
  traceInTVar,
  contramap,
  mkEnvelope,
  defaultQueueSize,
) where

import Hydra.Prelude

import Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import Control.Concurrent.Class.MonadSTM (
  flushTBQueue,
  modifyTVar,
  readTBQueue,
  readTVarIO,
  writeTBQueue,
 )
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Tracer (
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
 )
import Data.Aeson (pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import System.Log.FastLogger (LogStr, LogType, LogType' (LogFileNoRotate, LogStdout), ToLogStr, defaultBufSize, newFastLogger, toLogStr)

data Verbosity = Quiet | Verbose Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Provides logging metadata for entries.
data Envelope a = Envelope
  { timestamp :: UTCTime
  , threadId :: Int
  , namespace :: Text
  , message :: a
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON a => ToJSON (Envelope a) where
  toEncoding Envelope{timestamp, threadId, namespace, message} =
    pairs $
      mconcat
        [ "timestamp" .= timestamp
        , "threadId" .= threadId
        , "namespace" .= namespace
        , "message" .= message
        ]

instance FromJSON a => FromJSON (Envelope a) where
  parseJSON = Aeson.withObject "Envelope" $ \o -> do
    timestamp <- o Aeson..: "timestamp"
    threadId <- o Aeson..: "threadId"
    namespace <- o Aeson..: "namespace"
    message <- o Aeson..: "message"
    pure Envelope{timestamp, threadId, namespace, message}

defaultQueueSize :: Natural
defaultQueueSize = 500

-- | Start logging thread and acquire a 'Tracer'. This tracer will dump all
-- messages on @stdout@, one message per line, formatted as JSON. This tracer
-- is wrapping 'msg' into an 'Envelope' with metadata.
withTracer ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet = ($ nullTracer)
withTracer (Verbose namespace) = withTracerStdout namespace

-- | Log to stdout using fast-logger with a default buffer size.
withTracerStdout ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerStdout = withTracerLogType (LogStdout defaultBufSize)

-- | Log to a file path using fast-logger without rotation.
withTracerLogFile ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  FilePath ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerLogFile filepath =
  withTracerLogType (LogFileNoRotate filepath defaultBufSize)

-- | Log using a fast-logger 'LogType' (stdout/stderr/file/rotation).
withTracerLogType ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  LogType ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerLogType logType namespace action = do
  (logger, cleanup) <- newFastLogger logType
  msgQueue <- newLabelledTBQueueIO @_ @(Envelope msg) "logging-msg-queue" defaultQueueSize
  withAsyncLabelled ("logging-writeLogs", writeLogs logger msgQueue) $ \_ ->
    action (natTracer liftIO (tracer msgQueue)) `finally` (flushLogs logger msgQueue >> cleanup)
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs :: (LogStr -> IO ()) -> TBQueue IO (Envelope msg) -> IO ()
  writeLogs logger queue =
    forever $ do
      entries <- atomically $ do
        firstEntry <- readTBQueue queue
        rest <- flushTBQueue queue
        pure (firstEntry : rest)
      forM_ entries (write logger . Aeson.encode)

  flushLogs :: (LogStr -> IO ()) -> TBQueue IO (Envelope msg) -> IO ()
  flushLogs logger queue = do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write logger . Aeson.encode)

  write logger bs = logger (toLogStr bs <> "\n")

-- | Start logging to a provided handle; used for benchmarks and tests.
-- Prefer 'withTracerLogFile' or 'withTracerStdout' for production paths
-- TODO: delete
withTracerOutputTo ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputTo hdl namespace action = do
  hSetBuffering hdl (BlockBuffering (Just 64000))
  msgQueue <- newLabelledTBQueueIO @_ @(Envelope msg) "logging-msg-queue" defaultQueueSize
  withAsyncLabelled ("logging-writeLogs", writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs queue =
    forever $ do
      entries <- atomically $ do
        firstEntry <- readTBQueue queue
        rest <- flushTBQueue queue
        pure (firstEntry : rest)
      forM_ entries (write . Aeson.encode)

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . Aeson.encode)
    hFlush hdl

  write bs = LBS.hPut hdl (bs <> "\n")

-- | Capture logs and output them to stdout when an exception was raised by the
-- given 'action'. This tracer is wrapping 'msg' into an 'Envelope' with
-- metadata.
showLogsOnFailure ::
  (MonadLabelledSTM m, MonadCatch m, MonadFork m, MonadTime m, MonadSay m, ToJSON msg) =>
  Text ->
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure namespace action = do
  tvar <- newLabelledTVarIO "show-logs-on-failure" []
  action (traceInTVar tvar namespace)
    `onException` (readTVarIO tvar >>= mapM_ (say . decodeUtf8 . Aeson.encode) . reverse)

traceInTVar ::
  (MonadFork m, MonadTime m, MonadSTM m) =>
  TVar m [Envelope msg] ->
  Text ->
  Tracer m msg
traceInTVar tvar namespace = Tracer $ \msg -> do
  envelope <- mkEnvelope namespace msg
  atomically $ modifyTVar tvar (envelope :)
-- * Internal functions

mkEnvelope :: (MonadFork m, MonadTime m) => Text -> msg -> m (Envelope msg)
mkEnvelope namespace message = do
  timestamp <- getCurrentTime
  threadId <- mkThreadId <$> myThreadId
  pure $ Envelope{namespace, timestamp, threadId, message}
 where
  -- NOTE(AB): This is a bit contrived but we want a numeric threadId and we
  -- get some text which we know the structure of
  mkThreadId = fromMaybe 0 . readMaybe . Text.unpack . Text.drop 9 . show
