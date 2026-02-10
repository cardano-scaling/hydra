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
  withTracerOutputTo,
  showLogsOnFailure,
  traceInTVar,
  contramap,
  mkEnvelope,
  defaultQueueSize,
) where

import Hydra.Prelude

import "aeson" Data.Aeson (pairs, (.=))
import "aeson" Data.Aeson qualified as Aeson
import "bytestring" Data.ByteString.Lazy qualified as LBS
import "contra-tracer" Control.Tracer (
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
 )
import "io-classes" Control.Concurrent.Class.MonadSTM (
  flushTBQueue,
  modifyTVar,
  readTBQueue,
  readTVarIO,
  writeTBQueue,
 )
import "io-classes" Control.Monad.Class.MonadSay (MonadSay, say)
import "iohk-monitoring" Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import "text" Data.Text qualified as Text

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
withTracer (Verbose namespace) = withTracerOutputTo (BlockBuffering (Just 64000)) stdout namespace

-- | Start logging thread acquiring a 'Tracer', outputting JSON formatted
-- messages to some 'Handle'. This tracer is wrapping 'msg' into an 'Envelope'
-- with metadata.
withTracerOutputTo ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  BufferMode ->
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputTo bufferingMode hdl namespace action = do
  hSetBuffering hdl bufferingMode
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
