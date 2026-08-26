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
  defaultLogBuffering,
  withTracer,
  withTracerOutputTo,
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
  readTVarIO,
  retry,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (waitCatch)
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

defaultQueueSize :: Natural
defaultQueueSize = 500

-- | Buffering used for log output. The writer batches whatever the queue holds
-- and flushes each batch, so this bounds the syscalls rather than the latency.
defaultLogBuffering :: BufferMode
defaultLogBuffering = BlockBuffering (Just 64000)

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
withTracer (Verbose namespace) = withTracerOutputTo defaultLogBuffering stdout namespace

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
  closed <- newLabelledTVarIO "logging-closed" False
  withAsyncLabelled ("logging-writeLogs", writeLogs msgQueue closed) $ \writer ->
    action (tracer msgQueue) `finally` drainLogs closed writer
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs queue closed = do
    entries <- atomically $ do
      es <- flushTBQueue queue
      -- Block until there is something to write, or exit the loop below by
      -- returning the empty batch once the tracer scope has closed.
      when (null es) $ do
        isClosed <- readTVar closed
        unless isClosed retry
      pure es
    unless (null entries) $ do
      -- Flush once per drained batch, so the block buffer does not hold the
      -- first entries back until 64KB has accumulated.
      --
      -- Losing the batch must not take the node with it: GHC ignores SIGPIPE,
      -- so a reader that goes away turns the next write into an IOException,
      -- and this thread is not linked to its parent. Dying here would go
      -- unnoticed until the queue filled, at which point every 'traceWith' in
      -- the node blocks forever on a queue nobody drains.
      liftIO $
        (forM_ entries (write . Aeson.encode) >> hFlush hdl)
          `catch` \(_ :: IOException) -> pure ()
      writeLogs queue closed

  -- The writer thread claims queued entries before writing them, so shutdown
  -- must hand over to the writer rather than inspect the queue itself: signal
  -- it to stop, wait for it to finish draining, then flush. The wait is
  -- bounded, and the surrounding 'withAsync' cancels a writer that overran it,
  -- but the final flush below is not bounded: a handle whose reader has
  -- stalled can still hold up shutdown until an external signal arrives.
  --
  -- 'waitCatch' rather than 'wait': a writer that died would otherwise rethrow
  -- here, inside a 'finally', and replace whatever actually terminated the
  -- node.
  drainLogs closed writer = liftIO $ do
    atomically $ writeTVar closed True
    void $ timeout drainGraceSeconds (waitCatch writer)
    hFlush hdl `catch` \(_ :: IOException) -> pure ()

  drainGraceSeconds :: DiffTime
  drainGraceSeconds = 5

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
