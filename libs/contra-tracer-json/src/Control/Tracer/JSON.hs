{-# LANGUAGE UndecidableInstances #-}

-- | Structured JSON logging over the
-- [contra-tracer](https://hackage.haskell.org/package/contra-tracer) generic
-- logging framework. A 'Tracer' acquired here wraps each message in an
-- 'Envelope' carrying a timestamp, thread id and namespace, and writes it as
-- one JSON object per line.
module Control.Tracer.JSON (
  -- * Tracer
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
  contramap,

  -- * Using it
  Verbosity (..),
  Envelope (..),
  defaultLogBuffering,
  withTracer,
  withTracerOutputTo,
  showLogsOnFailure,
  traceInTVar,
  mkEnvelope,
  defaultQueueSize,
) where

import Control.Concurrent.Class.Labelled (newLabelledTBQueueIO, newLabelledTVarIO, withAsyncLabelled)
import Control.Concurrent.Class.MonadSTM (
  MonadLabelledSTM,
  MonadSTM,
  TVar,
  atomically,
  flushTBQueue,
  modifyTVar,
  readTVar,
  readTVarIO,
  retry,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (AsyncException (HeapOverflow, StackOverflow), IOException, SomeAsyncException, SomeException, displayException, evaluate, fromException, throwIO)
import Control.Monad (forM_, unless, void, when, (>=>))
import Control.Monad.Class.MonadAsync (waitCatch)
import Control.Monad.Class.MonadFork (MonadFork, myThreadId)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Monad.Class.MonadThrow (MonadCatch, catch, finally, onException)
import Control.Monad.Class.MonadTime.SI (MonadTime, getCurrentTime)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (Tracer (..), natTracer, nullTracer, traceWith)
import Data.Aeson (FromJSON, ToJSON (..), pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant (contramap)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (DiffTime, UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.IO (BufferMode (BlockBuffering), Handle, hFlush, hSetBuffering, stdout)
import Text.Read (readMaybe)

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
        (forM_ entries (encodeEntry >=> write) >> hFlush hdl)
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

  -- Encode and force, so a failure surfaces to the caller's handler rather than
  -- later, wherever the lazy result happens to be consumed.
  forceEncoded :: ToJSON entry => entry -> IO LBS.ByteString
  forceEncoded x = let bytes = Aeson.encode x in bytes <$ evaluate (LBS.length bytes)

  -- Run the substitute on any synchronous failure; cancellation still
  -- propagates.
  orSubstitute :: IO r -> (SomeException -> IO r) -> IO r
  orSubstitute attempt substitute =
    attempt `catch` \e -> if isCancellation e then throwIO e else substitute e

  -- Only what a canceller raises.
  --
  -- 'SomeAsyncException' alone is too broad: 'AsyncException's own 'Exception'
  -- instance wraps itself in it, so 'StackOverflow' and 'HeapOverflow' match as
  -- well -- and those come out of the encoding work rather than from anybody
  -- cancelling us. Rethrowing them would kill the writer on exactly the kind of
  -- entry it is meant to survive, so they are excluded by name. 'ThreadKilled'
  -- and 'UserInterrupt' do mean stop; so does anything else async, such as
  -- 'AsyncCancelled' from the surrounding 'withAsync'.
  isCancellation :: SomeException -> Bool
  isCancellation e =
    case fromException e :: Maybe AsyncException of
      Just StackOverflow -> False
      Just HeapOverflow -> False
      Just _ -> True
      Nothing -> isJust (fromException e :: Maybe SomeAsyncException)

  -- Last resort, when even the diagnostic entry cannot be encoded. A constant,
  -- so it has nothing left to fail on.
  unencodableFallback :: LBS.ByteString
  unencodableFallback = "{\"message\":{\"tag\":\"UnencodableLogEntry\"}}"

  -- Encode one entry, forcing it here so that a partial 'ToJSON' cannot take
  -- this thread down.
  --
  -- A 'ToJSON' instance reachable from a traced type can be partial: it may
  -- force a value whose computation calls 'error' (for example a lazily cached
  -- cryptographic commitment that is only computable for inputs below some
  -- size). Left unguarded, that exception surfaces here rather than at the
  -- 'traceWith' call site, and this thread dying is much worse than one lost
  -- log line: it is deliberately not linked to its parent, so nothing notices
  -- until the bounded queue fills, at which point every 'traceWith' in the
  -- process blocks forever on a queue nobody drains. Substitute a diagnostic
  -- entry for the one that cannot be encoded and keep the loop alive.
  --
  -- Cancellation is rethrown: the surrounding 'withAsync' must still be able to
  -- stop this thread.
  encodeEntry :: Envelope msg -> IO LBS.ByteString
  encodeEntry envelope =
    forceEncoded envelope `orSubstitute` \e ->
      -- The substitute is forced here too. Returning it lazily would leave it to
      -- be encoded by 'write', outside this handler, and it can fail in turn:
      -- 'displayException' on an exception whose 'show' is partial throws, which
      -- is precisely the escape this function exists to prevent. If even that
      -- fails, fall back to constant bytes, which cannot.
      forceEncoded (unencodable e) `orSubstitute` \_ -> pure unencodableFallback
   where
    Envelope{timestamp, threadId} = envelope

    unencodable :: SomeException -> Envelope Aeson.Value
    unencodable e =
      Envelope
        { timestamp
        , threadId
        , namespace
        , message =
            Aeson.object
              [ "tag" .= ("UnencodableLogEntry" :: Text)
              , "reason" .= Text.pack (displayException e)
              ]
        }

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
    `onException` (readTVarIO tvar >>= mapM_ (say . Text.unpack . decodeUtf8 . LBS.toStrict . Aeson.encode) . reverse)

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
  mkThreadId = fromMaybe 0 . readMaybe . drop 9 . show
