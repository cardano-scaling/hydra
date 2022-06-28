{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework.
-- All Hydra node components implements /Structured logging/ via [contra-tracer](https://hackage.haskell.org/package/contra-tracer)
-- generic logging framework. All logs are output in [JSON](https://www.json.org/json-en.html) in a format which is
-- documented in a [JSON-Schema](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-node/json-schemas/logs.yaml).
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
) where

import Hydra.Prelude

import Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import Control.Monad.Class.MonadFork (myThreadId)
import Control.Monad.Class.MonadSTM (
  flushTBQueue,
  modifyTVar,
  newTBQueueIO,
  newTVarIO,
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text

data Verbosity = Quiet | Verbose Text
  deriving (Eq, Show)

-- | Provides logging metadata for entries.
data Envelope a = Envelope
  { timestamp :: UTCTime
  , threadId :: Int
  , namespace :: Text
  , message :: a
  }
  deriving (Eq, Show, Generic, FromJSON)

instance ToJSON a => ToJSON (Envelope a) where
  toEncoding Envelope{timestamp, threadId, namespace, message} =
    pairs $
      mconcat
        [ "timestamp" .= timestamp
        , "threadId" .= threadId
        , "namespace" .= namespace
        , "message" .= message
        ]

instance Arbitrary a => Arbitrary (Envelope a) where
  arbitrary = genericArbitrary

defaultQueueSize :: Natural
defaultQueueSize = 500

-- | Start logging thread and acquire a 'Tracer'. This tracer will dump all
-- messsages on @stdout@, one message per line, formatted as JSON. This tracer
-- is wrapping 'msg' into an 'Envelope' with metadata.
withTracer ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet = ($ nullTracer)
withTracer (Verbose namespace) = withTracerOutputTo stdout namespace

-- | Start logging thread acquiring a 'Tracer', outputting JSON formatted
-- messages to some 'Handle'. This tracer is wrapping 'msg' into an 'Envelope'
-- with metadata.
withTracerOutputTo ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg) =>
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputTo hdl namespace action = do
  msgQueue <- newTBQueueIO @_ @(Envelope msg) defaultQueueSize
  withAsync (writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer $
      mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

  writeLogs queue =
    forever $ do
      atomically (readTBQueue queue) >>= write . Aeson.encode
      hFlush hdl

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . Aeson.encode)
    hFlush hdl

  write bs = LBS.hPut hdl (bs <> "\n")

-- | Capture logs and output them to stdout when an exception was raised by the
-- given 'action'. This tracer is wrapping 'msg' into an 'Envelope' with
-- metadata.
showLogsOnFailure ::
  (MonadSTM m, MonadCatch m, MonadFork m, MonadTime m, MonadSay m, ToJSON msg) =>
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure action = do
  tvar <- newTVarIO []
  action (traceInTVar tvar)
    `onException` (readTVarIO tvar >>= mapM_ (say . decodeUtf8 . Aeson.encode) . reverse)

traceInTVar ::
  (MonadFork m, MonadTime m, MonadSTM m) =>
  TVar m [Envelope msg] ->
  Tracer m msg
traceInTVar tvar = Tracer $ \msg -> do
  envelope <- mkEnvelope "" msg
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
