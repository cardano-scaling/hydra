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
  LogFormat (..),
  readLogFormat,
  Envelope (..),
  withTracer,
  withTracerFormat,
  withTracerOutputTo,
  withTracerOutputToFormat,
  encodeEnvelopeJson,
  encodeEnvelopeCbor,
  showLogsOnFailure,
  traceInTVar,
  contramap,
  mkEnvelope,
  defaultQueueSize,
) where

import Hydra.Prelude

import Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
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

data Verbosity = Quiet | Verbose Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR Verbosity where
  toCBOR = \case
    Quiet -> toCBOR ("Quiet" :: Text)
    Verbose namespace -> toCBOR ("Verbose" :: Text) <> toCBOR namespace

instance FromCBOR Verbosity where
  fromCBOR =
    fromCBOR >>= \case
      ("Quiet" :: Text) -> pure Quiet
      "Verbose" -> Verbose <$> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded Verbosity"

-- | Format of the log stream written by the tracer: newline-delimited JSON
-- (the default) or a CBOR sequence (RFC 8742).
data LogFormat = JsonFormat | CborFormat
  deriving stock (Eq, Show, Generic)

instance ToJSON LogFormat where
  toJSON = \case
    JsonFormat -> Aeson.String "json"
    CborFormat -> Aeson.String "cbor"

instance FromJSON LogFormat where
  parseJSON = Aeson.withText "LogFormat" $ \case
    "json" -> pure JsonFormat
    "cbor" -> pure CborFormat
    other -> fail $ "expected \"json\" or \"cbor\", got " <> show other

instance ToCBOR LogFormat where
  toCBOR = \case
    JsonFormat -> toCBOR ("json" :: Text)
    CborFormat -> toCBOR ("cbor" :: Text)

instance FromCBOR LogFormat where
  fromCBOR =
    fromCBOR >>= \case
      ("json" :: Text) -> pure JsonFormat
      "cbor" -> pure CborFormat
      tag -> fail $ show tag <> " is not a proper CBOR-encoded LogFormat"

-- | Parse a 'LogFormat' from a string, as used in the @--log-format@ option.
readLogFormat :: String -> Either String LogFormat
readLogFormat = \case
  "json" -> Right JsonFormat
  "cbor" -> Right CborFormat
  other -> Left $ "expected \"json\" or \"cbor\", got " <> show other

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

instance (Typeable a, ToCBOR a) => ToCBOR (Envelope a) where
  toCBOR Envelope{timestamp, threadId, namespace, message} =
    toCBOR timestamp <> toCBOR threadId <> toCBOR namespace <> toCBOR message

instance (Typeable a, FromCBOR a) => FromCBOR (Envelope a) where
  fromCBOR = Envelope <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

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

-- | Like 'withTracer', but writing log entries in the given 'LogFormat':
-- newline-delimited JSON or a CBOR sequence (RFC 8742). CBOR logs can be
-- converted back to JSON with @hydra-node convert-logs@.
withTracerFormat ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg, ToCBOR msg) =>
  LogFormat ->
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerFormat _ Quiet = ($ nullTracer)
withTracerFormat format (Verbose namespace) =
  withTracerOutputToFormat format (BlockBuffering (Just 64000)) stdout namespace

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
withTracerOutputTo = withTracerOutputWith encodeEnvelopeJson

-- | Like 'withTracerOutputTo', but writing log entries in the given
-- 'LogFormat'.
withTracerOutputToFormat ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg, ToCBOR msg) =>
  LogFormat ->
  BufferMode ->
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputToFormat = \case
  JsonFormat -> withTracerOutputWith encodeEnvelopeJson
  CborFormat -> withTracerOutputWith encodeEnvelopeCbor

-- | Encode a log entry as a JSON line (newline-terminated).
encodeEnvelopeJson :: ToJSON msg => Envelope msg -> LBS.ByteString
encodeEnvelopeJson e = Aeson.encode e <> "\n"

-- | Encode a log entry as a self-delimiting CBOR item, prefixed with tag
-- 55799 (\"self-described CBOR\", bytes @D9 D9 F7@). The tag doubles as a
-- file magic for format auto-detection and as a resync marker; concatenated
-- items form an RFC 8742 CBOR sequence, so appending and concatenating log
-- files remains safe.
encodeEnvelopeCbor :: ToCBOR msg => Envelope msg -> LBS.ByteString
encodeEnvelopeCbor e = CBOR.toLazyByteString (CBOR.encodeTag 55799 <> toCBOR e)

-- | Internal helper: start the logging thread with a given line encoder.
withTracerOutputWith ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m) =>
  (Envelope msg -> LBS.ByteString) ->
  BufferMode ->
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputWith encodeLine bufferingMode hdl namespace action = do
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
      forM_ entries (write . encodeLine)

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . encodeLine)
    hFlush hdl

  write = LBS.hPut hdl

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
