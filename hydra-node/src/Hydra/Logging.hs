{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework.
-- All Hydra node components implements /Structured logging/ via [contra-tracer](https://hackage.haskell.org/package/contra-tracer)
-- generic logging framework. Output is emitted as either compact JSON (one line per entry) or a
-- human-friendly, ANSI-coloured form depending on the 'LogFormat' chosen by the caller.
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
  Envelope (..),
  withTracer,
  withTracerOutputTo,
  showLogsOnFailure,
  traceInTVar,
  contramap,
  mkEnvelope,
  defaultQueueSize,

  -- * Per-type classification & pretty rendering
  Severity (..),
  PrettyError (..),
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
import Data.Aeson (Value (..), pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hydra.Logging.PrettyError (
  PrettyError (..),
  Severity (..),
  ansi,
  dim,
 )

data Verbosity = Quiet | Verbose Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Output format for log entries.
data LogFormat
  = -- | One entry per line, encoded as compact JSON. The format used in production
    -- and the only format suitable for log ingestion pipelines.
    LogJSON
  | -- | Human-friendly, ANSI-coloured rendering — timestamp and dotted
    -- message-tag path on one line, then each leaf field on its own indented
    -- @key=value@ line. Values are full JSON (no truncation). Intended for
    -- local development and the dev/smoke runner.
    LogPretty
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
-- messages on @stdout@, one entry at a time, formatted according to the given
-- 'LogFormat'. This tracer wraps each 'msg' into an 'Envelope' with metadata.
withTracer ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg, PrettyError msg) =>
  LogFormat ->
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer _ Quiet = ($ nullTracer)
withTracer fmt (Verbose namespace) =
  withTracerOutputTo fmt (bufferingFor fmt) stdout namespace

-- | Choose a buffering strategy that matches the format: pretty output is for
-- humans reading the terminal, so emit each entry promptly; JSON output is
-- routinely high-volume and benefits from block buffering.
bufferingFor :: LogFormat -> BufferMode
bufferingFor = \case
  LogJSON -> BlockBuffering (Just 64000)
  LogPretty -> LineBuffering

-- | Start logging thread acquiring a 'Tracer', outputting formatted messages
-- to some 'Handle'. Each message is wrapped into an 'Envelope' with metadata.
withTracerOutputTo ::
  forall m msg a.
  (MonadIO m, MonadFork m, MonadTime m, ToJSON msg, PrettyError msg) =>
  LogFormat ->
  BufferMode ->
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputTo fmt bufferingMode hdl namespace action = do
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
      forM_ entries (write . renderEntry fmt)

  flushLogs queue = liftIO $ do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (write . renderEntry fmt)
    hFlush hdl

  write bs = LBS.hPut hdl (bs <> "\n")

-- | Serialise an 'Envelope' according to the chosen 'LogFormat'.
renderEntry :: (ToJSON msg, PrettyError msg) => LogFormat -> Envelope msg -> LBS.ByteString
renderEntry = \case
  LogJSON -> Aeson.encode
  LogPretty -> compactEntry

-- | Render an 'Envelope' for human reading: timestamp + dotted message path
-- on the first line, then each leaf field on its own indented line. Body
-- lines come from the message's 'showPretty' if it has one; otherwise they
-- come from a recursive flatten of the message's JSON shape.
--
-- > HH:MM:SS.sss  Outer.Inner.Leaf
-- >   field1=42
-- >   by=
-- >     vkey="abc…"
--
-- Path colour is picked by 'severity':
--
-- * 'Error'   → bold red
-- * 'Warning' → yellow
-- * 'Info'    → a stable per-top-tag colour (the existing subsystem palette)
compactEntry :: (ToJSON msg, PrettyError msg) => Envelope msg -> LBS.ByteString
compactEntry Envelope{timestamp, message} =
  let ts = toText $ formatTime defaultTimeLocale "%H:%M:%S%3Q" timestamp
      tags = pathTags (Aeson.toJSON message)
      pathTxt = Text.intercalate "." tags
      sev = severity message
      colour = case sev of
        Error -> "1;31" -- bold red
        Warning -> "33" -- yellow
        Info -> colourForTag (firstTag tags)
      -- Prefix Warning/Error lines with an explicit "[warning]"/"[error]"
      -- tag in the same colour as the path. Info lines stay un-tagged —
      -- the path colour already classifies them and most events are Info.
      sevTag = case sev of
        Error -> ansi colour "[error] "
        Warning -> ansi colour "[warning] "
        Info -> ansi colour "[info] "
      headerLine = ansi dim ts <> "  " <> sevTag <> ansi colour pathTxt
      bodyLines = showPretty message
      body =
        if null bodyLines
          then ""
          else "\n" <> Text.intercalate "\n" bodyLines
   in LBS.fromStrict (encodeUtf8 (headerLine <> body))

-- | First tag of a walked path, or empty if there isn't one.
firstTag :: [Text] -> Text
firstTag = \case
  t : _ -> t
  [] -> ""

-- | Walk a 'Value' through single-tagged-field wrappers, returning the
-- chain of @tag@s (oldest first).
pathTags :: Value -> [Text]
pathTags = \case
  Object km
    | Just (String t) <- KeyMap.lookup "tag" km ->
        let rest = KeyMap.delete "tag" km
         in case singleTaggedField rest of
              Just inner -> t : pathTags inner
              Nothing -> [t]
  _ -> []
 where
  singleTaggedField km
    | [(_, v@(Object km'))] <- KeyMap.toList km
    , KeyMap.member "tag" km' =
        Just v
    | otherwise = Nothing

-- | Pick a stable ANSI colour for the path, keyed off the outermost tag so
-- a whole subsystem stays the same colour across lines.
colourForTag :: Text -> Text
colourForTag = \case
  "DirectChain" -> "35" -- magenta
  "APIServer" -> "32" -- green
  "Network" -> "34" -- blue
  "Node" -> "36" -- cyan
  "NodeOptions" -> "36" -- cyan
  "NodeHydrated" -> "36" -- cyan
  "EnteringMainloop" -> "36" -- cyan
  "ChainBackendStarted" -> "36" -- cyan
  "NetworkStarted" -> "36" -- cyan
  "SQLite" -> "33" -- yellow
  "FromCardanoNode" -> "31" -- red
  "FromFaucet" -> "33" -- yellow
  "FromHydraNode" -> "36" -- cyan
  "FromMithril" -> "35" -- magenta
  "ClusterOptions" -> "37" -- white
  _ -> "37"

-- | Capture logs and output them to stdout when an exception was raised by the
-- given 'action'. This tracer is wrapping 'msg' into an 'Envelope' with
-- metadata. Always emits JSON: this path is used by tests and ingestion-style
-- callers where machine-readable output is the right default.
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
