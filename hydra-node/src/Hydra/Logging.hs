{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework. For now we are using the
-- iohk-monitoring package, but that might change soon.
module Hydra.Logging (
  -- * Tracer
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
  traceInTVar,
  ToObject (..),
  TracingVerbosity (..),

  -- * Using it
  Verbosity (..),
  withTracer,
  contramap,
  showLogsOnFailure,
) where

import Hydra.Prelude

import Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import Control.Monad.Class.MonadFork (myThreadId)
import Control.Monad.Class.MonadSTM (flushTBQueue, modifyTVar, newTBQueueIO, newTVarIO, readTVarIO, writeTBQueue)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Tracer (
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
 )
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO

data Verbosity = Quiet | Verbose Text
  deriving (Eq, Show)

defaultQueueSize :: Natural
defaultQueueSize = 500

-- | Start logging thread and acquire a 'Tracer'.
-- This tracer will dump all messsages on @stdout@, one message per line,
-- formatted as JSON.
withTracer ::
  forall m msg a.
  (MonadIO m, ToJSON msg) =>
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet action = action nullTracer
withTracer (Verbose name) action = do
  msgQueue <- newTBQueueIO @_ @Value defaultQueueSize
  withAsync (logMessagesToStdout msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer
      ( \msg ->
          liftIO (mkEnvelop msg >>= atomically . writeTBQueue queue)
      )

  mkEnvelop :: msg -> IO Value
  mkEnvelop msg = do
    timestamp <- liftIO getCurrentTime
    threadId <- Text.drop 9 . show <$> liftIO myThreadId
    pure $
      object
        [ "namespace" .= name
        , "timestamp" .= timestamp
        , "thread" .= threadId
        , "message" .= msg
        ]

  logMessagesToStdout queue =
    forever $
      flushLogs queue >> threadDelay 0.1

  flushLogs queue = do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (TIO.putStrLn . decodeUtf8With lenientDecode . LBS.toStrict . encode)
    hFlush stdout

traceInTVar :: MonadSTM m => TVar m [a] -> Tracer m a
traceInTVar tvar = Tracer $ \a ->
  atomically $ modifyTVar tvar (a :)

showLogsOnFailure ::
  (MonadSTM m, MonadCatch m, Show msg, MonadSay m) =>
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure action = do
  tvar <- newTVarIO []
  action (traceInTVar tvar)
    `onException` (readTVarIO tvar >>= mapM_ (say . show) . reverse)
