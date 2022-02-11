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
  traceInTVar,
  ToObject (..),
  TracingVerbosity (..),

  -- * Using it
  Verbosity (..),
  Envelope (..),
  withTracer,
  contramap,
  showLogsOnFailure,
) where

import Hydra.Prelude

import Cardano.BM.Tracing (ToObject (..), TracingVerbosity (..))
import Control.Monad.Class.MonadFork (myThreadId)
import Control.Monad.Class.MonadSTM (MonadSTMTx (readTBQueue), flushTBQueue, modifyTVar, newTBQueueIO, newTVarIO, readTVarIO, writeTBQueue)
import Control.Monad.Class.MonadSay (MonadSay, say)
import Control.Tracer (
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
 )
import Data.Aeson (encode)
import qualified Data.Text as Text

data Verbosity = Quiet | Verbose Text
  deriving (Eq, Show)

-- | Provides logging metadata for entries.
-- NOTE(AB): setting the `message` to be `HydraLog` would require to pass along
-- @tx@ and @net@ type arguments which is useless in the context, hence the more
-- generic type.
data Envelope a = Envelope
  { namespace :: Text
  , timestamp :: UTCTime
  , threadId :: Int
  , message :: a
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (Envelope a) where
  arbitrary = genericArbitrary

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
withTracer (Verbose namespace) action = do
  msgQueue <- newTBQueueIO @_ @(Envelope msg) defaultQueueSize
  withAsync (writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
 where
  tracer queue =
    Tracer
      ( \msg ->
          liftIO (mkEnvelop msg >>= atomically . writeTBQueue queue)
      )

  mkEnvelop :: msg -> IO (Envelope msg)
  mkEnvelop message = do
    timestamp <- liftIO getCurrentTime
    threadId <- mkThreadId <$> liftIO myThreadId
    pure $ Envelope{namespace, timestamp, threadId, message}

  -- NOTE(AB): This is a bit contrived but we want a numeric threadId and we
  -- get some text which we know the structure of
  mkThreadId = fromMaybe 0 . readMaybe . Text.unpack . Text.drop 9 . show

  writeLogs queue =
    forever $ do
      atomically (readTBQueue queue) >>= putLBSLn . encode
      hFlush stdout

  flushLogs queue = do
    entries <- atomically $ flushTBQueue queue
    forM_ entries (putLBSLn . encode)
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
