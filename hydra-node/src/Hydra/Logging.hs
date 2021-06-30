{-# LANGUAGE RankNTypes #-}
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
  LoggerName,

  -- * Using it
  ToObject (..),
  Verbosity (..),
  TracingVerbosity (..),
  withTracer,
  contramap,
  showLogsOnFailure,
) where

import Hydra.Prelude

import Cardano.BM.Backend.Switchboard (
  Switchboard,
 )
import Cardano.BM.Configuration.Static (
  defaultConfigStdout,
 )
import Cardano.BM.Data.LogItem (
  LOContent (..),
  LogObject (..),
  LoggerName,
  PrivacyAnnotation (..),
  mkLOMeta,
 )
import Cardano.BM.Data.Severity (
  Severity (..),
 )
import Cardano.BM.Data.Tracer (
  ToObject (..),
  TracingVerbosity (..),
 )
import Cardano.BM.Setup (
  setupTrace_,
  shutdown,
 )
import Control.Tracer (
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,
 )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import Control.Monad.Class.MonadSTM (modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadSay (MonadSay, say)

data Verbosity = Quiet | Verbose LoggerName
  deriving (Eq, Show)

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withTracer ::
  forall m msg a.
  (MonadIO m, ToObject msg) =>
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet between = between nullTracer
withTracer (Verbose name) between = do
  bracket before after (between . natTracer liftIO . fst)
 where
  before :: IO (Tracer IO msg, Switchboard ())
  before = do
    config <- defaultConfigStdout
    CM.setSetupBackends config [CM.KatipBK]
    CM.setMinSeverity config Debug
    CM.setDefaultScribes config ["StdoutSK::json"]
    -- TODO: Pass those down from parent context, to appear in the logs:
    --
    -- CM.setTextOption config "appversion" version
    -- CM.setTextOption config "appcommit" commit
    (tr, sb) <- setupTrace_ config name
    pure (transformLogObject tr, sb)

  after :: (tracer, Switchboard ()) -> IO ()
  after = shutdown . snd

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'. NOTE: All log messages are of severity
-- 'Debug'.
transformLogObject ::
  (MonadIO m, ToObject msg) =>
  Tracer m (LoggerName, LogObject ()) ->
  Tracer m msg
transformLogObject tr = Tracer $ \msg -> do
  traceWith tr . (mempty,) =<< LogObject mempty
    <$> mkLOMeta Debug Public
    <*> pure (LogStructured (toObject MaximalVerbosity msg))

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
