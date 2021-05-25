{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework. For now we are using the
-- iohk-monitoring package, but that might change soon.
module Logging (
  -- * Tracer
  Tracer,
  natTracer,
  nullTracer,
  contramap,
  traceWith,

  -- * Severity
  Severity (..),
  HasSeverityAnnotation (..),

  -- * Instantiation
  LoggerName,
  withStdoutTracer,
) where

import Cardano.Prelude

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
  HasSeverityAnnotation (..),
 )
import Cardano.BM.Setup (
  setupTrace_,
  shutdown,
 )
import Control.Tracer (
  Tracer (..),
  contramap,
  natTracer,
  nullTracer,
  traceWith,
 )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withStdoutTracer ::
  forall m msg a.
  (MonadIO m, HasSeverityAnnotation msg) =>
  LoggerName ->
  Severity ->
  (msg -> Text) ->
  (Tracer m msg -> IO a) ->
  IO a
withStdoutTracer name minSeverity transform between = do
  bracket before after (between . natTracer liftIO . fst)
 where
  before :: IO (Tracer IO msg, Switchboard Text)
  before = do
    config <- defaultConfigStdout
    CM.setMinSeverity config minSeverity
    CM.setSetupBackends config [CM.KatipBK]
    (tr, sb) <- setupTrace_ config name
    pure (transformLogObject transform tr, sb)

  after :: (Tracer IO msg, Switchboard Text) -> IO ()
  after = shutdown . snd

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'.
transformLogObject ::
  (MonadIO m, HasSeverityAnnotation msg) =>
  (msg -> Text) ->
  Tracer m (LoggerName, LogObject Text) ->
  Tracer m msg
transformLogObject transform tr = Tracer $ \a -> do
  traceWith tr . (mempty,) =<< LogObject mempty
    <$> mkLOMeta (getSeverityAnnotation a) Public
    <*> pure (LogMessage (transform a))
