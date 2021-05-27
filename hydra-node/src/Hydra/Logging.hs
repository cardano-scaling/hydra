{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Adapter module to the actual logging framework. For now we are using the
-- iohk-monitoring package, but that might change soon.
module Hydra.Logging (
  -- * Tracer
  Tracer,
  natTracer,
  nullTracer,
  contramap,
  traceWith,
  traceInTVarIO,

  -- * Using it
  Verbosity (..),
  LoggerName,
  withTracer,
) where

import Cardano.BM.Backend.Switchboard (
  Switchboard,
  setSbEKGServer,
 )
import qualified Cardano.BM.Configuration.Model as CM
import Cardano.BM.Configuration.Static (
  defaultConfigStdout,
 )
import qualified Cardano.BM.Data.BackendKind as CM
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
import Cardano.BM.Setup (
  setupTrace_,
  shutdown,
 )
import Cardano.BM.Trace (traceInTVarIO)
import Cardano.Prelude
import Control.Tracer (
  Tracer (..),
  contramap,
  natTracer,
  nullTracer,
  traceWith,
 )
import qualified System.Remote.Monitoring as Ekg

data Verbosity = Quiet | Verbose LoggerName
  deriving (Eq, Show)

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withTracer ::
  forall m msg a.
  MonadIO m =>
  Verbosity ->
  (msg -> Text) ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet _ between = between nullTracer
withTracer (Verbose name) transform between = do
  bracket before after (between . natTracer liftIO . fst)
 where
  before :: IO (Tracer IO msg, Switchboard Text)
  before = do
    config <- defaultConfigStdout
    CM.setSetupBackends config [CM.KatipBK, CM.EKGViewBK]
    ekgServer <- Ekg.forkServer "127.0.0.1" 6001
    (tr, sb) <- setupTrace_ config name
    setSbEKGServer (Just ekgServer) sb
    pure (transformLogObject transform tr, sb)

  after :: (Tracer IO msg, Switchboard Text) -> IO ()
  after = shutdown . snd

-- | Tracer transformer which converts 'Trace m a' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'. NOTE: All log messages are of severity
-- 'Debug'.
transformLogObject ::
  MonadIO m =>
  (msg -> Text) ->
  Tracer m (LoggerName, LogObject Text) ->
  Tracer m msg
transformLogObject transform tr = Tracer $ \a -> do
  traceWith tr . (mempty,) =<< LogObject mempty
    <$> mkLOMeta Debug Public
    <*> pure (LogMessage (transform a))
