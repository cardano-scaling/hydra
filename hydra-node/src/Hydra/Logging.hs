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
  getSbEKGServer,
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
import Hydra.Network (PortNumber)
import qualified System.Remote.Monitoring as Ekg

data Verbosity = Quiet | Verbose LoggerName
  deriving (Eq, Show)

-- | Acquire a tracer that automatically shutdown once the action is done via
-- bracket-style allocation.
withTracer ::
  forall m msg a.
  MonadIO m =>
  Maybe PortNumber ->
  Verbosity ->
  (msg -> Text) ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer _ Quiet _ between = between nullTracer
withTracer mEkgPort (Verbose name) transform between = do
  bracket before after (between . natTracer liftIO . fst)
 where
  before :: IO (Tracer IO msg, Switchboard Text)
  before = do
    config <- defaultConfigStdout
    CM.setSetupBackends config [CM.KatipBK, CM.EKGViewBK]
    (tr, sb) <- setupTrace_ config name
    beforeEkg mEkgPort sb
    pure (transformLogObject transform tr, sb)

  beforeEkg Nothing _ = pure ()
  beforeEkg (Just ekgPort) sb = do
    ekgServer <- Ekg.forkServer "127.0.0.1" (fromIntegral ekgPort)
    setSbEKGServer (Just ekgServer) sb

  after :: (Tracer IO msg, Switchboard Text) -> IO ()
  after (_, sb) = getSbEKGServer sb >>= afterEkg >> shutdown sb

  afterEkg Nothing = pure ()
  afterEkg (Just ekg) = killThread $ Ekg.serverThreadId ekg

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
