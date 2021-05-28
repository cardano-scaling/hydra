{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
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
  Log (..),
  withTracer,
  traceEvent,
  traceCounter,
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
import qualified System.Metrics.Counter as Metrics
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
  (Tracer m (Log msg) -> IO a) ->
  IO a
withTracer _ Quiet _ between = between nullTracer
withTracer mEkgPort (Verbose name) transform between = do
  bracket before after (between . natTracer liftIO . fst)
 where
  before :: IO (Tracer IO (Log msg), Switchboard Text)
  before = do
    config <- defaultConfigStdout
    CM.setSetupBackends config [CM.KatipBK, CM.EKGViewBK]
    (tr, sb) <- setupTrace_ config name
    beforeEkg mEkgPort sb
    pure (makeTypedTracer sb transform tr, sb)

  beforeEkg Nothing _ = pure ()
  beforeEkg (Just ekgPort) sb = do
    ekgServer <- Ekg.forkServer "127.0.0.1" (fromIntegral ekgPort)
    setSbEKGServer (Just ekgServer) sb

  after :: (Tracer IO (Log msg), Switchboard Text) -> IO ()
  after (_, sb) = getSbEKGServer sb >>= afterEkg >> shutdown sb

  afterEkg Nothing = pure ()
  afterEkg (Just ekg) = killThread $ Ekg.serverThreadId ekg

-- | Tag an arbitrary trace with a counter name
data Log a = Log a | Counter Text a
  deriving (Eq, Show, Functor)

traceEvent :: Tracer m (Log a) -> a -> m ()
traceEvent tracer = traceWith (contramap Log tracer)

traceCounter :: Tracer m (Log a) -> Text -> a -> m ()
traceCounter tracer lbl a = traceWith tracer (Counter lbl a)

-- | Tracer transformer which converts 'Trace m (LoggerName, LogObject Text)' to 'Tracer m a' by wrapping
-- typed log messages into a 'LogObject'. NOTE: All log messages are of severity
-- 'Debug'.
makeTypedTracer ::
  MonadIO m =>
  Switchboard t ->
  (msg -> Text) ->
  Tracer m (LoggerName, LogObject Text) ->
  Tracer m (Log msg)
makeTypedTracer sb asText tr = Tracer $ \case
  Log a -> doTrace a
  Counter lbl a -> do
    doTrace a
    liftIO $
      getSbEKGServer sb >>= \case
        Nothing -> pure ()
        Just server -> Ekg.getCounter lbl server >>= Metrics.inc
 where
  doTrace a = do
    traceWith tr . (mempty,) =<< LogObject mempty
      <$> mkLOMeta Debug Public
      <*> pure (LogMessage (asText a))
