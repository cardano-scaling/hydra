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
  traceInTVar,
  LoggerName,

  -- * Using it
  Verbosity (..),
  withTracer,
) where

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
import Cardano.BM.Setup (
  setupTrace_,
  shutdown,
 )
import Cardano.Prelude hiding (atomically)
import Control.Tracer (
  Tracer (..),
  contramap,
  natTracer,
  nullTracer,
  traceWith,
 )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import Control.Monad.Class.MonadSTM (MonadSTM (atomically), TVar, modifyTVar)

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
    CM.setSetupBackends config [CM.KatipBK]
    (tr, sb) <- setupTrace_ config name
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

traceInTVar :: MonadSTM m => TVar m [a] -> Tracer m a
traceInTVar tvar = Tracer $ \a ->
  atomically $ modifyTVar tvar (a :)
