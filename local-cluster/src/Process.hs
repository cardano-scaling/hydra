module Process (
    withProcess,
    TraceProcess (..),
) where

import Cardano.Prelude

import Logging (HasSeverityAnnotation (..), Severity (..), Tracer)
import System.Exit (ExitCode)
import System.Process (CreateProcess)

data ProcessTerminated
    = ProcessDidNotStart Text IOException
    | ProcessHasExited Text ExitCode
    deriving (Show, Eq)

withProcess ::
    MonadIO m =>
    Tracer m TraceProcess ->
    CreateProcess ->
    m a ->
    m (Either ProcessTerminated a)
withProcess tracer process action =
    undefined

data TraceProcess where
    MsgProcessHasStarted :: TraceProcess
    MsgProcessHasExited :: ExitCode -> TraceProcess

instance HasSeverityAnnotation TraceProcess where
    getSeverityAnnotation = \case
        MsgProcessHasStarted -> Info
        MsgProcessHasExited{} -> Info
