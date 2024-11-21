{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Cardano.Ledger.Core (PParams)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO, readTVar)
import Control.Exception (IOException)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (httpApp)
import Hydra.API.Projection (Projection (..), mkProjection)
import Hydra.API.ServerOutput (
  CommitInfo (CannotCommit),
  HeadStatus (Idle),
  ServerOutput,
  TimedServerOutput (..),
  projectCommitInfo,
  projectHeadStatus,
  projectInitializingHeadId,
  projectPendingDeposits,
  projectSnapshotUtxo,
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter,
 )
import Hydra.API.WSServer (wsApp)
import Hydra.Cardano.Api (LedgerEra)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Chain.Direct.State ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Persistence (PersistenceIncremental (..))
import Hydra.Tx (Party)
import Hydra.Tx.Environment (Environment)
import Network.HTTP.Types (status500)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setBeforeMainLoop,
  setHost,
  setOnException,
  setOnExceptionResponse,
  setPort,
 )
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.WebSockets (
  defaultConnectionOptions,
 )

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { sendOutput :: ServerOutput tx -> m ()
  -- ^ Send some output to all connected clients.
  }

-- | Callback for receiving client inputs.
type ServerCallback tx m = ClientInput tx -> m ()

-- | A type tying both receiving input and sending output into a /Component/.
type ServerComponent tx m a = ServerCallback tx m -> (Server tx m -> m a) -> m a

data APIServerConfig = APIServerConfig
  { host :: IP
  , port :: PortNumber
  , tlsCertPath :: Maybe FilePath
  , tlsKeyPath :: Maybe FilePath
  }

withAPIServer ::
  forall tx.
  IsChainState tx =>
  APIServerConfig ->
  Environment ->
  Party ->
  PersistenceIncremental (TimedServerOutput tx) IO ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  PParams LedgerEra ->
  ServerOutputFilter tx ->
  ServerComponent tx IO ()
withAPIServer config env party persistence tracer chain pparams serverOutputFilter callback action =
  handle onIOException $ do
    responseChannel <- newBroadcastTChanIO
    timedOutputEvents <- loadAll

    -- Intialize our read model from stored events
    headStatusP <- mkProjection Idle (output <$> timedOutputEvents) projectHeadStatus
    snapshotUtxoP <- mkProjection Nothing (output <$> timedOutputEvents) projectSnapshotUtxo
    commitInfoP <- mkProjection CannotCommit (output <$> timedOutputEvents) projectCommitInfo
    headIdP <- mkProjection Nothing (output <$> timedOutputEvents) projectInitializingHeadId
    pendingDepositsP <- mkProjection [] (output <$> timedOutputEvents) projectPendingDeposits

    nextSeqVar <- newTVarIO 0
    let nextSeq = do
          seq <- readTVar nextSeqVar
          modifyTVar' nextSeqVar (+ 1)
          pure seq

    (notifyServerRunning, waitForServerRunning) <- setupServerNotification

    let serverSettings =
          defaultSettings
            & setHost (fromString $ show host)
            & setPort (fromIntegral port)
            & setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
            & setOnExceptionResponse (responseLBS status500 [] . show)
            & setBeforeMainLoop notifyServerRunning
    race_
      ( do
          traceWith tracer (APIServerStarted port)
          startServer serverSettings
            . simpleCors
            $ websocketsOr
              defaultConnectionOptions
              (wsApp party tracer nextSeq callback headStatusP headIdP snapshotUtxoP responseChannel serverOutputFilter)
              (httpApp tracer chain env pparams (atomically $ getLatest commitInfoP) (atomically $ getLatest snapshotUtxoP) (atomically $ getLatest pendingDepositsP) callback)
      )
      ( do
          waitForServerRunning
          action $
            Server
              { sendOutput = \output -> do
                  timedOutput <- persistOutput nextSeq output
                  atomically $ do
                    update headStatusP output
                    update commitInfoP output
                    update snapshotUtxoP output
                    update headIdP output
                    update pendingDepositsP output
                    writeTChan responseChannel timedOutput
              }
      )
 where
  APIServerConfig{host, port, tlsCertPath, tlsKeyPath} = config

  PersistenceIncremental{loadAll, append} = persistence

  startServer settings app =
    case (tlsCertPath, tlsKeyPath) of
      (Just cert, Just key) ->
        runTLS (tlsSettings cert key) settings app
      -- TODO: better error handling
      (Just _, Nothing) ->
        die "TLS certificate provided without key"
      (Nothing, Just _) ->
        die "TLS key provided without certificate"
      _ ->
        runSettings settings app

  persistOutput nextSeq output = do
    time <- getCurrentTime
    timedOutput <- atomically $ do
      seq <- nextSeq
      pure TimedServerOutput{output, time, seq}
    append timedOutput
    pure timedOutput

  onIOException ioException =
    throwIO
      RunServerException
        { ioException
        , host
        , port
        }

-- | An 'IOException' with more 'IP' and 'PortNumber' added as context.
data RunServerException = RunServerException
  { ioException :: IOException
  , host :: IP
  , port :: PortNumber
  }
  deriving stock (Show)

instance Exception RunServerException

type NotifyServerRunning = IO ()

type WaitForServer = IO ()

-- | Setup notification and waiter to ensure that something only runs after the
-- server is actually listening.
setupServerNotification :: IO (NotifyServerRunning, WaitForServer)
setupServerNotification = do
  mv <- newEmptyMVar
  pure (putMVar mv (), takeMVar mv)
