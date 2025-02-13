{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (TVar, mapM_, readTVar, seq)

import Cardano.Ledger.Core (PParams)
import Conduit (runConduitRes, sinkList, (.|))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import Control.Exception (IOException)
import Data.Conduit.Combinators (iterM)
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
import Hydra.API.WSServer (nextSequenceNumber, wsApp)
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
  Tracer IO APIServerLog ->
  Chain tx IO ->
  PParams LedgerEra ->
  ServerOutputFilter tx ->
  ServerComponent tx IO ()
withAPIServer config env party persistence tracer chain pparams serverOutputFilter callback action = do
  responseChannel <- newBroadcastTChanIO
  -- Intialize our read models from stored events
  -- NOTE: we do not keep the stored events around in memory
  headStatusP <- mkProjection Idle projectHeadStatus
  snapshotUtxoP <- mkProjection Nothing projectSnapshotUtxo
  commitInfoP <- mkProjection CannotCommit projectCommitInfo
  headIdP <- mkProjection Nothing projectInitializingHeadId
  pendingDepositsP <- mkProjection [] projectPendingDeposits
  loadedHistory <-
    runConduitRes $
      source
        -- .| mapC output
        .| iterM (lift . atomically . update headStatusP . output)
        .| iterM (lift . atomically . update snapshotUtxoP . output)
        .| iterM (lift . atomically . update commitInfoP . output)
        .| iterM (lift . atomically . update headIdP . output)
        .| iterM (lift . atomically . update pendingDepositsP . output)
        -- FIXME: don't load whole history into memory
        .| sinkList

  -- NOTE: we need to reverse the list because we store history in a reversed
  -- list in memory but in order on disk
  history <- newTVarIO $ reverse loadedHistory
  (notifyServerRunning, waitForServerRunning) <- setupServerNotification

  let serverSettings =
        defaultSettings
          & setHost (fromString $ show host)
          & setPort (fromIntegral port)
          & setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
          & setOnExceptionResponse (responseLBS status500 [] . show)
          & setBeforeMainLoop notifyServerRunning
  race_
    ( handle onIOException $ do
        traceWith tracer (APIServerStarted port)
        startServer serverSettings
          . simpleCors
          $ websocketsOr
            defaultConnectionOptions
            (wsApp party tracer history callback headStatusP headIdP snapshotUtxoP responseChannel serverOutputFilter)
            (httpApp tracer chain env pparams (atomically $ getLatest commitInfoP) (atomically $ getLatest snapshotUtxoP) (atomically $ getLatest pendingDepositsP) callback)
    )
    ( do
        waitForServerRunning
        action $
          Server
            { sendOutput = \output -> do
                timedOutput <- appendToHistory history output
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

  appendToHistory history output = do
    time <- getCurrentTime
    atomically $ do
      seq <- nextSequenceNumber history
      let timedOutput = TimedServerOutput{output, time, seq}
      modifyTVar' history (timedOutput :)
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
