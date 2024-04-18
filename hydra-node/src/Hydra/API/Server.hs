{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Cardano.Ledger.Core (PParams)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Concurrent.STM.TVar (modifyTVar', newTVarIO)
import Control.Exception (IOException)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (httpApp)
import Hydra.API.Projection (Projection (..), mkProjection)
import Hydra.API.ServerOutput (
  HeadStatus (Idle),
  ServerOutput,
  TimedServerOutput (..),
  projectHeadStatus,
  projectInitializingHeadId,
  projectSnapshotUtxo,
 )
import Hydra.API.WSServer (nextSequenceNumber, wsApp)
import Hydra.Cardano.Api (LedgerEra)
import Hydra.Chain (
  Chain (..),
  IsChainState,
 )
import Hydra.Chain.Direct.State ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Party (Party)
import Hydra.Persistence (PersistenceIncremental (..))
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setBeforeMainLoop,
  setHost,
  setOnException,
  setPort,
 )
import Network.Wai.Handler.WebSockets (websocketsOr)
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

withAPIServer ::
  forall tx.
  IsChainState tx =>
  IP ->
  PortNumber ->
  Party ->
  PersistenceIncremental (TimedServerOutput tx) IO ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  PParams LedgerEra ->
  ServerComponent tx IO ()
withAPIServer host port party PersistenceIncremental{loadAll, append} tracer chain pparams callback action =
  handle onIOException $ do
    responseChannel <- newBroadcastTChanIO
    timedOutputEvents <- loadAll

    -- Intialize our read model from stored events
    headStatusP <- mkProjection Idle (output <$> timedOutputEvents) projectHeadStatus
    snapshotUtxoP <- mkProjection Nothing (output <$> timedOutputEvents) projectSnapshotUtxo
    headIdP <- mkProjection Nothing (output <$> timedOutputEvents) projectInitializingHeadId

    -- NOTE: we need to reverse the list because we store history in a reversed
    -- list in memory but in order on disk
    history <- newTVarIO (reverse timedOutputEvents)
    (notifyServerRunning, waitForServerRunning) <- setupServerNotification

    let serverSettings =
          defaultSettings
            & setHost (fromString $ show host)
            & setPort (fromIntegral port)
            & setOnException (\_ e -> traceWith tracer $ APIConnectionError{reason = show e})
            & setBeforeMainLoop notifyServerRunning
    race_
      ( do
          traceWith tracer (APIServerStarted port)
          runSettings serverSettings $
            websocketsOr
              defaultConnectionOptions
              (wsApp party tracer history callback headStatusP snapshotUtxoP responseChannel)
              (httpApp tracer chain pparams (atomically $ getLatest headIdP) (atomically $ getLatest snapshotUtxoP))
      )
      ( do
          waitForServerRunning
          action $
            Server
              { sendOutput = \output -> do
                  timedOutput <- appendToHistory history output
                  atomically $ do
                    update headStatusP output
                    update snapshotUtxoP output
                    update headIdP output
                    writeTChan responseChannel timedOutput
              }
      )
 where
  appendToHistory history output = do
    time <- getCurrentTime
    timedOutput <- atomically $ do
      seq <- nextSequenceNumber history
      let timedOutput = TimedServerOutput{output, time, seq}
      modifyTVar' history (timedOutput :)
      pure timedOutput
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
