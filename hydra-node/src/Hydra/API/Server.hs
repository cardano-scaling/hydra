{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (mapM_, seq, state)

import Cardano.Ledger.Core (PParams)
import Conduit (mapWhileC, (.|))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Exception (IOException)
import Data.Conduit.Combinators (iterM)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (httpApp)
import Hydra.API.Projection (Projection (..), mkProjection)
import Hydra.API.ServerOutput (
  CommitInfo (CannotCommit),
  HeadStatus (Idle),
  ServerOutput (..),
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
import Hydra.Events (EventSource (..), StateEvent (..))
import Hydra.HeadLogic.Outcome qualified as StateChanged
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Tx (IsTx, Party, txId)
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
import Control.Concurrent.Class.MonadSTM (newTVarIO)

-- | Handle to provide a means for sending server outputs to clients.
newtype Server tx m = Server
  { sendOutput :: StateEvent tx -> m ()
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
  EventSource (StateEvent tx) IO ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  PParams LedgerEra ->
  ServerOutputFilter tx ->
  ServerComponent tx IO ()
withAPIServer config env party eventSource tracer chain pparams serverOutputFilter callback action =
  handle onIOException $ do
    responseChannel <- newBroadcastTChanIO
    -- Intialize our read models from stored events
    -- NOTE: we do not keep the stored events around in memory
    headStatusP <- mkProjection Idle projectHeadStatus
    snapshotUtxoP <- mkProjection Nothing projectSnapshotUtxo
    commitInfoP <- mkProjection CannotCommit projectCommitInfo
    headIdP <- mkProjection Nothing projectInitializingHeadId
    pendingDepositsP <- mkProjection [] projectPendingDeposits
    let history =
          sourceEvents
            .| mapWhileC mkTimedServerOutputFromStateEvent
            .| iterM
              ( \TimedServerOutput{output} ->
                  lift $ atomically $ do
                    update headStatusP output
                    update snapshotUtxoP output
                    update commitInfoP output
                    update headIdP output
                    update pendingDepositsP output
            )
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
              (wsApp party tracer history callback headStatusP headIdP snapshotUtxoP responseChannel serverOutputFilter)
              (httpApp tracer chain env pparams (atomically $ getLatest commitInfoP) (atomically $ getLatest snapshotUtxoP) (atomically $ getLatest pendingDepositsP) callback)
      )
      ( do
          waitForServerRunning
          action $
            Server
              { sendOutput = \StateEvent{stateChanged, time, eventId} -> do
                  case mapStateChangedToServerOutput stateChanged of
                    Nothing -> pure ()
                    Just output -> do
                      let timedOutput = TimedServerOutput{output, time, seq = fromIntegral eventId}
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

  EventSource{sourceEvents} = eventSource

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

mkTimedServerOutputFromStateEvent :: IsTx tx => StateEvent tx -> Maybe (TimedServerOutput tx)
mkTimedServerOutputFromStateEvent event =
  case mapStateChangedToServerOutput stateChanged of
    Nothing -> Nothing
    Just output ->
      Just $ TimedServerOutput{output, time, seq = fromIntegral eventId}
 where
  StateEvent{eventId, time, stateChanged} = event

mapStateChangedToServerOutput :: IsTx tx => StateChanged.StateChanged tx -> Maybe (ServerOutput tx)
mapStateChangedToServerOutput = \case
  StateChanged.PeerConnected{..} -> Just PeerConnected{..}
  StateChanged.PeerDisconnected{..} -> Just PeerDisconnected{..}
  StateChanged.PeerHandshakeFailure{..} -> Just PeerHandshakeFailure{..}
  StateChanged.HeadInitialized{headId, parties} -> Just HeadIsInitializing{headId, parties}
  StateChanged.CommittedUTxO{..} -> Just $ Committed{headId, party, utxo = committedUTxO}
  StateChanged.HeadOpened{headId, initialUTxO} -> Just HeadIsOpen{headId, utxo = initialUTxO}
  StateChanged.HeadClosed{..} -> Just HeadIsClosed{..}
  StateChanged.HeadContested{..} -> Just HeadIsContested{..}
  StateChanged.HeadIsReadyToFanout{..} -> Just ReadyToFanout{..}
  StateChanged.HeadAborted{headId, utxo} -> Just HeadIsAborted{headId, utxo}
  StateChanged.HeadFannedOut{..} -> Just HeadIsFinalized{..}
  StateChanged.CommandFailed{..} -> Just CommandFailed{..}
  StateChanged.TransactionAppliedToLocalUTxO{..} -> Just TxValid{headId, transactionId = txId tx, transaction = tx}
  StateChanged.TxInvalid{..} -> Just $ TxInvalid{..}
  StateChanged.SnapshotConfirmed{..} -> Just SnapshotConfirmed{..}
  StateChanged.PostTxOnChainFailed{..} -> Just PostTxOnChainFailed{..}
  StateChanged.IgnoredHeadInitializing{..} -> Just IgnoredHeadInitializing{..}
  StateChanged.DecommitRequested{..} -> Just DecommitRequested{..}
  StateChanged.DecommitInvalid{..} -> Just DecommitInvalid{..}
  StateChanged.DecommitApproved{..} -> Just DecommitApproved{..}
  StateChanged.DecommitFinalized{..} -> Just DecommitFinalized{..}
  StateChanged.CommitRecorded{..} -> Just CommitRecorded{..}
  StateChanged.CommitApproved{..} -> Just CommitApproved{..}
  StateChanged.CommitFinalized{..} -> Just CommitFinalized{..}
  StateChanged.CommitRecovered{..} -> Just CommitRecovered{..}
  StateChanged.CommitIgnored{..} -> Just CommitIgnored{..}
  StateChanged.TransactionReceived{} -> Nothing
  StateChanged.DecommitRecorded{} -> Nothing
  StateChanged.SnapshotRequested{} -> Nothing
  StateChanged.SnapshotRequestDecided{} -> Nothing
  StateChanged.PartySignedSnapshot{} -> Nothing
  StateChanged.ChainRolledBack{} -> Nothing
  StateChanged.TickObserved{} -> Nothing
