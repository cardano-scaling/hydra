{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Server where

import Hydra.Prelude hiding (catMaybes, map, mapM_, seq, state)

import Cardano.Ledger.Core (PParams)
import Conduit (mapM_C, runConduitRes, (.|))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM.TChan (newBroadcastTChanIO, writeTChan)
import Control.Exception (IOException)
import Data.Conduit.Combinators (map)
import Data.Conduit.List (catMaybes)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (httpApp)
import Hydra.API.Projection (Projection (..), mkProjection)
import Hydra.API.ServerOutput (
  ClientMessage,
  CommitInfo (..),
  ServerOutput (..),
  TimedServerOutput (..),
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter,
 )
import Hydra.API.WSServer (wsApp)
import Hydra.Cardano.Api (LedgerEra)
import Hydra.Chain (Chain (..))
import Hydra.Chain.ChainState (IsChainState)
import Hydra.Chain.Direct.State ()
import Hydra.Events (EventSink (..), EventSource (..))
import Hydra.HeadLogic (aggregate)
import Hydra.HeadLogic.Outcome qualified as StateChanged
import Hydra.HeadLogic.State (
  Deposit (..),
  HeadState (Idle),
  IdleState (..),
 )
import Hydra.HeadLogic.StateEvent (StateEvent (..))
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (IP, PortNumber)
import Hydra.Node.Environment (Environment)
import Hydra.Tx (HeadId, IsTx (..), Party, txId)
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
  { sendMessage :: ClientMessage tx -> m ()
  -- ^ Send some output to all connected clients.
  }

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
  (ClientInput tx -> IO ()) ->
  ((EventSink (StateEvent tx) IO, Server tx IO) -> IO ()) ->
  IO ()
withAPIServer config env party eventSource tracer chain pparams serverOutputFilter callback action =
  handle onIOException $ do
    responseChannel <- newBroadcastTChanIO
    -- Initialize our read models from stored events
    -- NOTE: we do not keep the stored events around in memory
    headStateP <- mkProjection (Idle $ IdleState mkChainState) aggregate
    commitInfoP <- mkProjection CannotCommit projectCommitInfo
    headIdP <- mkProjection Nothing projectInitializingHeadId
    pendingDepositsP <- mkProjection [] projectPendingDeposits
    let historyTimedOutputs = sourceEvents .| map mkTimedServerOutputFromStateEvent .| catMaybes
    _ <-
      runConduitRes $
        sourceEvents
          .| mapM_C
            ( \StateEvent{stateChanged} ->
                lift $ atomically $ do
                  update headStateP stateChanged
                  update commitInfoP stateChanged
                  update headIdP stateChanged
                  update pendingDepositsP stateChanged
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
              (wsApp party tracer historyTimedOutputs callback headStateP headIdP responseChannel serverOutputFilter)
              ( httpApp
                  tracer
                  chain
                  env
                  pparams
                  (atomically $ getLatest headStateP)
                  (atomically $ getLatest commitInfoP)
                  (atomically $ getLatest pendingDepositsP)
                  callback
              )
      )
      ( do
          waitForServerRunning
          action
            ( EventSink
                { putEvent = \event@StateEvent{stateChanged} -> do
                    -- Update our read models
                    atomically $ do
                      update headStateP stateChanged
                      update commitInfoP stateChanged
                      update headIdP stateChanged
                      update pendingDepositsP stateChanged
                    -- Send to the client if it maps to a server output
                    case mkTimedServerOutputFromStateEvent event of
                      Nothing -> pure ()
                      Just timedOutput -> do
                        atomically $ writeTChan responseChannel (Left timedOutput)
                }
            , Server{sendMessage = atomically . writeTChan responseChannel . Right}
            )
      )
 where
  APIServerConfig{host, port, tlsCertPath, tlsKeyPath} = config

  EventSource{sourceEvents} = eventSource

  Chain{mkChainState} = chain

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

-- | Defines the subset of 'StateEvent' that should be sent as 'TimedServerOutput' to clients.
mkTimedServerOutputFromStateEvent :: IsTx tx => StateEvent tx -> Maybe (TimedServerOutput tx)
mkTimedServerOutputFromStateEvent event =
  case mapStateChangedToServerOutput stateChanged of
    Nothing -> Nothing
    Just output ->
      Just $ TimedServerOutput{output, time, seq = fromIntegral eventId}
 where
  StateEvent{eventId, time, stateChanged} = event

  mapStateChangedToServerOutput = \case
    StateChanged.HeadInitialized{headId, parties} -> Just HeadIsInitializing{headId, parties}
    StateChanged.CommittedUTxO{..} -> Just $ Committed{headId, party, utxo = committedUTxO}
    StateChanged.HeadOpened{headId, initialUTxO} -> Just HeadIsOpen{headId, utxo = initialUTxO}
    StateChanged.HeadClosed{..} -> Just HeadIsClosed{..}
    StateChanged.HeadContested{..} -> Just HeadIsContested{..}
    StateChanged.HeadIsReadyToFanout{..} -> Just ReadyToFanout{..}
    StateChanged.HeadAborted{headId, utxo} -> Just HeadIsAborted{headId, utxo}
    StateChanged.HeadFannedOut{..} -> Just HeadIsFinalized{..}
    StateChanged.TransactionAppliedToLocalUTxO{..} -> Just TxValid{headId, transactionId = txId tx}
    StateChanged.TxInvalid{..} -> Just $ TxInvalid{..}
    StateChanged.SnapshotConfirmed{..} -> Just SnapshotConfirmed{..}
    StateChanged.IgnoredHeadInitializing{..} -> Just IgnoredHeadInitializing{..}
    StateChanged.DecommitRecorded{..} -> Just DecommitRequested{..}
    StateChanged.DecommitInvalid{..} -> Just DecommitInvalid{..}
    StateChanged.DecommitApproved{..} -> Just DecommitApproved{..}
    StateChanged.DecommitFinalized{..} -> Just DecommitFinalized{..}
    StateChanged.DepositRecorded{..} -> Just CommitRecorded{headId, utxoToCommit = deposited, pendingDeposit = depositTxId, deadline}
    StateChanged.DepositActivated{depositTxId, chainTime, deposit = Deposit{..}} -> Just DepositActivated{..}
    StateChanged.DepositExpired{depositTxId, chainTime, deposit = Deposit{..}} -> Just DepositExpired{..}
    StateChanged.DepositRecovered{..} -> Just CommitRecovered{headId, recoveredTxId = depositTxId, recoveredUTxO = recovered}
    StateChanged.CommitApproved{..} -> Just CommitApproved{..}
    StateChanged.CommitFinalized{..} -> Just CommitFinalized{..}
    StateChanged.NetworkConnected -> Just NetworkConnected
    StateChanged.NetworkDisconnected -> Just NetworkDisconnected
    StateChanged.NetworkVersionMismatch{..} -> Just NetworkVersionMismatch{..}
    StateChanged.NetworkClusterIDMismatch{..} -> Just NetworkClusterIDMismatch{..}
    StateChanged.PeerConnected{..} -> Just PeerConnected{..}
    StateChanged.PeerDisconnected{..} -> Just PeerDisconnected{..}
    StateChanged.TransactionReceived{} -> Nothing
    StateChanged.SnapshotRequested{} -> Nothing
    StateChanged.SnapshotRequestDecided{} -> Nothing
    StateChanged.PartySignedSnapshot{} -> Nothing
    StateChanged.ChainRolledBack{} -> Nothing
    StateChanged.TickObserved{} -> Nothing
    StateChanged.LocalStateCleared{..} -> Just SnapshotSideLoaded{..}
    StateChanged.Checkpoint{} -> Just EventLogRotated

-- | Projection to obtain the list of pending deposits.
projectPendingDeposits :: IsTx tx => [TxIdType tx] -> StateChanged.StateChanged tx -> [TxIdType tx]
projectPendingDeposits txIds = \case
  StateChanged.DepositRecorded{depositTxId} -> depositTxId : txIds
  StateChanged.DepositRecovered{depositTxId} -> filter (/= depositTxId) txIds
  StateChanged.CommitFinalized{depositTxId} -> filter (/= depositTxId) txIds
  _other -> txIds

-- | Projection to obtain 'CommitInfo' needed to draft commit transactions.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectCommitInfo :: CommitInfo -> StateChanged.StateChanged tx -> CommitInfo
projectCommitInfo commitInfo = \case
  StateChanged.HeadInitialized{headId} -> NormalCommit headId
  StateChanged.HeadOpened{headId} -> IncrementalCommit headId
  StateChanged.HeadAborted{} -> CannotCommit
  StateChanged.HeadClosed{} -> CannotCommit
  _other -> commitInfo

-- | Projection to obtain the 'HeadId' needed to draft a commit transaction.
-- NOTE: We only want to project 'HeadId' when the Head is in the 'Initializing'
-- state since this is when Head parties need to commit some funds.
projectInitializingHeadId :: Maybe HeadId -> StateChanged.StateChanged tx -> Maybe HeadId
projectInitializingHeadId mHeadId = \case
  StateChanged.HeadInitialized{headId} -> Just headId
  StateChanged.HeadOpened{} -> Nothing
  StateChanged.HeadAborted{} -> Nothing
  _other -> mHeadId
