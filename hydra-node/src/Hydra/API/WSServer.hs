{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, filter, readTVar, seq)

import Conduit (ConduitT, ResourceT, mapM_C, runConduitRes, (.|))
import Control.Concurrent.Class.MonadSTM.TChan (writeTChan)
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Control.Lens ((.~))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey)
import Data.Conduit.Combinators (filter)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput (SafeClose))
import Hydra.API.Projection (Projection (..))
import Hydra.API.ServerOutput (
  ClientMessage,
  Greetings (..),
  HeadStatus (..),
  InvalidInput (..),
  NetworkInfo,
  ServerOutput (ChainOutOfSync),
  ServerOutputConfig (..),
  TimedServerOutput (..),
  WithAddressedTx (..),
  WithUTxO (..),
  getSnapshotUtxo,
  handleUtxoInclusion,
  headStatus,
  me,
  prepareServerOutput,
  removeSnapshotUTxO,
  snapshotUtxo,
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter (..),
 )
import Hydra.Chain (Chain (..))
import Hydra.Chain.ChainState (
  IsChainState,
 )
import Hydra.Chain.Direct.State (chainSlotFromPoint)
import Hydra.Chain.SyncedStatus (SyncedStatus (..))
import Hydra.HeadLogic (ClosedState (ClosedState, readyToFanoutSent), HeadState, InitialState (..), OpenState (..), StateChanged)
import Hydra.HeadLogic.State qualified as HeadState
import Hydra.Logging (Tracer, traceWith)
import Hydra.NetworkVersions qualified as NetworkVersions
import Hydra.Node.Environment (Environment (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Tx (HeadId, Party)
import Network.WebSockets (
  PendingConnection (pendingRequest),
  RequestHead (..),
  acceptRequest,
  receiveData,
  sendTextData,
  withPingThread,
 )
import Text.URI hiding (ParseException)
import Text.URI.QQ (queryKey, queryValue)

wsApp ::
  forall tx.
  IsChainState tx =>
  Environment ->
  Party ->
  Tracer IO APIServerLog ->
  Chain tx IO ->
  ConduitT () (TimedServerOutput tx) (ResourceT IO) () ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (StateChanged tx) (NodeState tx) ->
  -- | Read model to enhance 'Greetings' messages with 'NetworkInfo'.
  Projection STM.STM (StateChanged tx) NetworkInfo ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ServerOutputFilter tx ->
  IO SyncedStatus ->
  PendingConnection ->
  IO ()
wsApp env party tracer chain history callback nodeStateP networkInfoP responseChannel ServerOutputFilter{txContainsAddr} chainSyncedStatus pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
  _ <- forkLabelled "ws-check-sync-status" $ forever $ do
    NodeState{currentSlot} <- atomically getLatestNodeState
    synced@SyncedStatus{status, point} <- chainSyncedStatus
    if status && currentSlot <= chainSlotFromPoint point
      then pure ()
      else do
        tso <- timed 0 (ChainOutOfSync synced currentSlot)
        atomically $ writeTChan responseChannel (Left tso)
    -- check every second
    -- TODO! configure threadDelay
    threadDelay 1
  chan <- STM.atomically $ dupTChan responseChannel

  let outConfig = mkServerOutputConfig queryParams

  -- api client can decide if they want to see the past history of server outputs
  when (shouldServeHistory queryParams) $
    forwardHistory con outConfig

  forwardGreetingOnly outConfig con

  withPingThread con 30 (pure ()) $
    raceLabelled_
      ("ws-con-receive-inputs", receiveInputs con)
      ("ws-con-send-outputs", sendOutputs chan con outConfig)
 where
  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  forwardGreetingOnly config con = do
    NodeState{headState} <- atomically getLatestNodeState
    networkInfo <- atomically getLatestNetworkInfo
    sendTextData con $
      handleUtxoInclusion config (atKey "snapshotUtxo" .~ Nothing) $
        Aeson.encode
          Greetings
            { me = party
            , headStatus = getHeadStatus headState
            , hydraHeadId = getHeadId headState
            , snapshotUtxo = getSnapshotUtxo headState
            , hydraNodeVersion = showVersion NetworkVersions.hydraNodeVersion
            , env
            , networkInfo
            }

  Projection{getLatest = getLatestNodeState} = nodeStateP
  Projection{getLatest = getLatestNetworkInfo} = networkInfoP

  mkServerOutputConfig :: [QueryParam] -> ServerOutputConfig
  mkServerOutputConfig qp =
    ServerOutputConfig
      { utxoInSnapshot = decideOnUTxODisplay qp
      , addressInTx = decideOnAddressDisplay qp
      }

  decideOnUTxODisplay :: [QueryParam] -> WithUTxO
  decideOnUTxODisplay qp =
    let k :: RText t
        k = [queryKey|snapshot-utxo|]
        v :: RText t
        v = [queryValue|no|]
        queryP = QueryParam k v
     in if queryP `elem` qp then WithoutUTxO else WithUTxO

  decideOnAddressDisplay :: [QueryParam] -> WithAddressedTx
  decideOnAddressDisplay qp =
    case find queryByAddress qp of
      Just (QueryParam _ v) -> WithAddressedTx (unRText v)
      _ -> WithoutAddressedTx
   where
    queryByAddress = \case
      (QueryParam key _) | key == [queryKey|address|] -> True
      _other -> False

  shouldServeHistory :: [QueryParam] -> Bool
  shouldServeHistory qp =
    flip any qp $ \case
      (QueryParam key val)
        | key == [queryKey|history|] -> val == [queryValue|yes|]
      _other -> False

  sendOutputs chan con outConfig@ServerOutputConfig{addressInTx} = forever $ do
    response <- STM.atomically $ readTChan chan
    when (isAddressInTx addressInTx response) $
      sendResponse response
   where
    sendResponse = \case
      Left response -> do
        let sentResponse = prepareServerOutput outConfig response
        sendTextData con sentResponse
        traceWith tracer (APIOutputSent $ toJSON response)
      Right response -> do
        sendTextData con (handleUtxoInclusion outConfig removeSnapshotUTxO $ Aeson.encode response)
        traceWith tracer (APIOutputSent $ toJSON response)

  Chain{checkNonADAAssets} = chain
  
  timed :: Natural -> ServerOutput tx -> IO (TimedServerOutput tx)
  timed seq out = TimedServerOutput out seq <$> getCurrentTime

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        NodeState{currentSlot, headState} <- atomically getLatestNodeState
        SyncedStatus{status, point} <- chainSyncedStatus
        if status && currentSlot <= chainSlotFromPoint point
          then do
            traceWith tracer (APIInputReceived $ toJSON input)
            case input of
              SafeClose ->
                case HeadState.getOpenStateConfirmedSnapshot headState of
                  Nothing -> callback input
                  Just confirmedSnapshot ->
                    case checkNonADAAssets confirmedSnapshot of
                      Left nonADAValue -> do
                        let clientInput = decodeUtf8With lenientDecode $ toStrict msg
                        let errorStr = "Cannot SafeClose with non-ADA assets present: " <> show nonADAValue
                        sendTextData con $ Aeson.encode $ InvalidInput errorStr clientInput
                        traceWith tracer (APIInvalidInput errorStr clientInput)
                      Right _ -> callback input
              _ -> callback input
          else do
            let err = "Rejected: chain out of sync" :: Text
            sendTextData con (Aeson.encode $ InvalidInput (toString err) (decodeUtf8With lenientDecode $ toStrict msg))
            traceWith tracer (APIInvalidInput (toString err) (decodeUtf8With lenientDecode $ toStrict msg))
      Left e -> do
        -- XXX(AB): toStrict might be problematic as it implies consuming the full
        -- message to memory
        let clientInput = decodeUtf8With lenientDecode $ toStrict msg
        sendTextData con $ Aeson.encode $ InvalidInput e clientInput
        traceWith tracer (APIInvalidInput e clientInput)

  forwardHistory con config@ServerOutputConfig{addressInTx} = do
    runConduitRes $ history .| filter (isAddressInTx addressInTx . Left) .| mapM_C (liftIO . sendTextData con . prepareServerOutput config)

  isAddressInTx addressInTx = \case
    Left tx -> checkAddress tx
    Right _ -> True
   where
    checkAddress tx =
      case addressInTx of
        WithAddressedTx addr -> txContainsAddr tx addr
        WithoutAddressedTx -> True

  -- \| Get the content of 'headStatus' field in 'Greetings' message from the full 'HeadState'.
  getHeadStatus :: HeadState tx -> HeadStatus
  getHeadStatus = \case
    HeadState.Idle{} -> Idle
    HeadState.Initial{} -> Initializing
    HeadState.Open{} -> Open
    HeadState.Closed ClosedState{readyToFanoutSent}
      | readyToFanoutSent -> FanoutPossible
      | otherwise -> Closed

  getHeadId :: HeadState tx -> Maybe HeadId
  getHeadId = \case
    HeadState.Idle{} -> Nothing
    HeadState.Initial InitialState{headId} -> Just headId
    HeadState.Open OpenState{headId} -> Just headId
    HeadState.Closed ClosedState{headId} -> Just headId
