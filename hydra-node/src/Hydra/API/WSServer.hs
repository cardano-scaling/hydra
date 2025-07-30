{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, filter, readTVar, seq)

import Conduit (ConduitT, ResourceT, mapM_C, runConduitRes, (.|))
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Control.Lens ((.~))
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens (atKey)
import Data.Conduit.Combinators (filter)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Projection (Projection (..))
import Hydra.API.ServerOutput (
  ClientMessage,
  Greetings (..),
  HeadStatus (..),
  InvalidInput (..),
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
import Hydra.Chain.ChainState (
  IsChainState,
 )
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic (ClosedState (ClosedState, readyToFanoutSent), HeadState, InitialState (..), OpenState (..), StateChanged)
import Hydra.HeadLogic.State qualified as HeadState
import Hydra.Logging (Tracer, traceWith)
import Hydra.NetworkVersions qualified as NetworkVersions
import Hydra.Node.Environment (Environment (..))
import Hydra.Tx (HeadParameters (..), Party)
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
  ConduitT () (TimedServerOutput tx) (ResourceT IO) () ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (StateChanged tx) (HeadState tx) ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ServerOutputFilter tx ->
  PendingConnection ->
  IO ()
wsApp env party tracer history callback headStateP responseChannel ServerOutputFilter{txContainsAddr} pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
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
    headState <- atomically getLatest
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
            }

  Projection{getLatest} = headStateP

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

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        traceWith tracer (APIInputReceived $ toJSON input)
        callback input
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

  -- | Get the content of 'headStatus' field in 'Greetings' message from the full 'HeadState'.
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
