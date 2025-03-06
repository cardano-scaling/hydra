{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, filter, readTVar, seq)

import Conduit (ConduitT, ResourceT, mapM_C, runConduitRes, (.|))
import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Data.Aeson qualified as Aeson
import Data.Conduit.Combinators (filter)
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Projection (Projection (..))
import Hydra.API.ServerOutput (
  ClientMessage,
  Greetings (..),
  HeadStatus,
  InvalidInput (..),
  ServerOutputConfig (..),
  TimedServerOutput (..),
  WithAddressedTx (..),
  WithUTxO (..),
  headStatus,
  me,
  prepareServerOutput,
  snapshotUtxo,
 )
import Hydra.API.ServerOutputFilter (
  ServerOutputFilter (..),
 )
import Hydra.Chain.ChainState (
  IsChainState,
 )
import Hydra.Chain.Direct.State ()
import Hydra.HeadLogic (StateChanged)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Options qualified as Options
import Hydra.Tx (Party, UTxOType)
import Hydra.Tx.HeadId (HeadId (..))
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
  Party ->
  Tracer IO APIServerLog ->
  ConduitT () (TimedServerOutput tx) (ResourceT IO) () ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (StateChanged tx) HeadStatus ->
  -- | Read model to enhance 'Greetings' messages with 'HeadId'.
  Projection STM.STM (StateChanged tx) (Maybe HeadId) ->
  -- | Read model to enhance 'Greetings' messages with snapshot UTxO.
  Projection STM.STM (StateChanged tx) (Maybe (UTxOType tx)) ->
  TChan (Either (TimedServerOutput tx) (ClientMessage tx)) ->
  ServerOutputFilter tx ->
  PendingConnection ->
  IO ()
wsApp party tracer history callback headStatusP headIdP snapshotUtxoP responseChannel ServerOutputFilter{txContainsAddr} pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
  chan <- STM.atomically $ dupTChan responseChannel

  let outConfig = mkServerOutputConfig queryParams

  -- api client can decide if they want to see the past history of server outputs
  when (shouldServeHistory queryParams) $
    forwardHistory con outConfig

  forwardGreetingOnly con

  withPingThread con 30 (pure ()) $
    race_ (receiveInputs con) (sendOutputs chan con outConfig)
 where
  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  forwardGreetingOnly con = do
    headStatus <- atomically getLatestHeadStatus
    hydraHeadId <- atomically getLatestHeadId
    snapshotUtxo <- atomically getLatestSnapshotUtxo

    sendTextData con $
      Aeson.encode
        Greetings
          { me = party
          , headStatus
          , hydraHeadId
          , snapshotUtxo
          , hydraNodeVersion = showVersion Options.hydraNodeVersion
          }

  Projection{getLatest = getLatestHeadStatus} = headStatusP
  Projection{getLatest = getLatestHeadId} = headIdP
  Projection{getLatest = getLatestSnapshotUtxo} = snapshotUtxoP

  mkServerOutputConfig qp =
    ServerOutputConfig
      { utxoInSnapshot = decideOnUTxODisplay qp
      , addressInTx = decideOnAddressDisplay qp
      }

  decideOnUTxODisplay qp =
    let k = [queryKey|snapshot-utxo|]
        v = [queryValue|no|]
        queryP = QueryParam k v
     in if queryP `elem` qp then WithoutUTxO else WithUTxO

  decideOnAddressDisplay qp =
    case find queryByAddress qp of
      Just (QueryParam _ v) -> WithAddressedTx (unRText v)
      _ -> WithoutAddressedTx
   where
    queryByAddress = \case
      (QueryParam key _) | key == [queryKey|address|] -> True
      _other -> False

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
        sendTextData con (Aeson.encode response)
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

  forwardHistory con ServerOutputConfig{addressInTx} = do
    runConduitRes $ history .| filter (isAddressInTx addressInTx . Left) .| mapM_C (liftIO . sendTextData con . Aeson.encode)

  isAddressInTx addressInTx = \case
    Left tx -> checkAddress tx
    Right _ -> True
   where
    checkAddress tx =
      case addressInTx of
        WithAddressedTx addr -> txContainsAddr tx addr
        WithoutAddressedTx -> True
