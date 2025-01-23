{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, seq)

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar (TVar)
import Data.Aeson qualified as Aeson
import Data.Version (showVersion)
import Hydra.API.APIServerLog (APIServerLog (..))
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.Projection (Projection (..))
import Hydra.API.ServerOutput (
  HeadStatus,
  ServerOutput (Greetings, InvalidInput, hydraHeadId, hydraNodeVersion),
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
  STM IO Natural ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (ServerOutput tx) HeadStatus ->
  -- | Read model to enhance 'Greetings' messages with 'HeadId'.
  Projection STM.STM (ServerOutput tx) (Maybe HeadId) ->
  -- | Read model to enhance 'Greetings' messages with snapshot UTxO.
  Projection STM.STM (ServerOutput tx) (Maybe (UTxOType tx)) ->
  TChan (TimedServerOutput tx) ->
  ServerOutputFilter tx ->
  PendingConnection ->
  IO ()
wsApp party tracer nextSeq callback headStatusP headIdP snapshotUtxoP responseChannel ServerOutputFilter{txContainsAddr} pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
  chan <- STM.atomically $ dupTChan responseChannel

  let outConfig = mkServerOutputConfig queryParams

  -- FIXME: No support of history forwarding anymore (disabled because of memory growing too much)
  -- api client can decide if they want to see the past history of server outputs
  -- unless (shouldNotServeHistory queryParams) $
  --   forwardHistory con outConfig

  -- forwardGreetingOnly con
  sendGreetings con

  withPingThread con 30 (pure ()) $
    race_ (receiveInputs con) (sendOutputs chan con outConfig)
 where
  -- NOTE: We will add a 'Greetings' message on each API server start. This is
  -- important to make sure the latest configured 'party' is reaching the
  -- client.
  sendGreetings con = do
    seq <- atomically nextSeq
    headStatus <- atomically getLatestHeadStatus
    hydraHeadId <- atomically getLatestHeadId
    snapshotUtxo <- atomically getLatestSnapshotUtxo
    time <- getCurrentTime

    sendTextData con $
      Aeson.encode
        TimedServerOutput
          { time
          , seq
          , output =
              Greetings
                { me = party
                , headStatus
                , hydraHeadId
                , snapshotUtxo
                , hydraNodeVersion = showVersion Options.hydraNodeVersion
                } ::
                ServerOutput tx
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

  -- shouldNotServeHistory qp =
  --   flip any qp $ \case
  --     (QueryParam key val)
  --       | key == [queryKey|history|] -> val == [queryValue|no|]
  --     _other -> False

  sendOutputs chan con outConfig@ServerOutputConfig{addressInTx} = forever $ do
    response <- STM.atomically $ readTChan chan
    when (isAddressInTx addressInTx response) $
      sendResponse response
   where
    sendResponse response = do
      let sentResponse = prepareServerOutput outConfig response
      sendTextData con sentResponse
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
        time <- getCurrentTime
        seq <- atomically nextSeq
        let timedOutput = TimedServerOutput{output = InvalidInput @tx e clientInput, time, seq}
        sendTextData con $ Aeson.encode timedOutput
        traceWith tracer (APIInvalidInput e clientInput)

  -- forwardHistory con ServerOutputConfig{addressInTx} = do
  --   rawHist <- STM.atomically (readTVar history)
  --   let hist = filter (isAddressInTx addressInTx) rawHist
  --   let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
  --   sendTextDatas con $ foldl' encodeAndReverse [] hist

  isAddressInTx addressInTx tx =
    case addressInTx of
      WithAddressedTx addr -> txContainsAddr tx addr
      WithoutAddressedTx -> True

nextSequenceNumber :: TVar [TimedServerOutput tx] -> STM.STM Natural
nextSequenceNumber historyList =
  STM.readTVar historyList >>= \case
    [] -> pure 0
    (TimedServerOutput{seq} : _) -> pure (seq + 1)
