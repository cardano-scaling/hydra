{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.WSServer where

import Hydra.Prelude hiding (TVar, readTVar, seq)

import Control.Concurrent.STM (TChan, dupTChan, readTChan)
import Control.Concurrent.STM qualified as STM
import Control.Concurrent.STM.TVar (TVar, readTVar)
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
  WithUTxO (..),
  headStatus,
  me,
  prepareServerOutput,
  snapshotUtxo,
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
  sendTextDatas,
  withPingThread,
 )
import Text.URI hiding (ParseException)
import Text.URI.QQ (queryKey, queryValue)

wsApp ::
  forall tx.
  IsChainState tx =>
  Party ->
  Tracer IO APIServerLog ->
  -- | Get next sequence number.
  STM IO Natural ->
  (ClientInput tx -> IO ()) ->
  -- | Read model to enhance 'Greetings' messages with 'HeadStatus'.
  Projection STM.STM (ServerOutput tx) HeadStatus ->
  -- | Read model to enhance 'Greetings' messages with 'HeadId'.
  Projection STM.STM (ServerOutput tx) (Maybe HeadId) ->
  -- | Read model to enhance 'Greetings' messages with snapshot UTxO.
  Projection STM.STM (ServerOutput tx) (Maybe (UTxOType tx)) ->
  TChan (TimedServerOutput tx) ->
  PendingConnection ->
  IO ()
wsApp party tracer nextSeq callback headStatusP headIdP snapshotUtxoP responseChannel pending = do
  traceWith tracer NewAPIConnection
  let path = requestPath $ pendingRequest pending
  queryParams <- uriQuery <$> mkURIBs path
  con <- acceptRequest pending
  chan <- STM.atomically $ dupTChan responseChannel

  -- FIXME: No support of history forwarding anymore (disabled because of memory growing too much)
  -- api client can decide if they want to see the past history of server outputs
  -- unless (shouldNotServeHistory queryParams) $
  --   forwardHistory con

  sendGreetings con

  let outConfig = mkServerOutputConfig queryParams

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
      }

  decideOnUTxODisplay qp =
    let k = [queryKey|snapshot-utxo|]
        v = [queryValue|no|]
        queryP = QueryParam k v
     in if queryP `elem` qp then WithoutUTxO else WithUTxO

  -- shouldNotServeHistory qp =
  --   flip any qp $ \case
  --     (QueryParam key val)
  --       | key == [queryKey|history|] -> val == [queryValue|no|]
  --     _other -> False

  sendOutputs chan con outConfig = forever $ do
    response <- STM.atomically $ readTChan chan
    let sentResponse =
          prepareServerOutput outConfig response

    sendTextData con sentResponse
  -- TODO: disabled for doom traceWith tracer (APIOutputSent $ toJSON response)

  receiveInputs con = forever $ do
    msg <- receiveData con
    case Aeson.eitherDecode msg of
      Right input -> do
        -- TODO: disabled for doom traceWith tracer (APIInputReceived $ toJSON input)
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

-- forwardHistory con = do
--   hist <- STM.atomically (readTVar history)
--   let encodeAndReverse xs serverOutput = Aeson.encode serverOutput : xs
--   sendTextDatas con $ foldl' encodeAndReverse [] hist
