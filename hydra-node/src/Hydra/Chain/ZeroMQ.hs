{-# LANGUAGE UndecidableInstances #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.Chain.ZeroMQ where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTBQueue, newTVarIO, readTBQueue, readTVarIO, writeTBQueue)
import Data.Aeson (eitherDecodeStrict, encode)
import qualified Data.Text.Encoding as Enc
import Hydra.Chain (Chain (..), ChainComponent, OnChainTx, PostChainTx, toOnChainTx)
import Hydra.Ledger (IsTx, UTxOType)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (MockChain (..))
import qualified Hydra.Network as Network
import System.ZMQ4.Monadic (
  Pub (..),
  Rep (..),
  Req (..),
  Sub (..),
  ZMQ,
  bind,
  connect,
  receive,
  runZMQ,
  send,
  socket,
  subscribe,
 )

data MockChainLog tx
  = ConnectingToMockChain {chain :: MockChain}
  | MockChainStarted {syncAddress :: String, catchupAddress :: String, postAddress :: String}
  | TransactionListenerStarted {postAddress :: String}
  | MessageReceived {message :: String}
  | FailedToDecodeMessage {message :: String}
  | TransactionPublisherStarted {postAddress :: String}
  | PublishingTransaction {publishedTransaction :: PostChainTx tx}
  | TransactionSyncerStarted {catchupAddress :: String}
  | SyncingTransactions {numberOfTransactions :: Int}
  | TransactionPosted {postAddress :: String, postedTransaction :: PostChainTx tx}
  | ChainSyncStarted {syncAddress :: String}
  | ReceivedTransaction {receivedTransaction :: OnChainTx tx}
  | CatchingUpTransactions {catchupAddress :: String, numberOfTransactions :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (MockChainLog tx) where
  arbitrary = genericArbitrary

startChain :: IsTx tx => String -> String -> String -> Tracer IO (MockChainLog tx) -> IO ()
startChain chainSyncAddress chainCatchupAddress postTxAddress tracer = do
  txQueue <- atomically $ newTBQueue 50
  transactionLog <- newTVarIO []
  traceWith tracer (MockChainStarted chainSyncAddress chainCatchupAddress postTxAddress)
  race_
    ( runZMQ
        ( transactionSyncer
            chainCatchupAddress
            transactionLog
            tracer
        )
    )
    $ race_
      ( runZMQ
          ( transactionPublisher
              chainSyncAddress
              txQueue
              tracer
          )
      )
      ( runZMQ
          ( transactionListener
              postTxAddress
              transactionLog
              txQueue
              tracer
          )
      )

transactionListener :: IsTx tx => String -> TVar IO [OnChainTx tx] -> TBQueue IO (PostChainTx tx) -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionListener postTxAddress transactionLog txQueue tracer = do
  rep <- socket Rep
  bind rep postTxAddress
  liftIO $ traceWith tracer (TransactionListenerStarted postTxAddress)
  forever $ do
    msg <- receive rep
    liftIO $ traceWith tracer (MessageReceived $ toString . Enc.decodeUtf8 $ msg)
    case eitherDecodeStrict msg of
      Right tx -> do
        liftIO $ do
          time <- getCurrentTime
          atomically $ do
            writeTBQueue txQueue tx
            modifyTVar' transactionLog (<> [toOnChainTx time tx])
        send rep [] "OK"
      Left other -> do
        liftIO $ traceWith tracer (FailedToDecodeMessage (show msg <> ", error: " <> show other))
        send rep [] "KO"

transactionPublisher :: IsTx tx => String -> TBQueue IO (PostChainTx tx) -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue tracer = do
  pub <- socket Pub
  bind pub chainSyncAddress
  liftIO $ traceWith tracer (TransactionPublisherStarted chainSyncAddress)
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    liftIO $ traceWith tracer (PublishingTransaction tx)
    time <- liftIO getCurrentTime
    send pub [] (toStrict . encode $ toOnChainTx time tx)

transactionSyncer :: IsTx tx => String -> TVar IO [OnChainTx tx] -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionSyncer chainCatchupAddress transactionLog tracer = do
  rep <- socket Rep
  bind rep chainCatchupAddress
  liftIO $ traceWith tracer (TransactionSyncerStarted chainCatchupAddress)
  forever $ do
    _ <- receive rep
    txs <- liftIO $ readTVarIO transactionLog
    liftIO $ traceWith tracer (SyncingTransactions $ length txs)
    send rep [] (toStrict $ encode txs)

mockChainClient :: (IsTx tx, MonadIO m) => String -> Tracer IO (MockChainLog tx) -> PostChainTx tx -> m ()
mockChainClient postTxAddress tracer tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (toStrict $ encode tx)
  resp <- receive req
  case resp of
    "OK" -> liftIO (traceWith tracer (TransactionPosted postTxAddress tx)) >> pure ()
    _ -> error $ "Something went wrong posting " <> show tx <> ", error: " <> show resp

runChainSync :: (IsTx tx, MonadIO m) => String -> (OnChainTx tx -> IO ()) -> Tracer IO (MockChainLog tx) -> m ()
runChainSync chainSyncAddress handler tracer = do
  runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub chainSyncAddress
    liftIO (traceWith tracer (ChainSyncStarted chainSyncAddress))
    forever $ do
      msg <- receive sub
      case eitherDecodeStrict msg of
        Right tx -> liftIO $ do
          traceWith tracer (ReceivedTransaction tx)
          handler tx
        Left{} -> error $ "cannot decode transaction " <> show msg

catchUpTransactions :: IsTx tx => String -> (OnChainTx tx -> IO ()) -> Tracer IO (MockChainLog tx) -> IO ()
catchUpTransactions catchUpAddress handler tracer = runZMQ $ do
  req <- socket Req
  connect req catchUpAddress
  send req [] "Hello"
  message <- receive req
  case eitherDecodeStrict message of
    Right (txs :: [OnChainTx tx]) -> liftIO $ do
      traceWith tracer (CatchingUpTransactions catchUpAddress $ length txs)
      forM_ txs handler
    Left{} -> error $ "cannot decode catch-up transactions  " <> show message

withMockChain ::
  IsTx tx =>
  Tracer IO (MockChainLog tx) ->
  MockChain ->
  ChainComponent tx IO ()
withMockChain tracer mockChain@MockChain{mockChainHost, Network.syncPort = chainSyncPort, catchUpPort, postTxPort} callback action = do
  traceWith tracer $ ConnectingToMockChain mockChain
  catchUpTransactions ("tcp://" <> mockChainHost <> ":" <> show catchUpPort) callback tracer
  runChainSync ("tcp://" <> mockChainHost <> ":" <> show chainSyncPort) callback tracer
    `race_` action chain
 where
  chain = Chain{postTx = mockChainClient ("tcp://" <> mockChainHost <> ":" <> show postTxPort) tracer}
