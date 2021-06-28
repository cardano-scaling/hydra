{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.Chain.ZeroMQ where

import Hydra.Prelude

import Control.Monad.Class.MonadAsync (async, link)
import Control.Monad.Class.MonadSTM (modifyTVar', newTBQueue, newTVarIO, readTBQueue, writeTBQueue, readTVarIO)
import qualified Data.Text.Encoding as Enc
import Hydra.Chain (Chain (..))
import Hydra.HeadLogic (Event (OnChainEvent), OnChainTx)
import Hydra.Ledger (Tx)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Node (EventQueue (..))
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
  = MockChainStarted {syncAddress :: String, catchupAddress :: String, postAddress :: String}
  | TransactionListenerStarted {postAddress :: String}
  | MessageReceived {message :: String}
  | FailedToDecodeMessage {message :: String}
  | TransactionPublisherStarted {postAddress :: String}
  | PublishingTransaction {transaction :: OnChainTx tx}
  | TransactionSyncerStarted {catchupAddress :: String}
  | SyncingTransactions {numberOfTransactions :: Int}
  | TransactionPosted {postAddress :: String, transaction :: OnChainTx tx}
  | ChainSyncStarted {syncAddress :: String}
  | ReceivedTransaction {transaction :: OnChainTx tx}
  | CatchingUpTransactions {catchupAddress :: String, numberOfTransactions :: Int}
  deriving (Eq, Show)

startChain :: Tx tx => String -> String -> String -> Tracer IO (MockChainLog tx) -> IO ()
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

transactionListener :: Tx tx => String -> TVar IO [OnChainTx tx] -> TBQueue IO (OnChainTx tx) -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionListener postTxAddress transactionLog txQueue tracer = do
  rep <- socket Rep
  bind rep postTxAddress
  liftIO $ traceWith tracer (TransactionListenerStarted postTxAddress)
  forever $ do
    msg <- toString . Enc.decodeUtf8 <$> receive rep
    liftIO $ traceWith tracer (MessageReceived msg)
    case reads msg of
      (tx, "") : _ -> do
        liftIO $
          atomically $ do
            writeTBQueue txQueue tx
            modifyTVar' transactionLog (<> [tx])
        send rep [] "OK"
      other -> do
        liftIO $ traceWith tracer (FailedToDecodeMessage (msg <> ", error: " <> show other))
        send rep [] "KO"

transactionPublisher :: Tx tx => String -> TBQueue IO (OnChainTx tx) -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue tracer = do
  pub <- socket Pub
  bind pub chainSyncAddress
  liftIO $ traceWith tracer (TransactionPublisherStarted chainSyncAddress)
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    liftIO $ traceWith tracer (PublishingTransaction tx)
    send pub [] (Enc.encodeUtf8 $ show tx)

transactionSyncer :: Tx tx => String -> TVar IO [OnChainTx tx] -> Tracer IO (MockChainLog tx) -> ZMQ z ()
transactionSyncer chainCatchupAddress transactionLog tracer = do
  rep <- socket Rep
  bind rep chainCatchupAddress
  liftIO $ traceWith tracer (TransactionSyncerStarted chainCatchupAddress)
  forever $ do
    _ <- receive rep
    txs <- liftIO $ readTVarIO transactionLog
    liftIO $ traceWith tracer (SyncingTransactions $ length txs)
    send rep [] (Enc.encodeUtf8 $ show txs)

mockChainClient :: (Tx tx, MonadIO m) => String -> Tracer IO (MockChainLog tx) -> OnChainTx tx -> m ()
mockChainClient postTxAddress tracer tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (Enc.encodeUtf8 $ show tx)
  resp <- receive req
  case resp of
    "OK" -> liftIO (traceWith tracer (TransactionPosted postTxAddress tx)) >> pure ()
    _ -> error $ "Something went wrong posting " <> show tx <> ", error: " <> show resp

runChainSync :: (Tx tx, MonadIO m) => String -> (OnChainTx tx -> IO ()) -> Tracer IO (MockChainLog tx) -> m ()
runChainSync chainSyncAddress handler tracer = do
  runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub chainSyncAddress
    liftIO (traceWith tracer (ChainSyncStarted chainSyncAddress))
    forever $ do
      msg <- toString . Enc.decodeUtf8 <$> receive sub
      case reads msg of
        (tx, "") : _ -> liftIO $ do
          traceWith tracer (ReceivedTransaction tx)
          handler tx
        _ -> error $ "cannot decode transaction " <> show msg

catchUpTransactions :: Tx tx => String -> (OnChainTx tx -> IO ()) -> Tracer IO (MockChainLog tx) -> IO ()
catchUpTransactions catchUpAddress handler tracer = runZMQ $ do
  req <- socket Req
  connect req catchUpAddress
  send req [] "Hello"
  message <- toString . Enc.decodeUtf8 <$> receive req
  case readMaybe message of
    Just (txs :: [OnChainTx tx]) -> liftIO $ do
      traceWith tracer (CatchingUpTransactions catchUpAddress $ length txs)
      forM_ txs handler
    Nothing -> error $ "cannot decode catch-up transactions  " <> show message

createMockChainClient :: Tx tx => EventQueue IO (Event tx) -> Tracer IO (MockChainLog tx) -> IO (Chain tx IO)
createMockChainClient EventQueue{putEvent} tracer = do
  -- TODO: Do a proper cleanup of threads and what not
  -- BUG(SN): This should wait until we are connected to the chain, otherwise we
  -- might think that the 'OnChain' is ready, but it in fact would not see any
  -- txs from the chain. For now, we assume it takes 1 sec to connect.
  catchUpTransactions "tcp://127.0.0.1:56790" onTx tracer
  link =<< async (runChainSync "tcp://127.0.0.1:56789" onTx tracer)
  threadDelay 0.1
  pure Chain{postTx = sendTx}
 where
  sendTx tx = mockChainClient "tcp://127.0.0.1:56791" tracer tx

  onTx tx = putEvent $ OnChainEvent tx
