{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.MockZMQChain where

import Cardano.Prelude hiding (Option, async, option)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, TVar, modifyTVar', newTBQueue, newTVarIO, readTBQueue, readTVarIO, writeTBQueue)
import Data.String
import Data.Text (unpack)
import qualified Data.Text.Encoding as Enc
import Hydra.Logging (Log (..), Tracer, traceEvent)
import Hydra.Logic (OnChainTx)
import System.ZMQ4.Monadic

data MockChainLog
  = MockChainStarted {syncAddress :: String, catchupAddress :: String, postAddress :: String}
  | TransactionListenerStarted {postAddress :: String}
  | MessageReceived {message :: String}
  | FailedToDecodeMessage {message :: String}
  | TransactionPublisherStarted {postAddress :: String}
  | PublishingTransaction {transaction :: OnChainTx}
  | TransactionSyncerStarted {catchupAddress :: String}
  | SyncingTransactions {numberOfTransactions :: Int}
  | TransactionPosted {postAddress :: String, transaction :: OnChainTx}
  | ChainSyncStarted {syncAddress :: String}
  | ReceivedTransaction {transaction :: OnChainTx}
  | CatchingUpTransactions {catchupAddress :: String, numberOfTransactions :: Int}
  deriving (Show)

startChain :: String -> String -> String -> Tracer IO (Log MockChainLog) -> IO ()
startChain chainSyncAddress chainCatchupAddress postTxAddress tracer = do
  txQueue <- atomically $ newTBQueue 50
  transactionLog <- newTVarIO []
  traceEvent tracer (MockChainStarted chainSyncAddress chainCatchupAddress postTxAddress)
  concurrently_
    ( runZMQ
        ( transactionSyncer
            chainCatchupAddress
            transactionLog
            tracer
        )
    )
    $ concurrently_
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

transactionListener :: String -> TVar [OnChainTx] -> TBQueue OnChainTx -> Tracer IO (Log MockChainLog) -> ZMQ z ()
transactionListener postTxAddress transactionLog txQueue tracer = do
  rep <- socket Rep
  bind rep postTxAddress
  liftIO $ traceEvent tracer (TransactionListenerStarted postTxAddress)
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    liftIO $ traceEvent tracer (MessageReceived msg)
    case reads msg of
      (tx, "") : _ -> do
        liftIO $
          atomically $ do
            writeTBQueue txQueue tx
            modifyTVar' transactionLog (<> [tx])
        send rep [] "OK"
      _ -> do
        liftIO $ traceEvent tracer (FailedToDecodeMessage msg)
        send rep [] "KO"

transactionPublisher :: String -> TBQueue OnChainTx -> Tracer IO (Log MockChainLog) -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue tracer = do
  pub <- socket Pub
  bind pub chainSyncAddress
  liftIO $ traceEvent tracer (TransactionPublisherStarted chainSyncAddress)
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    liftIO $ traceEvent tracer (PublishingTransaction tx)
    send pub [] (Enc.encodeUtf8 $ show tx)

transactionSyncer :: String -> TVar [OnChainTx] -> Tracer IO (Log MockChainLog) -> ZMQ z ()
transactionSyncer chainCatchupAddress transactionLog tracer = do
  rep <- socket Rep
  bind rep chainCatchupAddress
  liftIO $ traceEvent tracer (TransactionSyncerStarted chainCatchupAddress)
  forever $ do
    _ <- receive rep
    txs <- liftIO $ readTVarIO transactionLog
    liftIO $ traceEvent tracer (SyncingTransactions $ length txs)
    send rep [] (Enc.encodeUtf8 $ show txs)

mockChainClient :: MonadIO m => String -> Tracer IO (Log MockChainLog) -> OnChainTx -> m ()
mockChainClient postTxAddress tracer tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (Enc.encodeUtf8 $ show tx)
  resp <- receive req
  case resp of
    "OK" -> liftIO (traceEvent tracer (TransactionPosted postTxAddress tx)) >> pure ()
    _ -> panic $ "Something went wrong posting " <> show tx

runChainSync :: MonadIO m => String -> (OnChainTx -> IO ()) -> Tracer IO (Log MockChainLog) -> m ()
runChainSync chainSyncAddress handler tracer = do
  runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub chainSyncAddress
    liftIO (traceEvent tracer (ChainSyncStarted chainSyncAddress))
    forever $ do
      msg <- unpack . Enc.decodeUtf8 <$> receive sub
      case reads msg of
        (tx, "") : _ -> liftIO $ do
          traceEvent tracer (ReceivedTransaction tx)
          handler tx
        _ -> panic $ "cannot decode transaction " <> show msg

catchUpTransactions :: String -> (OnChainTx -> IO ()) -> Tracer IO (Log MockChainLog) -> IO ()
catchUpTransactions catchUpAddress handler tracer = runZMQ $ do
  req <- socket Req
  connect req catchUpAddress
  send req [] "Hello"
  message <- unpack . Enc.decodeUtf8 <$> receive req
  case readMaybe message of
    Just (txs :: [OnChainTx]) -> liftIO $ do
      traceEvent tracer (CatchingUpTransactions catchUpAddress $ length txs)
      forM_ txs handler
    Nothing -> panic $ "cannot decode catch-up transactions  " <> show message
