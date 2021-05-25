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
import Hydra.Logic (OnChainTx)
import Logging (Tracer, traceWith)
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
  deriving (Show)

startChain :: String -> String -> String -> Tracer IO MockChainLog -> IO ()
startChain chainSyncAddress chainCatchupAddress postTxAddress tracer = do
  txQueue <- atomically $ newTBQueue 50
  transactionLog <- newTVarIO []
  traceWith tracer (MockChainStarted chainSyncAddress chainCatchupAddress postTxAddress)
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

transactionListener :: String -> TVar [OnChainTx] -> TBQueue OnChainTx -> Tracer IO MockChainLog -> ZMQ z ()
transactionListener postTxAddress transactionLog txQueue tracer = do
  rep <- socket Rep
  bind rep postTxAddress
  liftIO $ traceWith tracer (TransactionListenerStarted postTxAddress)
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    liftIO $ traceWith tracer (MessageReceived msg)
    case reads msg of
      (tx, "") : _ -> do
        liftIO $
          atomically $ do
            writeTBQueue txQueue tx
            modifyTVar' transactionLog (<> [tx])
        send rep [] "OK"
      _ -> do
        liftIO $ traceWith tracer (FailedToDecodeMessage msg)
        send rep [] "KO"

transactionPublisher :: String -> TBQueue OnChainTx -> Tracer IO MockChainLog -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue tracer = do
  pub <- socket Pub
  bind pub chainSyncAddress
  liftIO $ traceWith tracer (TransactionPublisherStarted chainSyncAddress)
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    liftIO $ traceWith tracer (PublishingTransaction tx)
    send pub [] (Enc.encodeUtf8 $ show tx)

transactionSyncer :: String -> TVar [OnChainTx] -> Tracer IO MockChainLog -> ZMQ z ()
transactionSyncer chainCatchupAddress transactionLog tracer = do
  rep <- socket Rep
  bind rep chainCatchupAddress
  liftIO $ traceWith tracer (TransactionSyncerStarted chainCatchupAddress)
  forever $ do
    _ <- receive rep
    txs <- liftIO $ readTVarIO transactionLog
    liftIO $ traceWith tracer (SyncingTransactions $ length txs)
    send rep [] (Enc.encodeUtf8 $ show txs)

mockChainClient :: MonadIO m => String -> OnChainTx -> m ()
mockChainClient postTxAddress tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (Enc.encodeUtf8 $ show tx)
  resp <- receive req
  case resp of
    "OK" -> pure ()
    _ -> panic $ "Something went wrong posting " <> show tx

runChainSync :: MonadIO m => String -> (OnChainTx -> IO ()) -> m ()
runChainSync chainSyncAddress handler = do
  runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    connect sub chainSyncAddress
    forever $ do
      msg <- unpack . Enc.decodeUtf8 <$> receive sub
      case reads msg of
        (tx, "") : _ -> liftIO $ handler tx
        _ -> panic $ "cannot decode transaction " <> show msg

catchUpTransactions :: String -> (OnChainTx -> IO ()) -> IO ()
catchUpTransactions catchUpAddress handler = runZMQ $ do
  req <- socket Req
  connect req catchUpAddress
  send req [] "Hello"
  message <- unpack . Enc.decodeUtf8 <$> receive req
  case readMaybe message of
    Just (txs :: [OnChainTx]) -> liftIO $ do
      putText $ "catch-up  " <> show (length txs) <> " transactions"
      forM_ txs handler
    Nothing -> panic $ "cannot decode catch-up transactions  " <> show message
