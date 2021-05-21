{-# LANGUAGE TypeApplications #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.MockZMQChain where

import Cardano.Prelude hiding (Option, async, option)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, TVar, modifyTVar', newTBQueue, newTVarIO, readTBQueue, readTVarIO, writeTBQueue)
import Data.String
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as Enc
import Hydra.Logic (OnChainTx)
import System.ZMQ4.Monadic

-- TODO: replace crude putText with proper logging
startChain :: String -> String -> String -> IO ()
startChain chainSyncAddress chainCatchupAddress postTxAddress = do
  txQueue <- atomically $ newTBQueue 50
  transactionLog <- newTVarIO []
  putText $ "[MockChain] starting at addresses (" <> pack chainSyncAddress <> ", " <> pack chainCatchupAddress <> ", " <> pack postTxAddress <> ")"
  concurrently_
    ( runZMQ
        ( transactionSyncer
            chainCatchupAddress
            transactionLog
        )
    )
    $ concurrently_
      ( runZMQ
          ( transactionPublisher
              chainSyncAddress
              txQueue
          )
      )
      ( runZMQ
          ( transactionListener
              postTxAddress
              transactionLog
              txQueue
          )
      )

transactionListener :: String -> TVar [OnChainTx] -> TBQueue OnChainTx -> ZMQ z ()
transactionListener postTxAddress transactionLog txQueue = do
  rep <- socket Rep
  bind rep postTxAddress
  putText $ "[MockChain] transaction listener started on " <> pack postTxAddress
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    putText $ "[MockChain] received message " <> show msg
    case reads msg of
      (tx, "") : _ -> do
        liftIO $
          atomically $ do
            writeTBQueue txQueue tx
            modifyTVar' transactionLog (<> [tx])
        send rep [] "OK"
      _ -> do
        hPutStrLn stderr $ "[MockChain] cannot decode " <> msg <> " as a valid transaction"
        send rep [] "KO"

transactionPublisher :: String -> TBQueue OnChainTx -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue = do
  pub <- socket Pub
  bind pub chainSyncAddress
  putText $ "[MockChain] transaction publisher started on " <> pack chainSyncAddress
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    putText $ "[MockChain] sending transaction " <> show tx
    send pub [] (Enc.encodeUtf8 $ show tx)

transactionSyncer :: String -> TVar [OnChainTx] -> ZMQ z ()
transactionSyncer chainCatchupAddress transactionLog = do
  rep <- socket Rep
  bind rep chainCatchupAddress
  putText $ "[MockChain] transaction syncer started on " <> pack chainCatchupAddress
  forever $ do
    _ <- receive rep
    txs <- liftIO $ readTVarIO transactionLog
    putText $ "[MockChain] received sync request, sending " <> show (length txs) <> " transactions"
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
