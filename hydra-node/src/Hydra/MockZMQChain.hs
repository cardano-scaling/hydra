{-# LANGUAGE TypeApplications #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.MockZMQChain where

import Cardano.Prelude hiding (Option, async, option)
import Control.Concurrent.STM (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Data.String
import Data.Text (pack, unpack)
import qualified Data.Text.Encoding as Enc
import Hydra.Logic (OnChainTx)
import System.ZMQ4.Monadic

-- TODO: replace crude putText with proper logging
startChain :: String -> String -> IO ()
startChain chainSyncAddress postTxAddress = do
  txQueue <- atomically $ newTBQueue 50
  putText $ "Starting chain at addresses (" <> pack chainSyncAddress <> "," <> pack postTxAddress <> ")"
  runZMQ $ do
    async (transactionPublisher chainSyncAddress txQueue) >>= liftIO . link
    transactionListener postTxAddress txQueue

transactionListener :: String -> TBQueue OnChainTx -> ZMQ z ()
transactionListener postTxAddress txQueue = do
  rep <- socket Rep
  bind rep postTxAddress
  putText $ "transaction listener started on " <> pack postTxAddress
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    putText $ "received message " <> show msg
    case reads msg of
      (tx, "") : _ -> do
        liftIO $ atomically $ writeTBQueue txQueue tx
        send rep [] "OK"
      _ -> do
        hPutStrLn stderr $ "cannot decode " <> msg <> " as a valid transaction"
        send rep [] "KO"

transactionPublisher :: String -> TBQueue OnChainTx -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue = do
  pub <- socket Pub
  bind pub chainSyncAddress
  putText $ "transaction publisher started on " <> pack chainSyncAddress
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    putText $ "sending transaction " <> show tx
    send pub [] (Enc.encodeUtf8 $ show tx)

mockChainClient :: MonadIO m => String -> OnChainTx -> m ()
mockChainClient postTxAddress tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (Enc.encodeUtf8 $ show tx)
  liftIO $ putText ("sent message " <> show tx)
  resp <- receive req
  case resp of
    "OK" -> liftIO (putText "received OK ") >> pure ()
    _ -> panic $ "Something went wrong posting " <> show tx

runChainSync :: MonadIO m => String -> (OnChainTx -> IO ()) -> m ()
runChainSync chainSyncAddress handler = runZMQ $ do
  sub <- socket Sub
  subscribe sub ""
  connect sub chainSyncAddress
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive sub
    liftIO $ putText ("received message " <> show msg)
    case reads msg of
      (tx, "") : _ -> liftIO $ handler tx
      _ -> panic $ "cannot decode transaction " <> show msg
