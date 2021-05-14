{-# LANGUAGE TypeApplications #-}

-- | A 0MQ based mock chain implementation.
--
--This module provides both the "server-side" of the mock-chain and the "client-side" which
--can be used by `HydraNode` to post and receive TX from the mainchain
module Hydra.MockZMQChain where

import Cardano.Prelude hiding (Option, async, option)
import Control.Concurrent.STM (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Data.String
import Data.Text (unpack)
import qualified Data.Text.Encoding as Enc
import Hydra.Ledger.MockTx
import System.ZMQ4.Monadic

startChain :: String -> String -> IO ()
startChain chainSyncAddress postTxAddress = do
  txQueue <- atomically $ newTBQueue 50
  publisher <- runZMQ $ async $ transactionPublisher chainSyncAddress txQueue
  link publisher

  runZMQ $ transactionListener postTxAddress txQueue

transactionListener :: String -> TBQueue MockTx -> ZMQ z ()
transactionListener postTxAddress txQueue = do
  rep <- socket Rep
  bind rep postTxAddress
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    case reads msg of
      (txid, "") : _ -> do
        liftIO $ atomically $ writeTBQueue txQueue (ValidTx txid)
        send rep [] "OK"
      _ -> do
        hPutStrLn stderr $ "cannot decode " <> msg <> " as a valid transaction"
        send rep [] "KO"

transactionPublisher :: String -> TBQueue MockTx -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue = do
  pub <- socket Pub
  bind pub chainSyncAddress
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    case tx of
      ValidTx txid -> send pub [] (Enc.encodeUtf8 $ show txid)
      InvalidTx -> liftIO $ hPutStrLn @Text stderr "Got invalid transaction"
