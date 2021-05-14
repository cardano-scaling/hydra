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
import Hydra.Logic (OnChainTx)
import System.ZMQ4.Monadic

startChain :: String -> String -> IO ()
startChain chainSyncAddress postTxAddress = do
  txQueue <- atomically $ newTBQueue 50
  publisher <- runZMQ $ async $ transactionPublisher chainSyncAddress txQueue
  link publisher

  runZMQ $ transactionListener postTxAddress txQueue

transactionListener :: String -> TBQueue OnChainTx -> ZMQ z ()
transactionListener postTxAddress txQueue = do
  rep <- socket Rep
  bind rep postTxAddress
  forever $ do
    msg <- unpack . Enc.decodeUtf8 <$> receive rep
    hPutStrLn @Text stdout $ "received message " <> show msg
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
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    hPutStrLn @Text stdout $ "sending transaction " <> show tx
    send pub [] (Enc.encodeUtf8 $ show tx)

mockChainClient :: MonadIO m => String -> OnChainTx -> m ()
mockChainClient postTxAddress tx = runZMQ $ do
  req <- socket Req
  connect req postTxAddress
  send req [] (Enc.encodeUtf8 $ show tx)
  liftIO $ hPutStrLn @Text stdout ("sent message " <> show tx)
  resp <- receive req
  case resp of
    "OK" -> liftIO (hPutStrLn @Text stdout "received OK ") >> pure ()
    _ -> panic $ "Something went wrong posting " <> show tx

startChainSync :: MonadIO m => String -> (OnChainTx -> IO ()) -> m (Async ())
startChainSync chainSyncAddress handler = runZMQ $ do
  sub <- socket Sub
  connect sub chainSyncAddress
  async $
    forever $ do
      msg <- unpack . Enc.decodeUtf8 <$> receive sub
      liftIO $ hPutStrLn @Text stderr ("received message " <> show msg)
      case reads msg of
        (tx, "") : _ -> liftIO $ handler tx
        _ -> panic $ "cannot decode transaction " <> show msg
