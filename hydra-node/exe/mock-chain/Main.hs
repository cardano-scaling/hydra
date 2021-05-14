{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Cardano.Prelude hiding (Option, async, option)
import Control.Concurrent.STM (TBQueue, newTBQueue, readTBQueue, writeTBQueue)
import Data.String
import Data.Text (unpack)
import qualified Data.Text.Encoding as Enc
import Hydra.Ledger.MockTx
import Options.Applicative
import System.ZMQ4.Monadic

data Option = Option
  { chainSyncAddress :: String
  , postTxAddress :: String
  }
  deriving (Eq, Show)

mockChainParser :: Parser Option
mockChainParser =
  Option
    <$> strOption
      ( long "sync-address"
          <> short 's'
          <> value "tls://127.0.0.1:56789"
          <> help "The address where clients can connect for syncing transactions"
      )
    <*> strOption
      ( long "post-address"
          <> short 'p'
          <> value "tls://127.0.0.1:56790"
          <> help "The address where clients can post transactions"
      )

mockChainOptions :: ParserInfo Option
mockChainOptions =
  info
    (mockChainParser <**> helper)
    ( fullDesc
        <> progDesc "Starts a mock Chain server"
        <> header "mock-chain - a mock chain server"
    )

main :: IO ()
main = do
  opts <- execParser mockChainOptions
  startChain opts

startChain :: Option -> IO ()
startChain Option{chainSyncAddress, postTxAddress} = do
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
      (txid, "") : _ -> liftIO $ atomically $ writeTBQueue txQueue (ValidTx txid)
      _ -> hPutStrLn stderr $ "cannot decode " <> msg <> " as a valid transaction"

transactionPublisher :: String -> TBQueue MockTx -> ZMQ z ()
transactionPublisher chainSyncAddress txQueue = do
  pub <- socket Pub
  bind pub chainSyncAddress
  forever $ do
    tx <- liftIO $ atomically $ readTBQueue txQueue
    case tx of
      ValidTx txid -> send pub [] (Enc.encodeUtf8 $ show txid)
      InvalidTx -> liftIO $ hPutStrLn @Text stderr "Got invalid transaction"
