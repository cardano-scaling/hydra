{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude

import Data.Text (pack)
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Node (HydraNode, createHydraNode, createMockChainClient, handleClientRequest, runHydraNode)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), accept, bind, defaultProtocol, listen, socket, socketToHandle)
import System.Directory (removeFile)
import System.IO (hGetLine, hPrint)

main :: IO ()
main = do
  [nodeId] <- getArgs
  -- TODO(SN): this is leaking resources (definitely)
  h <- openUnixSocket ("/tmp/hydra.socket." <> nodeId)
  case readMaybe nodeId of
    Nothing -> panic $ "invalid nodeId argument, should be a number: " <> pack nodeId
    Just n -> do
      node <- createHydraNode n Ledger.mockLedger createMockChainClient (hPrint h)
      race_
        (runAPIServer @Ledger.MockTx h node)
        (runHydraNode node)

runAPIServer :: Read tx => Handle -> HydraNode tx IO -> IO ()
runAPIServer h node = forever $ do
  threadDelay 100_000
  try @IOException (hGetLine h) >>= \case
    Left ex -> putText $ "[API] error reading: " <> show ex
    Right input -> case readMaybe input of
      Just command -> handleClientRequest node command
      Nothing -> hPutStrLn h $ "[API] Invalid command: " <> input

openUnixSocket :: FilePath -> IO Handle
openUnixSocket socketPath = do
  void $ try @IOException $ removeFile socketPath
  s <- socket AF_UNIX Stream defaultProtocol
  bind s $ SockAddrUnix socketPath
  listen s 1
  (conn, _peer) <- accept s
  putText "[API] Accepted connection"
  socketToHandle conn ReadWriteMode
