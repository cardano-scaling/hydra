{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Main where

import Cardano.Prelude

import Data.Text (pack)
import Hydra.Ledger (cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (Party (Party))
import Hydra.Node (HydraNode, createHydraNode, handleClientRequest, runHydraNode)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), bind, defaultProtocol, listen, socket, socketToHandle)
import System.Directory (removeFile)
import System.IO (hGetLine, hPrint)

main :: IO ()
main = do
  [nodeId] <- getArgs
  h <- openUnixSocket ("hydra.socket." <> nodeId)
  case readMaybe nodeId of
    Just n -> do
      node <- createHydraNode (Party n) ledger (hPrint h)
      _ <- async $ runAPIServer h node
      runHydraNode node
    Nothing -> panic $ "invalid nodeId argument, should be a number: " <> pack nodeId
 where
  ledger = cardanoLedger defaultEnv

  defaultEnv = MaryTest.mkLedgerEnv

runAPIServer :: Handle -> HydraNode tx IO -> IO a0
runAPIServer h node = forever $ do
  input <- hGetLine h
  case readMaybe input of
    Just command -> handleClientRequest node command
    Nothing -> hPutStrLn h $ "Invalid command: " <> input

openUnixSocket :: FilePath -> IO Handle
openUnixSocket socketPath = do
  void $ try @IOException $ removeFile socketPath
  s <- socket AF_UNIX Stream defaultProtocol
  bind s $ SockAddrUnix socketPath
  listen s 1
  socketToHandle s ReadWriteMode
