{-# LANGUAGE TypeApplications #-}

module Main where

import Cardano.Prelude

import Data.Text (pack)
import qualified Hydra.Ledger.Mock as Ledger
import Hydra.Logic (
  Environment (..),
  Event (..),
  HeadParameters (..),
  SnapshotStrategy (..),
 )
import Hydra.Network.ZeroMQ (
  withZeroMQHydraNetwork,
 )
import Hydra.Node (
  EventQueue (..),
  HydraNode (..),
  createEventQueue,
  createHydraHead,
  createMockChainClient,
  handleClientRequest,
  runHydraNode,
 )
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), accept, bind, defaultProtocol, listen, socket, socketToHandle)
import System.Directory (removeFile)
import System.IO (BufferMode (LineBuffering), hGetLine, hPrint, hSetBuffering)

import qualified Hydra.Logic as Logic

main :: IO ()
main = do
  [nodeId] <- getArgs
  -- TODO(SN): this is leaking resources (definitely)
  h <- openUnixSocket ("/tmp/hydra.socket." <> nodeId)
  case readMaybe nodeId of
    Nothing -> panic $ "invalid nodeId argument, should be a number: " <> pack nodeId
    Just n -> do
      eq <- createEventQueue
      let headState = Logic.createHeadState [] HeadParameters SnapshotStrategy
      hh <- createHydraHead headState Ledger.mockLedger
      oc <- createMockChainClient eq
      withZeroMQHydraNetwork (me n) (them n) (putEvent eq . NetworkEvent) $ \hn -> do
        let sendResponse = hPrint h
        let env = Environment n
        let node = HydraNode{eq, hn, hh, oc, sendResponse, env}
        race_
          (runAPIServer @Ledger.MockTx h node)
          (runHydraNode node)
 where
  me nodeId = ("127.0.0.1", show $ 5000 + nodeId)
  them nodeId = [("127.0.0.1", show $ 5000 + id) | id <- [1 .. 3], id /= nodeId]

runAPIServer :: Read tx => Show tx => Handle -> HydraNode tx IO -> IO ()
runAPIServer h node = forever $ do
  threadDelay 100_000
  try @IOException (hGetLine h) >>= \case
    Left ex -> putText $ "[API] error reading: " <> show ex
    Right input -> case readMaybe input of
      Just command -> do
        putText $ show (party $ env node) <> ": received command " <> show command
        handleClientRequest node command
      Nothing -> hPutStrLn h $ "[API] Invalid command: " <> input

openUnixSocket :: FilePath -> IO Handle
openUnixSocket socketPath = do
  void $ try @IOException $ removeFile socketPath
  s <- socket AF_UNIX Stream defaultProtocol
  bind s $ SockAddrUnix socketPath
  listen s 1
  (conn, _peer) <- accept s
  putText "[API] Accepted connection"
  h <- socketToHandle conn ReadWriteMode
  hSetBuffering h LineBuffering
  pure h
