{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module HydraNode where

import Cardano.Prelude
import Data.Streaming.Process (terminateProcess)
import Data.String (String)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket, socketToHandle)
import Safe (readEitherSafe)
import System.IO (BufferMode (LineBuffering), hGetLine, hSetBuffering)
import System.Process (
  CreateProcess (..),
  proc,
  withCreateProcess,
 )
import System.Timeout (timeout)

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , inputStream :: Handle
  , outputStream :: Handle
  }

data Request
  = Init [Int]
  deriving (Eq, Show, Read)

data Response
  = ReadyToCommit
  deriving (Eq, Show, Read)

sendRequest :: HydraNode -> Request -> IO ()
sendRequest HydraNode{inputStream} request =
  hPutStrLn inputStream (show @_ @Text request)

wait1sForResponse :: HydraNode -> IO (Either String Response)
wait1sForResponse HydraNode{outputStream} = do
  result <- timeout 1_000_000 $ hGetLine outputStream
  case result of
    Nothing -> pure $ Left "Timed out"
    Just r -> pure $ readEitherSafe r

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId action = do
  withCreateProcess (hydraNodeProcess hydraNodeId) $
    \_stdin _stdout _stderr ph -> do
      bracket (open $ "/tmp/hydra.socket." <> show hydraNodeId) close client
      terminateProcess ph
 where
  open addr =
    bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
      tryConnect sock addr
      return sock

  tryConnect sock addr =
    connect sock (SockAddrUnix addr) `catch` \(e :: IOException) -> do
      threadDelay 100_000
      tryConnect sock addr

  client sock = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    action $ HydraNode hydraNodeId h h

data CannotStartHydraNode = CannotStartHydraNode Int deriving (Show)
instance Exception CannotStartHydraNode

hydraNodeProcess :: Int -> CreateProcess
hydraNodeProcess nodeId = proc "hydra-node" [show nodeId]
