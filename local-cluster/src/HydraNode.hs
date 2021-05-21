{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module HydraNode where

import Cardano.Prelude
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
import Prelude (error)

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , inputStream :: Handle
  , outputStream :: Handle
  }

-- | Deliberately distinct client request type to not take shortcuts.
data Request
  = Init [Int]
  | Commit
  | NewTx Int
  deriving (Eq, Show, Read)

-- | Deliberately distinct client response type to not take shortcuts.
data Response
  = ReadyToCommit
  | HeadIsOpen
  | TxReceived Int
  deriving (Eq, Show, Read)

sendRequest :: HydraNode -> Request -> IO ()
sendRequest HydraNode{inputStream} request =
  hPutStrLn inputStream (show @_ @Text request)

wait3sForResponse :: HasCallStack => HydraNode -> IO (Either String Response)
wait3sForResponse HydraNode{hydraNodeId, outputStream} = do
  -- The chain is slow...
  result <- timeout 3_000_000 $ hGetLine outputStream
  case result of
    Nothing -> error $ "Timed out " <> show hydraNodeId
    Just r -> pure $ readEitherSafe r

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId action = do
  withCreateProcess (hydraNodeProcess hydraNodeId) $
    \_stdin _stdout _stderr _ph -> do
      bracket (open $ "/tmp/hydra.socket." <> show hydraNodeId) close client
 where
  open addr =
    bracketOnError (socket AF_UNIX Stream defaultProtocol) close $ \sock -> do
      tryConnect sock addr
      return sock

  tryConnect sock addr =
    connect sock (SockAddrUnix addr) `catch` \(_ :: IOException) -> do
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

withMockChain :: IO () -> IO ()
withMockChain action = do
  withCreateProcess (proc "mock-chain" []) $
    \_in _out _err _handle -> do
      putText "Mock chain started"
      action
