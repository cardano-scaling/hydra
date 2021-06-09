{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HydraNode (
  HydraNode (..),
  withHydraNode,
  failAfter,
  waitForResponse,
  sendRequest,
  getMetrics,
  queryNode,
  defaultArguments,
  withMockChain,
  hydraNodeProcess,
  module System.Process,
) where

import Cardano.Prelude

import Control.Concurrent.Async (
  forConcurrently_,
 )
import qualified Data.ByteString.Lazy as BSL
import Data.IORef (modifyIORef', newIORef, readIORef)
import qualified Data.Text.Encoding as Text
import Network.HTTP.Conduit (HttpExceptionContent (ConnectionFailure), parseRequest)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Response, getResponseBody, getResponseStatusCode, httpBS)
import Network.WebSockets (Connection, DataMessage (Binary, Text), receiveDataMessage, runClient, sendClose, sendTextData)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  proc,
  readCreateProcess,
  waitForProcess,
  withCreateProcess,
 )
import System.Timeout (timeout)
import Test.Hspec.Expectations (expectationFailure)
import Prelude (String, error)

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , connection :: Connection
  , nodeStdout :: Maybe Handle
  }

sendRequest :: HydraNode -> Text -> IO ()
sendRequest HydraNode{hydraNodeId, connection} request = do
  putText ("Tester sending to " <> show hydraNodeId <> ": " <> show request)
  sendTextData connection request

data WaitForResponseTimeout = WaitForResponseTimeout
  { nodeId :: Int
  , expectedResponse :: Text
  , actualMessages :: [Text]
  }
  deriving (Show)

instance Exception WaitForResponseTimeout

failAfter :: HasCallStack => Natural -> IO a -> IO a
failAfter seconds action =
  timeout (fromIntegral seconds * 1_000_000) action >>= \case
    Just a -> pure a
    Nothing -> error $ "Timed out after " <> show seconds <> " second(s)"

waitForResponse :: HasCallStack => Natural -> [HydraNode] -> Text -> IO ()
waitForResponse delay nodes expected = do
  forConcurrently_ nodes $ \HydraNode{hydraNodeId, connection} -> do
    msgs <- newIORef []
    -- The chain is slow...
    result <- timeout (fromIntegral delay * 1_000_000) $ tryNext msgs connection
    case result of
      Just x -> pure x
      Nothing -> do
        actualMsgs <- readIORef msgs
        expectationFailure $ show $ WaitForResponseTimeout hydraNodeId expected actualMsgs
 where
  tryNext msgs c = do
    msg <-
      receiveDataMessage c >>= \case
        Text b _mt -> pure $ Text.decodeUtf8 $ BSL.toStrict b
        Binary b -> pure $ Text.decodeUtf8 $ BSL.toStrict b
    modifyIORef' msgs (msg :)
    if msg == expected
      then pure ()
      else tryNext msgs c

getMetrics :: HasCallStack => HydraNode -> IO ByteString
getMetrics HydraNode{hydraNodeId} = do
  response <-
    failAfter 3 $ queryNode hydraNodeId
  when (getResponseStatusCode response /= 200) $ expectationFailure ("Request for Hydra-node metrics failed :" <> show (getResponseBody response))
  pure $ getResponseBody response

queryNode :: Int -> IO (Response ByteString)
queryNode nodeId =
  parseRequest ("http://127.0.0.1:" <> show (6000 + nodeId) <> "/metrics") >>= loop
 where
  loop req =
    httpBS req
      `catch` (\(HttpExceptionRequest _ (ConnectionFailure _)) -> threadDelay 100_000 >> loop req)

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId action = do
  withCreateProcess (hydraNodeProcess $ defaultArguments hydraNodeId) $
    \_stdin out _stderr processHandle ->
      race_ (checkProcessHasNotDied processHandle) (tryConnect out)
 where
  tryConnect out = doConnect out `catch` \(_ :: IOException) -> tryConnect out

  doConnect out = runClient "127.0.0.1" (4000 + hydraNodeId) "/" $ \con -> do
    action $ HydraNode hydraNodeId con out
    sendClose con ("Bye" :: Text)

data CannotStartHydraNode = CannotStartHydraNode Int deriving (Show)
instance Exception CannotStartHydraNode

hydraNodeProcess :: [String] -> CreateProcess
hydraNodeProcess = proc "hydra-node"

defaultArguments :: Int -> [String]
defaultArguments nodeId =
  [ "--node-id"
  , show nodeId
  , -- , "--quiet"
    "--host"
  , "127.0.0.1"
  , "--port"
  , show (5000 + nodeId)
  , "--api-host"
  , "127.0.0.1"
  , "--api-port"
  , show (4000 + nodeId)
  , "--monitoring-port"
  , show (6000 + nodeId)
  ]
    <> concat [["--peer", "127.0.0.1@" <> show (5000 + id)] | id <- [1 .. 3], id /= nodeId]

withMockChain :: IO () -> IO ()
withMockChain action = do
  withCreateProcess (proc "mock-chain" ["--quiet"]) $
    \_in _out _err processHandle -> do
      race_ (checkProcessHasNotDied processHandle) action

checkProcessHasNotDied :: ProcessHandle -> IO ()
checkProcessHasNotDied processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> pure ()
    ExitFailure exit -> expectationFailure $ "Process exited with failure code: " <> show exit
