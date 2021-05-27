{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module HydraNode where

import Cardano.Prelude
import Control.Concurrent.Async (
  forConcurrently_,
 )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as Text
import Network.WebSockets (Connection, DataMessage (Binary, Text), receiveDataMessage, runClient, sendClose, sendTextData)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  proc,
  waitForProcess,
  withCreateProcess,
 )
import System.Timeout (timeout)
import Test.Hspec.Expectations (expectationFailure)

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , connection :: Connection
  }

sendRequest :: HydraNode -> Text -> IO ()
sendRequest HydraNode{hydraNodeId, connection} request = do
  putText ("Tester sending to " <> show hydraNodeId <> ": " <> show request)
  sendTextData connection request

data WaitForResponseTimeout = WaitForResponseTimeout {nodeId :: Int, expectedResponse :: Text}
  deriving (Show)

instance Exception WaitForResponseTimeout

failAfter :: HasCallStack => Natural -> IO () -> IO ()
failAfter seconds action =
  timeout (fromIntegral seconds * 1_000_000) action >>= \case
    Just _ -> pure ()
    Nothing -> expectationFailure $ "Timed out after " <> show seconds <> " second(s)"

wait3sForResponse :: HasCallStack => [HydraNode] -> Text -> IO ()
wait3sForResponse nodes expected = do
  forConcurrently_ nodes $ \HydraNode{hydraNodeId, connection} -> do
    -- The chain is slow...
    result <- timeout 3_000_000 $ tryNext connection
    maybe (expectationFailure $ show $ WaitForResponseTimeout hydraNodeId expected) pure result
 where
  tryNext c = do
    msg <-
      receiveDataMessage c >>= \case
        Text b _mt -> pure $ Text.decodeUtf8 $ BSL.toStrict b
        Binary b -> pure $ Text.decodeUtf8 $ BSL.toStrict b
    if msg == expected
      then pure ()
      else tryNext c

withHydraNode :: Int -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId action = do
  withCreateProcess (hydraNodeProcess hydraNodeId) $
    \_stdin _stdout _stderr processHandle ->
      race_ (checkProcessHasNotDied processHandle) tryConnect
 where
  tryConnect = doConnect `catch` \(_ :: IOException) -> tryConnect

  doConnect = runClient "127.0.0.1" (4000 + hydraNodeId) "/" $ \con -> do
    action $ HydraNode hydraNodeId con
    sendClose con ("Bye" :: Text)

data CannotStartHydraNode = CannotStartHydraNode Int deriving (Show)
instance Exception CannotStartHydraNode

hydraNodeProcess :: Int -> CreateProcess
hydraNodeProcess nodeId =
  proc
    "hydra-node"
    $ [ "--node-id"
      , show nodeId
      , "--quiet"
      , "--host"
      , "127.0.0.1"
      , "--port"
      , show (5000 + nodeId)
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
