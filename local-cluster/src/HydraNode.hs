{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

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
  waitForNodesConnected,
) where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Control.Concurrent.Async (
  forConcurrently_,
 )
import Control.Exception (IOException)
import qualified Data.ByteString as BS
import Data.Text.IO (hPutStrLn)
import GHC.IO.Handle (hDuplicate)
import Network.HTTP.Conduit (HttpExceptionContent (ConnectionFailure), parseRequest)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Response, getResponseBody, getResponseStatusCode, httpBS)
import Network.WebSockets (Connection, DataMessage (Binary, Text), receiveDataMessage, runClient, sendClose, sendTextData)
import Say (say)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
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

data HydraNode = HydraNode
  { hydraNodeId :: Int
  , connection :: Connection
  , nodeStdout :: Handle
  }

sendRequest :: HydraNode -> Text -> IO ()
sendRequest HydraNode{hydraNodeId, connection, nodeStdout} request = do
  hPutStrLn nodeStdout ("Tester sending to " <> show hydraNodeId <> ": " <> show request)
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
        Text b _mt -> pure $ decodeUtf8 b
        Binary b -> pure $ decodeUtf8 b
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
    httpBS req `catch` onConnectionFailure (loop req)

  onConnectionFailure cont = \case
    (HttpExceptionRequest _ (ConnectionFailure _)) -> threadDelay 100_000 >> cont
    e -> throwIO e

withHydraNode :: forall alg. DSIGNAlgorithm alg => Int -> SignKeyDSIGN alg -> [VerKeyDSIGN alg] -> (HydraNode -> IO ()) -> IO ()
withHydraNode hydraNodeId sKey vKeys action = do
  withSystemTempFile "hydra-node" $ \f out -> traceOnFailure f $ do
    out' <- hDuplicate out
    withSystemTempDirectory "hydra-node" $ \dir -> do
      let sKeyPath = dir </> (show hydraNodeId <> ".sk")
      BS.writeFile sKeyPath (rawSerialiseSignKeyDSIGN sKey)
      vKeysPaths <- forM (zip [1 ..] vKeys) $ \(i :: Int, vKey) -> do
        let filepath = dir </> (show i <> ".vk")
        filepath <$ BS.writeFile filepath (rawSerialiseVerKeyDSIGN vKey)

      let p =
            (hydraNodeProcess $ defaultArguments hydraNodeId sKeyPath vKeysPaths)
              { std_out = UseHandle out
              }
      withCreateProcess p $
        \_stdin _stdout _stderr processHandle -> do
          race_ (checkProcessHasNotDied processHandle) (tryConnect out')
 where
  tryConnect out = doConnect out `catch` \(_ :: IOException) -> tryConnect out

  doConnect out = runClient "127.0.0.1" (4000 + hydraNodeId) "/" $ \con -> do
    action $ HydraNode hydraNodeId con out
    sendClose con ("Bye" :: Text)

  traceOnFailure f io = do
    io `onException` (readFileText f >>= say)

newtype CannotStartHydraNode = CannotStartHydraNode Int deriving (Show)
instance Exception CannotStartHydraNode

hydraNodeProcess :: [String] -> CreateProcess
hydraNodeProcess = proc "hydra-node"

defaultArguments ::
  Int ->
  FilePath ->
  [FilePath] ->
  [String]
defaultArguments nodeId sKey vKeys =
  [ "--node-id"
  , show nodeId
  , "--host"
  , "127.0.0.1"
  , "--port"
  , show (5000 + nodeId)
  , "--api-host"
  , "127.0.0.1"
  , "--api-port"
  , show (4000 + nodeId)
  , "--monitoring-port"
  , show (6000 + nodeId)
  , "--me"
  , sKey
  ]
    <> concat [["--peer", "127.0.0.1@" <> show (5000 + i)] | i <- allNodeIds, i /= nodeId]
    <> concat [["--party", vKey] | vKey <- vKeys]

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

-- HACK(SN): These functions here are hard-coded for three nodes, but the tests
-- are somewhat parameterized -> make it all or nothing hard-coded
allNodeIds :: [Int]
allNodeIds = [1 .. 3]

waitForNodesConnected :: [HydraNode] -> IO ()
waitForNodesConnected = mapM_ waitForOtherNodesConnected

waitForOtherNodesConnected :: HydraNode -> IO ()
waitForOtherNodesConnected n@HydraNode{hydraNodeId} =
  forM_ otherNodeIds $ \otherParty ->
    -- HACK(AB): This is gross, we hijack the node ids and because we know
    -- keys are just integers we can compute them but that's ugly -> use property
    -- party identifiers everywhere
    waitForResponse 10 [n] $ "PeerConnected (VerKeyMockDSIGN " <> show (otherParty * 10) <> ")"
 where
  otherNodeIds = filter (/= hydraNodeId) allNodeIds
