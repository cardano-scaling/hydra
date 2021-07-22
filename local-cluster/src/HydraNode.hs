{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HydraNode (
  HydraClient (..),
  withHydraNode,
  failAfter,
  send,
  input,
  waitFor,
  waitMatch,
  output,
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
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson (Value (String), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import Data.List (delete)
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import GHC.IO.Handle (hDuplicate)
import Hydra.Network.Ports (randomUnusedTCPPorts)
import Network.HTTP.Conduit (HttpExceptionContent (ConnectionFailure), parseRequest)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Response, getResponseBody, getResponseStatusCode, httpBS)
import Network.WebSockets (Connection, receiveData, runClient, sendClose, sendTextData)
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

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , connection :: Connection
  , nodeStdout :: Handle
  }

failAfter :: HasCallStack => Natural -> IO a -> IO a
failAfter seconds action =
  timeout (fromIntegral seconds * 1_000_000) action >>= \case
    Just a -> pure a
    Nothing -> error $ "Timed out after " <> show seconds <> " second(s)"

-- | Create an input as expected by 'send'.
input :: Text -> [Pair] -> Value
input tag pairs = object $ ("input" .= tag) : pairs

send :: HydraClient -> Value -> IO ()
send HydraClient{hydraNodeId, connection, nodeStdout} v = do
  hPutStrLn nodeStdout ("Tester sending to " <> show hydraNodeId <> ": " <> show v)
  sendTextData connection (Aeson.encode v)

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Value
output tag pairs = object $ ("output" .= tag) : pairs

-- | Wait some time for a single output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Natural -> [HydraClient] -> Value -> IO ()
waitFor delay nodes v = waitForAll delay nodes [v]

waitMatch :: HasCallStack => Natural -> HydraClient -> (Value -> Maybe a) -> IO a
waitMatch delay HydraClient{connection} match = do
  seenMsgs <- newTVarIO []
  timeout (fromIntegral delay * 1_000_000) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      expectationFailure $ "Didn't match within allocated time, received messages: " <> show msgs
      error "should never get there"
 where
  go seenMsgs = do
    bytes <- receiveData connection
    case Aeson.decode' bytes of
      Nothing -> go seenMsgs
      Just msg -> do
        atomically (modifyTVar' seenMsgs (msg :))
        maybe (go seenMsgs) pure (match msg)

-- | Wait some time for a list of outputs from each of given nodes.
-- This function is the generalised version of 'waitFor', allowing several messages
-- to be waited for and received in /any order/.
waitForAll :: HasCallStack => Natural -> [HydraClient] -> [Value] -> IO ()
waitForAll delay nodes expected = do
  forConcurrently_ nodes $ \HydraClient{hydraNodeId, connection} -> do
    msgs <- newIORef []
    -- The chain is slow...
    result <- timeout (fromIntegral delay * 1_000_000) $ tryNext msgs expected connection
    case result of
      Just x -> pure x
      Nothing -> do
        actualMsgs <- readIORef msgs
        expectationFailure $
          toString $
            unlines
              [ "waitFor... timeout!"
              , padRight " " 20 "  nodeId:"
                  <> show hydraNodeId
              , padRight " " 20 "  expected:"
                  <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> expected))
              , padRight " " 20 "  seen messages:"
                  <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> actualMsgs))
              ]
 where
  padRight c n str = T.take n (str <> T.replicate n c)
  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q
  tryNext _ [] _ = pure ()
  tryNext msgs stillExpected c = do
    bytes <- receiveData c
    msg <- case Aeson.decode' bytes of
      Nothing -> fail $ "received non-JSON message from the server: " <> show bytes
      Just m -> pure m
    modifyIORef' msgs (msg :)
    tryNext msgs (delete msg stillExpected) c

getMetrics :: HasCallStack => HydraClient -> IO ByteString
getMetrics HydraClient{hydraNodeId} = do
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

withHydraNode :: forall alg. DSIGNAlgorithm alg => (Int, Int, Int) -> Int -> SignKeyDSIGN alg -> [VerKeyDSIGN alg] -> (HydraClient -> IO ()) -> IO ()
withHydraNode mockChainPorts hydraNodeId sKey vKeys action = do
  withSystemTempFile "hydra-node" $ \f out -> traceOnFailure f $ do
    out' <- hDuplicate out
    withSystemTempDirectory "hydra-node" $ \dir -> do
      let sKeyPath = dir </> (show hydraNodeId <> ".sk")
      BS.writeFile sKeyPath (rawSerialiseSignKeyDSIGN sKey)
      vKeysPaths <- forM (zip [1 ..] vKeys) $ \(i :: Int, vKey) -> do
        let filepath = dir </> (show i <> ".vk")
        filepath <$ BS.writeFile filepath (rawSerialiseVerKeyDSIGN vKey)

      let p =
            (hydraNodeProcess $ defaultArguments hydraNodeId sKeyPath vKeysPaths mockChainPorts)
              { std_out = UseHandle out
              }
      withCreateProcess p $
        \_stdin _stdout _stderr processHandle -> do
          race_ (checkProcessHasNotDied processHandle) (startConnect out')
 where
  startConnect out = do
    connectedOnce <- newIORef False
    tryConnect connectedOnce out

  tryConnect connectedOnce out =
    doConnect connectedOnce out `catch` \(e :: IOException) -> do
      readIORef connectedOnce >>= \case
        False -> tryConnect connectedOnce out
        True -> throwIO e

  doConnect connectedOnce out = runClient "127.0.0.1" (4000 + hydraNodeId) "/" $ \con -> do
    atomicWriteIORef connectedOnce True
    action $ HydraClient hydraNodeId con out
    sendClose con ("Bye" :: Text)

  traceOnFailure f io = do
    io `onException` (readFileText f >>= say)

newtype CannotStartHydraClient = CannotStartHydraClient Int deriving (Show)
instance Exception CannotStartHydraClient

hydraNodeProcess :: [String] -> CreateProcess
hydraNodeProcess = proc "hydra-node"

defaultArguments ::
  Int ->
  FilePath ->
  [FilePath] ->
  (Int, Int, Int) ->
  [String]
defaultArguments nodeId sKey vKeys ports =
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
    <> ["--mock-chain-ports", show ports]

withMockChain :: ((Int, Int, Int) -> IO ()) -> IO ()
withMockChain action = do
  [sync, catchUp, post] <- randomUnusedTCPPorts 3
  withCreateProcess (proc "mock-chain" (arguments sync catchUp post)) $
    \_in _out _err processHandle -> do
      race_ (checkProcessHasNotDied processHandle) (action (sync, catchUp, post))
 where
  arguments s c p =
    [ "--quiet"
    , "--sync-address"
    , "tcp://127.0.0.1:" <> show s
    , "--catch-up-address"
    , "tcp://127.0.0.1:" <> show c
    , "--post-address"
    , "tcp://127.0.0.1:" <> show p
    ]

checkProcessHasNotDied :: ProcessHandle -> IO ()
checkProcessHasNotDied processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> pure ()
    ExitFailure exit -> expectationFailure $ "Process exited with failure code: " <> show exit

-- HACK(SN): These functions here are hard-coded for three nodes, but the tests
-- are somewhat parameterized -> make it all or nothing hard-coded
allNodeIds :: [Int]
allNodeIds = [1 .. 3]

waitForNodesConnected :: HasCallStack => [HydraClient] -> IO ()
waitForNodesConnected = mapM_ waitForNodeConnected

waitForNodeConnected :: HasCallStack => HydraClient -> IO ()
waitForNodeConnected n@HydraClient{hydraNodeId} =
  -- HACK(AB): This is gross, we hijack the node ids and because we know
  -- keys are just integers we can compute them but that's ugly -> use property
  -- party identifiers everywhere
  waitForAll 10 [n] $
    fmap
      ( \party ->
          object
            [ "output" .= String "peerConnected"
            , "peer" .= (party * 10)
            ]
      )
      (filter (/= hydraNodeId) allNodeIds)
