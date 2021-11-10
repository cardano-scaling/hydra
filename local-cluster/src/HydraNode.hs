{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HydraNode (
  HydraClient (..),
  withHydraNode,
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
  waitNext,
  withNewClient,
  withHydraCluster,
  EndToEndLog (..),
) where

import Hydra.Prelude hiding (delete)

import Cardano.BM.Tracing (ToObject)
import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (..),
  SignKeyDSIGN (SignKeyMockDSIGN),
  VerKeyDSIGN (VerKeyMockDSIGN),
 )
import CardanoCluster (ClusterLog)
import Control.Concurrent.Async (
  forConcurrently_,
 )
import Control.Exception (IOException)
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson (Value (String), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Text as T
import Hydra.Logging (Tracer, traceWith)
import Network.HTTP.Conduit (HttpExceptionContent (ConnectionFailure), parseRequest)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Response, getResponseBody, getResponseStatusCode, httpBS)
import Network.WebSockets (Connection, receiveData, runClient, sendClose, sendTextData)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
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
import Test.Hydra.Prelude (checkProcessHasNotDied, failAfter, failure, withFile')
import Test.Network.Ports (randomUnusedTCPPorts)

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , connection :: Connection
  , tracer :: Tracer IO EndToEndLog
  }

-- | Create an input as expected by 'send'.
input :: Text -> [Pair] -> Value
input tag pairs = object $ ("tag" .= tag) : pairs

send :: HydraClient -> Value -> IO ()
send HydraClient{connection} v =
  sendTextData connection (Aeson.encode v)

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Value
output tag pairs = object $ ("tag" .= tag) : pairs

-- | Wait some time for a single output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Tracer IO EndToEndLog -> Natural -> [HydraClient] -> Value -> IO ()
waitFor tracer delay nodes v = waitForAll tracer delay nodes [v]

-- TODO(AB): reuse waitNext in other waiters
waitNext :: HasCallStack => HydraClient -> IO Value
waitNext HydraClient{connection} = do
  bytes <- receiveData connection
  case Aeson.eitherDecode' bytes of
    Left err -> failure $ "WaitNext failed to decode msg: " <> err
    Right value -> pure value

waitMatch :: HasCallStack => Natural -> HydraClient -> (Value -> Maybe a) -> IO a
waitMatch delay HydraClient{connection} match = do
  seenMsgs <- newTVarIO []
  timeout (fromIntegral delay * 1_000_000) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        "Didn't match within allocated time. There were some messages " <> show (length msgs) <> " messages received though"
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
waitForAll :: HasCallStack => Tracer IO EndToEndLog -> Natural -> [HydraClient] -> [Value] -> IO ()
waitForAll tracer delay nodes expected = do
  traceWith tracer (StartWaiting (map hydraNodeId nodes) expected)
  forConcurrently_ nodes $ \HydraClient{hydraNodeId, connection} -> do
    msgs <- newIORef []
    -- The chain is slow...
    result <- timeout (fromIntegral delay * 1_000_000) $ tryNext hydraNodeId msgs expected connection
    case result of
      Just x -> pure x
      Nothing -> do
        actualMsgs <- readIORef msgs
        failure $
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

  tryNext nodeId _ [] _ = traceWith tracer (EndWaiting nodeId)
  tryNext nodeId msgs stillExpected c = do
    bytes <- receiveData c
    msg <- case Aeson.decode' bytes of
      Nothing -> fail $ "received non-JSON message from the server: " <> show bytes
      Just m -> pure m
    traceWith tracer (ReceivedMessage nodeId msg)
    modifyIORef' msgs (msg :)
    tryNext nodeId msgs (List.delete msg stillExpected) c

getMetrics :: HasCallStack => HydraClient -> IO ByteString
getMetrics HydraClient{hydraNodeId} = do
  response <-
    failAfter 3 $ queryNode hydraNodeId
  when (getResponseStatusCode response /= 200) $ failure ("Request for Hydra-node metrics failed :" <> show (getResponseBody response))
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

data EndToEndLog
  = NodeStarted Int
  | StartWaiting [Int] [Value]
  | ReceivedMessage Int Value
  | EndWaiting Int
  | FromCluster ClusterLog
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToObject)

withHydraCluster ::
  Tracer IO EndToEndLog ->
  FilePath ->
  FilePath ->
  Word64 ->
  (NonEmpty HydraClient -> IO ()) ->
  IO ()
withHydraCluster tracer workDir nodeSocket clusterSize action =
  case clusterSize of
    0 -> error "Cannot run a cluster with 0 number of nodes"
    n -> go n [] [1 .. n]
 where
  go n clients = \case
    [] -> action (fromList clients)
    (nodeId : rest) ->
      let vKeys = map VerKeyMockDSIGN $ filter (/= nodeId) allNodeIds
          key = SignKeyMockDSIGN nodeId
       in -- FIXME: this code is broken now as we need to pass a singing key for direct chain interaction
          withHydraNode tracer "" workDir nodeSocket (fromIntegral nodeId) key vKeys (map fromIntegral allNodeIds) (\c -> go n (c : clients) rest)
     where
      allNodeIds = [1 .. n]

withHydraNode ::
  forall alg.
  DSIGNAlgorithm alg =>
  Tracer IO EndToEndLog ->
  FilePath ->
  FilePath ->
  FilePath ->
  Int ->
  SignKeyDSIGN alg ->
  [VerKeyDSIGN alg] ->
  [Int] ->
  (HydraClient -> IO ()) ->
  IO ()
withHydraNode tracer walletKey workDir nodeSocket hydraNodeId sKey vKeys allNodeIds action = do
  withFile' (workDir </> show hydraNodeId) $ \out -> do
    withSystemTempDirectory "hydra-node" $ \dir -> do
      let sKeyPath = dir </> (show hydraNodeId <> ".sk")
      BS.writeFile sKeyPath (rawSerialiseSignKeyDSIGN sKey)
      vKeysPaths <- forM (zip [1 ..] vKeys) $ \(i :: Int, vKey) -> do
        let filepath = dir </> (show i <> ".vk")
        filepath <$ BS.writeFile filepath (rawSerialiseVerKeyDSIGN vKey)

      let p =
            (hydraNodeProcess $ defaultArguments hydraNodeId walletKey sKeyPath vKeysPaths nodeSocket allNodeIds)
              { std_out = UseHandle out
              }
      withCreateProcess p $
        \_stdin _stdout _stderr processHandle -> do
          race_
            (checkProcessHasNotDied ("hydra-node (" <> show hydraNodeId <> ")") processHandle)
            (withConnectionToNode tracer hydraNodeId action)

withConnectionToNode :: Tracer IO EndToEndLog -> Int -> (HydraClient -> IO a) -> IO a
withConnectionToNode tracer hydraNodeId action = do
  connectedOnce <- newIORef False
  tryConnect connectedOnce
 where
  tryConnect connectedOnce =
    doConnect connectedOnce `catch` \(e :: IOException) -> do
      readIORef connectedOnce >>= \case
        False -> tryConnect connectedOnce
        True -> throwIO e

  doConnect connectedOnce = runClient "127.0.0.1" (4000 + hydraNodeId) "/" $ \connection -> do
    atomicWriteIORef connectedOnce True
    traceWith tracer (NodeStarted hydraNodeId)
    res <- action $ HydraClient{hydraNodeId, connection, tracer}
    sendClose connection ("Bye" :: Text)
    pure res

-- | Runs an action with a new connection to given Hydra node.
withNewClient :: HydraClient -> (HydraClient -> IO a) -> IO a
withNewClient HydraClient{hydraNodeId, tracer} =
  withConnectionToNode tracer hydraNodeId

newtype CannotStartHydraClient = CannotStartHydraClient Int deriving (Show)
instance Exception CannotStartHydraClient

hydraNodeProcess :: [String] -> CreateProcess
hydraNodeProcess = proc "hydra-node"

defaultArguments ::
  Int ->
  FilePath ->
  FilePath ->
  [FilePath] ->
  FilePath ->
  [Int] ->
  [String]
defaultArguments nodeId walletKey sKey vKeys nodeSocket allNodeIds =
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
    <> concat [["--peer", "127.0.0.1:" <> show (5000 + i)] | i <- allNodeIds, i /= nodeId]
    <> concat [["--party", vKey] | vKey <- vKeys]
    <> ["--network-magic", "42"]
    <> ["--node-socket", nodeSocket]
    <> ["--wallet-key-file", walletKey]

withMockChain :: ((Int, Int, Int) -> IO ()) -> IO ()
withMockChain action = do
  [sync, catchUp, post] <- randomUnusedTCPPorts 3
  withCreateProcess (proc "mock-chain" (arguments sync catchUp post)) $
    \_in _out _err processHandle -> do
      race_ (checkProcessHasNotDied "mock-chain" processHandle) (action (sync, catchUp, post))
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

waitForNodesConnected :: HasCallStack => Tracer IO EndToEndLog -> [Int] -> [HydraClient] -> IO ()
waitForNodesConnected tracer allNodeIds = mapM_ (waitForNodeConnected tracer allNodeIds)

waitForNodeConnected :: HasCallStack => Tracer IO EndToEndLog -> [Int] -> HydraClient -> IO ()
waitForNodeConnected tracer allNodeIds n@HydraClient{hydraNodeId} =
  -- HACK(AB): This is gross, we hijack the node ids and because we know
  -- keys are just integers we can compute them but that's ugly -> use property
  -- party identifiers everywhere
  waitForAll tracer (fromIntegral $ 20 * length allNodeIds) [n] $
    fmap
      ( \party ->
          object
            [ "tag" .= String "PeerConnected"
            , "peer"
                .= object
                  [ "hostname" .= ("127.0.0.1" :: Text)
                  , "port" .= (5000 + party)
                  ]
            ]
      )
      (filter (/= hydraNodeId) allNodeIds)
