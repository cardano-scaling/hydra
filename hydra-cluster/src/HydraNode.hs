{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NumericUnderscores #-}
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
  hydraNodeProcess,
  module System.Process,
  waitForNodesConnected,
  waitNext,
  withNewClient,
  withHydraCluster,
  EndToEndLog (..),
) where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (delete)

import Cardano.BM.Tracing (ToObject)
import CardanoNode (NodeLog)
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
import Hydra.Cluster.Util (readConfigFile)
import Hydra.Crypto (deriveVerificationKey, serialiseSigningKeyToRawBytes, serialiseVerificationKeyToRawBytes)
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (Host))
import qualified Hydra.Network as Network
import Hydra.Options (ChainConfig (..), LedgerConfig (..), Options (..), defaultChainConfig, defaultOptions, toArgs)
import Network.HTTP.Conduit (HttpExceptionContent (ConnectionFailure), parseRequest)
import Network.HTTP.Simple (HttpException (HttpExceptionRequest), Response, getResponseBody, getResponseStatusCode, httpBS)
import Network.WebSockets (Connection, receiveData, runClient, sendClose, sendTextData)
import System.FilePath ((<.>), (</>))
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
import Test.Hydra.Prelude (checkProcessHasNotDied, failAfter, failure, withLogFile)
import qualified Prelude

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , connection :: Connection
  , tracer :: Tracer IO EndToEndLog
  }

-- | Create an input as expected by 'send'.
input :: Text -> [Pair] -> Aeson.Value
input tag pairs = object $ ("tag" .= tag) : pairs

send :: HydraClient -> Aeson.Value -> IO ()
send HydraClient{tracer, hydraNodeId, connection} v = do
  sendTextData connection (Aeson.encode v)
  traceWith tracer $ SentMessage hydraNodeId v

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Aeson.Value
output tag pairs = object $ ("tag" .= tag) : pairs

-- | Wait some time for a single output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Tracer IO EndToEndLog -> Natural -> [HydraClient] -> Aeson.Value -> IO ()
waitFor tracer delay nodes v = waitForAll tracer delay nodes [v]

-- TODO(AB): reuse waitNext in other waiters
waitNext :: HasCallStack => HydraClient -> IO Aeson.Value
waitNext HydraClient{connection} = do
  bytes <- receiveData connection
  case Aeson.eitherDecode' bytes of
    Left err -> failure $ "WaitNext failed to decode msg: " <> err
    Right value -> pure value

waitMatch :: HasCallStack => Natural -> HydraClient -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay HydraClient{tracer, hydraNodeId, connection} match = do
  seenMsgs <- newTVarIO []
  timeout (fromIntegral delay * 1_000_000) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitMatch did not match a message within " <> show delay <> "s"
            , padRight ' ' 20 "  nodeId:" <> show hydraNodeId
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> msgs))
            ]
 where
  go seenMsgs = do
    bytes <- receiveData connection
    case Aeson.decode' bytes of
      Nothing -> go seenMsgs
      Just msg -> do
        traceWith tracer (ReceivedMessage hydraNodeId msg)
        atomically (modifyTVar' seenMsgs (msg :))
        maybe (go seenMsgs) pure (match msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

-- | Wait some time for a list of outputs from each of given nodes.
-- This function is the generalised version of 'waitFor', allowing several messages
-- to be waited for and received in /any order/.
waitForAll :: HasCallStack => Tracer IO EndToEndLog -> Natural -> [HydraClient] -> [Aeson.Value] -> IO ()
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
              [ "waitForAll timed out after " <> show delay <> "s"
              , padRight ' ' 20 "  nodeId:"
                  <> show hydraNodeId
              , padRight ' ' 20 "  expected:"
                  <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> expected))
              , padRight ' ' 20 "  seen messages:"
                  <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> actualMsgs))
              ]
 where
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
  | SentMessage Int Aeson.Value
  | StartWaiting [Int] [Aeson.Value]
  | ReceivedMessage Int Aeson.Value
  | EndWaiting Int
  | FromCardanoNode NodeLog
  | StartingFunds {actor :: String, fuelUTxO :: UTxO, otherUTxO :: UTxO}
  | RefueledFunds {actor :: String, refuelingAmount :: Lovelace, fuelUTxO :: UTxO}
  | RemainingFunds {actor :: String, fuelUTxO :: UTxO, otherUTxO :: UTxO}
  deriving (Eq, Show, Generic, ToJSON, FromJSON, ToObject)

-- XXX: The two lists need to be of same length. Also the verification keys can
-- be derived from the signing keys.
withHydraCluster ::
  Tracer IO EndToEndLog ->
  FilePath ->
  FilePath ->
  -- | First node id
  Int ->
  -- | NOTE: This decides on the size of the cluster!
  [(VerificationKey PaymentKey, SigningKey PaymentKey)] ->
  [Hydra.SigningKey] ->
  (NonEmpty HydraClient -> IO ()) ->
  IO ()
withHydraCluster tracer workDir nodeSocket firstNodeId allKeys hydraKeys action = do
  -- We have been bitten by this in the past
  when (clusterSize == 0) $
    error "Cannot run a cluster with 0 number of nodes"

  forM_ (zip allKeys allNodeIds) $ \((vk, sk), ix) -> do
    let vkFile = workDir </> show ix <.> "vk"
    let skFile = workDir </> show ix <.> "sk"
    void $ writeFileTextEnvelope vkFile Nothing vk
    void $ writeFileTextEnvelope skFile Nothing sk
  startNodes [] allNodeIds
 where
  clusterSize = length allKeys
  allNodeIds = [firstNodeId .. firstNodeId + clusterSize - 1]

  startNodes clients = \case
    [] -> action (fromList $ reverse clients)
    (nodeId : rest) -> do
      let hydraSKey = hydraKeys Prelude.!! (nodeId - firstNodeId)
          hydraVKeys = map deriveVerificationKey $ filter (/= hydraSKey) hydraKeys
          cardanoVerificationKeys = [workDir </> show i <.> "vk" | i <- allNodeIds, i /= nodeId]
          cardanoSigningKey = workDir </> show nodeId <.> "sk"
          chainConfig =
            defaultChainConfig
              { nodeSocket
              , cardanoSigningKey
              , cardanoVerificationKeys
              }
      withHydraNode
        tracer
        chainConfig
        workDir
        nodeId
        hydraSKey
        hydraVKeys
        allNodeIds
        (\c -> startNodes (c : clients) rest)

withHydraNode ::
  Tracer IO EndToEndLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  Hydra.SigningKey ->
  [Hydra.VerificationKey] ->
  [Int] ->
  (HydraClient -> IO a) ->
  IO a
withHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds action = do
  withLogFile logFilePath $ \out -> do
    withSystemTempDirectory "hydra-node" $ \dir -> do
      let cardanoLedgerGenesisFile = dir </> "genesis.json"
      readConfigFile "genesis-shelley.json" >>= writeFileBS cardanoLedgerGenesisFile
      let cardanoLedgerProtocolParametersFile = dir </> "protocol-parameters.json"
      readConfigFile "protocol-parameters.json" >>= writeFileBS cardanoLedgerProtocolParametersFile
      let hydraSigningKey = dir </> (show hydraNodeId <> ".sk")
      BS.writeFile hydraSigningKey (serialiseSigningKeyToRawBytes hydraSKey)
      hydraVerificationKeys <- forM (zip [1 ..] hydraVKeys) $ \(i :: Int, vKey) -> do
        let filepath = dir </> (show i <> ".vk")
        filepath <$ BS.writeFile filepath (serialiseVerificationKeyToRawBytes vKey)
      let ledgerConfig =
            CardanoLedgerConfig
              { cardanoLedgerGenesisFile
              , cardanoLedgerProtocolParametersFile
              }
      let p =
            ( hydraNodeProcess $
                defaultOptions
                  { nodeId = fromIntegral hydraNodeId
                  , port = fromIntegral $ 5000 + hydraNodeId
                  , apiPort = fromIntegral $ 4000 + hydraNodeId
                  , monitoringPort = Just $ fromIntegral $ 6000 + hydraNodeId
                  , hydraSigningKey
                  , hydraVerificationKeys
                  , chainConfig
                  , ledgerConfig
                  , peers
                  }
            )
              { std_out = UseHandle out
              }
      withCreateProcess p $
        \_stdin _stdout _stderr processHandle -> do
          result <-
            race
              (checkProcessHasNotDied ("hydra-node (" <> show hydraNodeId <> ")") processHandle)
              (withConnectionToNode tracer hydraNodeId action)
          case result of
            Left err -> absurd err
            Right a -> pure a
 where
  logFilePath = workDir </> "logs" </> "hydra-node-" <> show hydraNodeId <.> "log"

  peers =
    [ Host
      { Network.hostname = "127.0.0.1"
      , Network.port = fromIntegral $ 5000 + i
      }
    | i <- allNodeIds
    , i /= hydraNodeId
    ]

withConnectionToNode :: Tracer IO EndToEndLog -> Int -> (HydraClient -> IO a) -> IO a
withConnectionToNode tracer hydraNodeId action = do
  connectedOnce <- newIORef False
  tryConnect connectedOnce
 where
  tryConnect connectedOnce =
    doConnect connectedOnce `catch` \(e :: IOException) -> do
      readIORef connectedOnce >>= \case
        False -> threadDelay 0.1 >> tryConnect connectedOnce
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

hydraNodeProcess :: Options -> CreateProcess
hydraNodeProcess = proc "hydra-node" . toArgs

waitForNodesConnected :: HasCallStack => Tracer IO EndToEndLog -> [HydraClient] -> IO ()
waitForNodesConnected tracer clients =
  mapM_ waitForNodeConnected clients
 where
  allNodeIds = hydraNodeId <$> clients
  waitForNodeConnected n@HydraClient{hydraNodeId} =
    waitForAll tracer (fromIntegral $ 20 * length allNodeIds) [n] $
      fmap
        ( \nodeId ->
            object
              [ "tag" .= String "PeerConnected"
              , "peer"
                  .= object
                    [ "hostname" .= ("127.0.0.1" :: Text)
                    , "port" .= (5000 + nodeId)
                    ]
              ]
        )
        (filter (/= hydraNodeId) allNodeIds)
