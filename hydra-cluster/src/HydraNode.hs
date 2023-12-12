{-# LANGUAGE DuplicateRecordFields #-}

module HydraNode where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (delete)

import Cardano.BM.Tracing (ToObject)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (IOException)
import Control.Monad.Class.MonadAsync (forConcurrently)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.List qualified as List
import Data.Text (pack)
import Data.Text qualified as T
import Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..), TxOutWithWitness (..))
import Hydra.Cluster.Util (readConfigFile)
import Hydra.ContestationPeriod (ContestationPeriod)
import Hydra.Crypto (HydraKey)
import Hydra.Ledger.Cardano ()
import Hydra.Logging (Tracer, Verbosity (..), traceWith)
import Hydra.Network (Host (Host), NodeId (NodeId))
import Hydra.Network qualified as Network
import Hydra.Options (ChainConfig (..), LedgerConfig (..), OfflineConfig, RunOptions (..), defaultChainConfig, toArgs)
import Network.HTTP.Req (GET (..), HttpException, JsonResponse, NoReqBody (..), POST (..), ReqBodyJson (..), defaultHttpConfig, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import Network.WebSockets (Connection, receiveData, runClient, sendClose, sendTextData)
import System.FilePath ((<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
  CreateProcess (..),
  ProcessHandle,
  StdStream (..),
  proc,
  withCreateProcess,
 )
import Test.Hydra.Prelude (checkProcessHasNotDied, failAfter, failure, withLogFile)
import Prelude qualified

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , connection :: Connection
  , tracer :: Tracer IO HydraNodeLog
  }

-- | Create an input as expected by 'send'.
input :: Text -> [Pair] -> Aeson.Value
input tag pairs = object $ ("tag" .= tag) : pairs

send :: HydraClient -> Aeson.Value -> IO ()
send HydraClient{tracer, hydraNodeId, connection} v = do
  sendTextData connection (Aeson.encode v)
  traceWith tracer $ SentMessage hydraNodeId v

waitNext :: HasCallStack => HydraClient -> IO Aeson.Value
waitNext HydraClient{connection} = do
  bytes <- receiveData connection
  case Aeson.eitherDecode' bytes of
    Left err -> failure $ "WaitNext failed to decode msg: " <> err
    Right value -> pure value

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Aeson.Value
output tag pairs = object $ ("tag" .= tag) : pairs

-- | Wait some time for a single API server output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Tracer IO HydraNodeLog -> DiffTime -> [HydraClient] -> Aeson.Value -> IO ()
waitFor tracer delay nodes v = waitForAll tracer delay nodes [v]

-- | Wait up to some time for an API server output to match the given predicate.
waitMatch :: HasCallStack => DiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay client@HydraClient{tracer, hydraNodeId} match = do
  seenMsgs <- newTVarIO []
  timeout delay (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitMatch did not match a message within " <> show delay
            , padRight ' ' 20 "  nodeId:" <> show hydraNodeId
            , padRight ' ' 20 "  seen messages:"
                <> unlines (align 20 (decodeUtf8 . Aeson.encode <$> msgs))
            ]
 where
  go seenMsgs = do
    msg <- waitNext client
    traceWith tracer (ReceivedMessage hydraNodeId msg)
    atomically (modifyTVar' seenMsgs (msg :))
    maybe (go seenMsgs) pure (match msg)

  align _ [] = []
  align n (h : q) = h : fmap (T.replicate n " " <>) q

-- | Wait up to some `delay` for some JSON `Value` to match given function.
--
-- This is a generalisation of `waitMatch` to multiple nodes.
waitForAllMatch :: (Eq a, Show a, HasCallStack) => DiffTime -> [HydraClient] -> (Aeson.Value -> Maybe a) -> IO a
waitForAllMatch delay nodes match = do
  when (null nodes) $
    failure "no clients to wait for"
  results <- forConcurrently nodes $ \n -> waitMatch delay n match
  case results of
    [] -> failure $ "empty results, but " <> show (length nodes) <> " clients"
    (r : rs) -> do
      unless (all (== r) rs) $
        failure $
          "inconsistent results: " <> show results
      pure r

-- | Wait some time for a list of outputs from each of given nodes.
-- This function is the generalised version of 'waitFor', allowing several messages
-- to be waited for and received in /any order/.
waitForAll :: HasCallStack => Tracer IO HydraNodeLog -> DiffTime -> [HydraClient] -> [Aeson.Value] -> IO ()
waitForAll tracer delay nodes expected = do
  traceWith tracer (StartWaiting (map hydraNodeId nodes) expected)
  forConcurrently_ nodes $ \client@HydraClient{hydraNodeId} -> do
    msgs <- newIORef []
    -- The chain is slow...
    result <- timeout delay $ tryNext client msgs expected
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

  tryNext :: HydraClient -> IORef [Aeson.Value] -> [Aeson.Value] -> IO ()
  tryNext c@HydraClient{hydraNodeId} msgs = \case
    [] -> traceWith tracer (EndWaiting hydraNodeId)
    stillExpected -> do
      msg <- waitNext c
      traceWith tracer (ReceivedMessage hydraNodeId msg)
      modifyIORef' msgs (msg :)
      case msg of
        Object km -> do
          let cleaned = Object $ km & KeyMap.delete "seq" & KeyMap.delete "timestamp"
          tryNext c msgs (List.delete cleaned stillExpected)
        _ ->
          tryNext c msgs stillExpected

-- | Create a commit tx using the hydra-node for later submission
requestCommitTx' :: HydraClient -> UTxO' TxOutWithWitness -> IO Tx
requestCommitTx' HydraClient{hydraNodeId} utxos =
  runReq defaultHttpConfig request <&> commitTx . responseBody
 where
  request =
    Req.req
      POST
      (Req.http "127.0.0.1" /: "commit")
      (ReqBodyJson $ DraftCommitTxRequest utxos)
      (Proxy :: Proxy (JsonResponse DraftCommitTxResponse))
      (Req.port $ 4_000 + hydraNodeId)

-- | Helper to make it easy to obtain a commit tx using a wallet utxo
requestCommitTx :: HydraClient -> UTxO -> IO Tx
requestCommitTx client =
  requestCommitTx' client . fmap (`TxOutWithWitness` Nothing)

getMetrics :: HasCallStack => HydraClient -> IO ByteString
getMetrics HydraClient{hydraNodeId} = do
  failAfter 3 $
    try (runReq defaultHttpConfig request) >>= \case
      Left (e :: HttpException) -> failure $ "Request for hydra-node metrics failed: " <> show e
      Right body -> pure $ Req.responseBody body
 where
  request =
    Req.req
      GET
      (Req.http "127.0.0.1" /: "metrics")
      NoReqBody
      Req.bsResponse
      (Req.port $ 6_000 + hydraNodeId)

data HydraNodeLog
  = NodeStarted {nodeId :: Int}
  | SentMessage {nodeId :: Int, message :: Aeson.Value}
  | StartWaiting {nodeIds :: [Int], messages :: [Aeson.Value]}
  | ReceivedMessage {nodeId :: Int, message :: Aeson.Value}
  | EndWaiting {nodeId :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToObject)

-- XXX: The two lists need to be of same length. Also the verification keys can
-- be derived from the signing keys.
withHydraCluster ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  FilePath ->
  SocketPath ->
  -- | First node id
  -- This sets the starting point for assigning ports
  Int ->
  -- | NOTE: This decides on the size of the cluster!
  [(VerificationKey PaymentKey, SigningKey PaymentKey)] ->
  [SigningKey HydraKey] ->
  -- | Transaction id at which Hydra scripts should have been published.
  TxId ->
  ContestationPeriod ->
  (NonEmpty HydraClient -> IO a) ->
  IO a
withHydraCluster tracer workDir nodeSocket firstNodeId allKeys hydraKeys hydraScriptsTxId =
  withConfiguredHydraCluster tracer workDir nodeSocket firstNodeId allKeys hydraKeys hydraScriptsTxId (const id)

withConfiguredHydraCluster ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  FilePath ->
  SocketPath ->
  -- | First node id
  -- This sets the starting point for assigning ports
  Int ->
  -- | NOTE: This decides on the size of the cluster!
  [(VerificationKey PaymentKey, SigningKey PaymentKey)] ->
  [SigningKey HydraKey] ->
  -- | Transaction id at which Hydra scripts should have been published.
  TxId ->
  -- | Modifies the `ChainConfig` passed to a node upon startup
  (Int -> ChainConfig -> ChainConfig) ->
  ContestationPeriod ->
  (NonEmpty HydraClient -> IO a) ->
  IO a
withConfiguredHydraCluster tracer workDir nodeSocket firstNodeId allKeys hydraKeys hydraScriptsTxId chainConfigDecorator contestationPeriod action = do
  when (clusterSize == 0) $
    failure "Cannot run a cluster with 0 number of nodes"
  when (length allKeys /= length hydraKeys) $
    failure "Not matching number of cardano/hydra keys"

  forM_ (zip allKeys allNodeIds) $ \((vk, sk), ix) -> do
    let vkFile = File $ workDir </> show ix <.> "vk"
    let skFile = File $ workDir </> show ix <.> "sk"
    void $ writeFileTextEnvelope vkFile Nothing vk
    void $ writeFileTextEnvelope skFile Nothing sk
  startNodes [] allNodeIds
 where
  clusterSize = length allKeys
  allNodeIds = [firstNodeId .. firstNodeId + clusterSize - 1]

  startNodes clients = \case
    [] -> action (fromList $ reverse clients)
    (nodeId : rest) -> do
      let hydraSigningKey = hydraKeys Prelude.!! (nodeId - firstNodeId)
          hydraVerificationKeys = map getVerificationKey $ filter (/= hydraSigningKey) hydraKeys
          cardanoSigningKey = workDir </> show nodeId <.> "sk"
          cardanoVerificationKeys = [workDir </> show i <.> "vk" | i <- allNodeIds, i /= nodeId]
          chainConfig =
            chainConfigDecorator nodeId $
              defaultChainConfig
                { nodeSocket
                , cardanoSigningKey
                , cardanoVerificationKeys
                , contestationPeriod
                }
      withHydraNode
        tracer
        chainConfig
        workDir
        nodeId
        hydraSigningKey
        hydraVerificationKeys
        allNodeIds
        hydraScriptsTxId
        (\c -> startNodes (c : clients) rest)

withOfflineHydraNode ::
  Tracer IO HydraNodeLog ->
  OfflineConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  (HydraClient -> IO a) ->
  IO a
withOfflineHydraNode tracer offlineConfig workDir hydraNodeId hydraSKey action =
  withLogFile logFilePath $ \logFileHandle -> do
    withOfflineHydraNode' offlineConfig workDir hydraNodeId hydraSKey (Just logFileHandle) $ do
      \_stdoutHandle _stderrHandle processHandle -> do
        result <-
          race
            (checkProcessHasNotDied ("hydra-node (" <> show hydraNodeId <> ")") processHandle)
            (withConnectionToNode tracer hydraNodeId action)
        case result of
          Left e -> absurd e
          Right a -> pure a
 where
  logFilePath = workDir </> "logs" </> "hydra-node-" <> show hydraNodeId <.> "log"


withOfflineHydraNode' ::
  OfflineConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  -- | If given use this as std out.
  Maybe Handle ->
  -- -> (HydraClient -> IO a)
  (Handle -> Handle -> ProcessHandle -> IO a) ->
  IO a
withOfflineHydraNode' offlineConfig workDir hydraNodeId hydraSKey mGivenStdOut action =
  withSystemTempDirectory "hydra-node" $ \dir -> do
    let cardanoLedgerProtocolParametersFile = dir </> "protocol-parameters.json"
    readConfigFile "protocol-parameters.json" >>= writeFileBS cardanoLedgerProtocolParametersFile
    let hydraSigningKey = dir </> (show hydraNodeId <> ".sk")
    void $ writeFileTextEnvelope (File hydraSigningKey) Nothing hydraSKey
    let ledgerConfig =
          CardanoLedgerConfig
            { cardanoLedgerProtocolParametersFile
            }
    let p =
          -- ( hydraNodeProcess . (\args -> trace ("ARGS DUMP: " <> foldMap (" "<>) (toArgs args)) args) $
          ( hydraNodeProcess $
              RunOptions
                { verbosity = Verbose "HydraNode"
                , nodeId = NodeId $ show hydraNodeId
                , host = "127.0.0.1"
                , -- NOTE(Elaine): port 5000 is used on recent versions of macos
                  port = fromIntegral $ 5_100 + hydraNodeId
                , peers
                , apiHost = "127.0.0.1"
                , apiPort = fromIntegral $ 4_000 + hydraNodeId
                , monitoringPort = Just $ fromIntegral $ 6_000 + hydraNodeId
                , hydraSigningKey
                , hydraVerificationKeys = []
                , hydraScriptsTxId = "9fdc525c20bc00d9dfa9d14904b65e01910c0dfe3bb39865523c1e20eaeb0903"
                , persistenceDir = workDir </> "state-" <> show hydraNodeId
                , chainConfig = defaultChainConfig
                , ledgerConfig
                , offlineConfig = Just offlineConfig
                }
          )
            { std_out = maybe CreatePipe UseHandle mGivenStdOut
            , std_err = CreatePipe
            }
    withCreateProcess p $ \_stdin mCreatedHandle mErr processHandle ->
      case (mCreatedHandle, mGivenStdOut, mErr) of
        (Just out, _, Just err) -> action out err processHandle
        (Nothing, Just out, Just err) -> action out err processHandle
        (_, _, _) -> error "Should not happen™"
 where
  peers = []

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config/.
withHydraNode ::
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  [VerificationKey HydraKey] ->
  [Int] ->
  -- | Transaction id at which Hydra scripts should have been published.
  TxId ->
  (HydraClient -> IO a) ->
  IO a
withHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds hydraScriptsTxId action = do
  withLogFile logFilePath $ \logFileHandle -> do
    withHydraNode' chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds hydraScriptsTxId (Just logFileHandle) $ do
      \_ processHandle -> do
        race
          (checkProcessHasNotDied ("hydra-node (" <> show hydraNodeId <> ")") processHandle)
          (withConnectionToNode tracer hydraNodeId action)
          <&> either absurd id
 where
  logFilePath = workDir </> "logs" </> "hydra-node-" <> show hydraNodeId <.> "log"

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config/.
withHydraNode' ::
  ChainConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  [VerificationKey HydraKey] ->
  [Int] ->
  -- | Transaction id at which Hydra scripts should have been published.
  TxId ->
  -- | If given use this as std out.
  Maybe Handle ->
  (Handle -> ProcessHandle -> IO a) ->
  IO a
withHydraNode' chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds hydraScriptsTxId mGivenStdOut action = do
  withSystemTempDirectory "hydra-node" $ \dir -> do
    let cardanoLedgerProtocolParametersFile = dir </> "protocol-parameters.json"
    readConfigFile "protocol-parameters.json" >>= writeFileBS cardanoLedgerProtocolParametersFile
    let hydraSigningKey = dir </> (show hydraNodeId <> ".sk")
    void $ writeFileTextEnvelope (File hydraSigningKey) Nothing hydraSKey
    hydraVerificationKeys <- forM (zip [1 ..] hydraVKeys) $ \(i :: Int, vKey) -> do
      let filepath = dir </> (show i <> ".vk")
      filepath <$ writeFileTextEnvelope (File filepath) Nothing vKey
    let ledgerConfig =
          CardanoLedgerConfig
            { cardanoLedgerProtocolParametersFile
            }
    let p =
          ( hydraNodeProcess $
              RunOptions
                { verbosity = Verbose "HydraNode"
                , nodeId = NodeId $ show hydraNodeId
                , host = "127.0.0.1"
                , port = fromIntegral $ 5_100 + hydraNodeId
                , peers
                , apiHost = "127.0.0.1"
                , apiPort = fromIntegral $ 4_000 + hydraNodeId
                , monitoringPort = Just $ fromIntegral $ 6_000 + hydraNodeId
                , hydraSigningKey
                , hydraVerificationKeys
                , hydraScriptsTxId
                , persistenceDir = workDir </> "state-" <> show hydraNodeId
                , -- , chainConfig = fromRight defaultChainConfig chainConfig
                  chainConfig
                , ledgerConfig
                , -- , offlineConfig = leftToMaybe chainConfig
                  offlineConfig = Nothing
                }
          )
            { std_out = maybe CreatePipe UseHandle mGivenStdOut
            , std_err = Inherit
            }
    withCreateProcess p $ \_stdin mCreatedHandle mErr processHandle ->
      case (mCreatedHandle, mGivenStdOut, mErr) of
        (Just out, _, _) -> action out processHandle
        (Nothing, Just out, _) -> action out processHandle
        (_, _, _) -> error "Should not happen™"
 where
  peers =
    [ Host
      { Network.hostname = "127.0.0.1"
      , Network.port = fromIntegral $ 5_100 + i
      }
    | i <- allNodeIds
    , i /= hydraNodeId
    ]

withConnectionToNode :: Tracer IO HydraNodeLog -> Int -> (HydraClient -> IO a) -> IO a
withConnectionToNode tracer hydraNodeId action = do
  connectedOnce <- newIORef False
  tryConnect connectedOnce (200 :: Int)
 where
  tryConnect connectedOnce n
    | n == 0 = failure $ "Timed out waiting for connection to hydra-node " <> show hydraNodeId
    | otherwise =
        doConnect connectedOnce `catch` \(e :: IOException) -> do
          readIORef connectedOnce >>= \case
            False -> threadDelay 0.1 >> tryConnect connectedOnce (n - 1)
            True -> throwIO e

  doConnect connectedOnce = runClient "127.0.0.1" (4_000 + hydraNodeId) "/" $ \connection -> do
    atomicWriteIORef connectedOnce True
    traceWith tracer (NodeStarted hydraNodeId)
    res <- action $ HydraClient{hydraNodeId, connection, tracer}
    sendClose connection ("Bye" :: Text)
    pure res

hydraNodeProcess :: RunOptions -> CreateProcess
hydraNodeProcess = proc "hydra-node" . toArgs

waitForNodesConnected :: HasCallStack => Tracer IO HydraNodeLog -> DiffTime -> [HydraClient] -> IO ()
waitForNodesConnected tracer timeOut clients =
  mapM_ waitForNodeConnected clients
 where
  allNodeIds = hydraNodeId <$> clients
  waitForNodeConnected n@HydraClient{hydraNodeId} =
    waitForAll tracer timeOut [n] $
      fmap
        ( \nodeId ->
            object
              [ "tag" .= String "PeerConnected"
              , "peer" .= String (pack $ show nodeId)
              ]
        )
        (filter (/= hydraNodeId) allNodeIds)
