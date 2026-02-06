{-# LANGUAGE DuplicateRecordFields #-}

module HydraNode where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (STM, delete)

import CardanoNode (cliQueryProtocolParameters)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO)
import Control.Exception (Handler (..), IOException, catches)
import Control.Lens ((?~), (^?))
import Control.Monad.Class.MonadAsync (forConcurrently)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key)
import Data.Aeson.Types (Pair)
import Data.ByteString (hGetContents)
import Data.List qualified as List
import Data.Text qualified as T
import Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..))
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Cluster.Util (readConfigFile)
import Hydra.HeadLogic.State (SeenSnapshot)
import Hydra.Logging (Tracer, Verbosity (..), traceWith)
import Hydra.Network (Host (Host), NodeId (NodeId), WhichEtcd (EmbeddedEtcd))
import Hydra.Network qualified as Network
import Hydra.Options (BlockfrostOptions (..), CardanoChainConfig (..), ChainBackendOptions (..), ChainConfig (..), DirectOptions (..), LedgerConfig (..), RunOptions (..), defaultBFQueryTimeout, defaultCardanoChainConfig, defaultDirectOptions, nodeSocket, toArgs)
import Hydra.Tx (ConfirmedSnapshot)
import Hydra.Tx.ContestationPeriod (ContestationPeriod)
import Hydra.Tx.Crypto (HydraKey)
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Req (GET (..), HttpException, JsonResponse, NoReqBody (..), POST (..), ReqBodyJson (..), defaultHttpConfig, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLbs, setRequestBodyJSON)
import Network.WebSockets (Connection, ConnectionException, HandshakeException, receiveData, runClient, sendClose, sendTextData)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((<.>), (</>))
import System.Info (os)
import System.Process.Typed (
  ExitCode (..),
  createPipe,
  getStderr,
  proc,
  setStderr,
  setStdout,
  useHandleOpen,
  waitExitCode,
  withProcessTerm,
 )
import Test.Hydra.Prelude (HydraBackend (..), failAfter, failure, getHydraBackend, shouldNotBe, withLogFile)
import Prelude qualified

-- * Client to interact with a hydra-node

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , apiHost :: Host
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
  -- NOTE: We delay on connection errors to give other assertions the chance to
  -- provide more detail (e.g. checkProcessHasNotDied) before this fails.
  bytes <-
    try (receiveData connection) >>= \case
      Left (err :: ConnectionException) -> do
        threadDelay 1
        failure $ "waitNext: " <> show err
      Right msg -> pure msg
  case Aeson.eitherDecode' bytes of
    Left err -> failure $ "WaitNext failed to decode msg: " <> err
    Right value -> pure value

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Aeson.Value
output tag pairs = object $ ("tag" .= tag) : pairs

setupBFDelay :: NominalDiffTime -> IO NominalDiffTime
setupBFDelay d = do
  getHydraBackend >>= \case
    BlockfrostBackendType -> pure $ d * fromIntegral defaultBFQueryTimeout
    _backend -> pure d

-- | Wait some time for a single API server output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Tracer IO HydraNodeLog -> NominalDiffTime -> [HydraClient] -> Aeson.Value -> IO ()
waitFor tracer delay nodes v = waitForAll tracer delay nodes [v]

-- | Wait up to some time and succeed if no API server output matches the given predicate.
waitNoMatch :: HasCallStack => NominalDiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO ()
waitNoMatch delay client match = do
  result <- try (void $ waitMatch delay client match) :: IO (Either SomeException ())
  case result of
    Left _ -> pure () -- Success: waitMatch failed to find a match
    Right _ -> failure "waitNoMatch: A match was found when none was expected"

-- | Wait up to some time for an API server output to match the given predicate.
waitMatch :: HasCallStack => NominalDiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay' client@HydraClient{tracer, hydraNodeId} match = do
  seenMsgs <- newLabelledTVarIO "wait-match-seen-msgs" []
  delay <- setupBFDelay delay'
  timeout (realToFrac delay) (go seenMsgs) >>= \case
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
waitForAllMatch :: (Eq a, Show a, HasCallStack) => NominalDiffTime -> [HydraClient] -> (Aeson.Value -> Maybe a) -> IO a
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
waitForAll :: HasCallStack => Tracer IO HydraNodeLog -> NominalDiffTime -> [HydraClient] -> [Aeson.Value] -> IO ()
waitForAll tracer d nodes expected = do
  traceWith tracer (StartWaiting (map hydraNodeId nodes) expected)
  delay <- setupBFDelay d
  forConcurrently_ nodes $ \client@HydraClient{hydraNodeId} -> do
    msgs <- newIORef []
    result <- timeout (realToFrac delay) $ tryNext client msgs expected
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

-- | Helper to make it easy to obtain a commit tx using some wallet utxo
requestCommitTx :: HydraClient -> UTxO -> IO Tx
requestCommitTx HydraClient{apiHost = Host{hostname, port}} utxo =
  runReq defaultHttpConfig request <&> commitTx . responseBody
 where
  request =
    Req.req
      POST
      (Req.http hostname /: "commit")
      (ReqBodyJson $ SimpleCommitRequest @Tx utxo)
      (Proxy :: Proxy (JsonResponse (DraftCommitTxResponse Tx)))
      (Req.port (fromInteger . toInteger $ port))

-- | Submit a decommit transaction to the hydra-node.
postDecommit :: HydraClient -> Tx -> IO ()
postDecommit HydraClient{apiHost = Host{hostname, port}} decommitTx = do
  void $
    parseUrlThrow ("POST http://" <> T.unpack hostname <> ":" <> show port <> "/decommit")
      <&> setRequestBodyJSON decommitTx
        >>= httpLbs

-- | Get the protocol-parameters from the hydra-node.
getProtocolParameters :: HydraClient -> IO (PParams LedgerEra)
getProtocolParameters HydraClient{apiHost = Host{hostname, port}} =
  parseUrlThrow ("GET http://" <> T.unpack hostname <> ":" <> show port <> "/protocol-parameters")
    >>= httpJSON
    <&> getResponseBody

-- | Get the latest snapshot UTxO from the hydra-node. NOTE: While we usually
-- avoid parsing responses using the same data types as the system under test,
-- this parses the response as a 'UTxO' type as we often need to pick it apart.
getSnapshotUTxO :: HydraClient -> IO UTxO
getSnapshotUTxO HydraClient{apiHost = Host{hostname, port}} =
  parseUrlThrow ("GET http://" <> T.unpack hostname <> ":" <> show port <> "/snapshot/utxo")
    >>= httpJSON
    <&> getResponseBody

-- | Get the latest snapshot from the hydra-node. NOTE: While we usually
-- avoid parsing responses using the same data types as the system under test,
-- this parses the response as a 'ConfirmedSnapshot' type as we often need to pick it apart.
getSnapshotConfirmed :: HydraClient -> IO (ConfirmedSnapshot Tx)
getSnapshotConfirmed HydraClient{apiHost = Host{hostname, port}} =
  runReq defaultHttpConfig request <&> responseBody
 where
  request =
    Req.req
      GET
      (Req.http hostname /: "snapshot")
      NoReqBody
      (Proxy :: Proxy (JsonResponse (ConfirmedSnapshot Tx)))
      (Req.port (fromInteger . toInteger $ port))

-- | Get the latest seen snapshot from the hydra-node.
getSnapshotLastSeen :: HydraClient -> IO (SeenSnapshot Tx)
getSnapshotLastSeen HydraClient{apiHost = Host{hostname, port}} =
  runReq defaultHttpConfig request <&> responseBody
 where
  request =
    Req.req
      GET
      (Req.http hostname /: "snapshot" /: "last-seen")
      NoReqBody
      (Proxy :: Proxy (JsonResponse (SeenSnapshot Tx)))
      (Req.port (fromInteger . toInteger $ port))

getMetrics :: HasCallStack => HydraClient -> IO ByteString
getMetrics HydraClient{hydraNodeId, apiHost = Host{hostname}} = do
  failAfter 3 $
    try (runReq defaultHttpConfig request) >>= \case
      Left (e :: HttpException) -> failure $ "Request for hydra-node metrics failed: " <> show e
      Right body -> pure $ Req.responseBody body
 where
  request =
    Req.req
      GET
      (Req.http hostname /: "metrics")
      NoReqBody
      Req.bsResponse
      (Req.port $ 6_000 + hydraNodeId)

-- * Start / connect to a cluster of nodes

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
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  ContestationPeriod ->
  (NonEmpty HydraClient -> IO a) ->
  IO a
withHydraCluster tracer workDir nodeSocket firstNodeId allKeys hydraKeys hydraScriptsTxId contestationPeriod action = do
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
            Cardano
              defaultCardanoChainConfig
                { hydraScriptsTxId
                , cardanoSigningKey
                , cardanoVerificationKeys
                , contestationPeriod
                , chainBackendOptions =
                    Direct
                      defaultDirectOptions
                        { nodeSocket = nodeSocket
                        }
                }
      withHydraNode
        tracer
        chainConfig
        workDir
        nodeId
        hydraSigningKey
        hydraVerificationKeys
        allNodeIds
        (\c -> startNodes (c : clients) rest)

-- * Start / connect to a hydra-node

-- | Prepare protocol-parameters to run a hydra-node with given 'ChainConfig' and using the config from
-- config/.
preparePParams ::
  ChainConfig ->
  FilePath ->
  (Aeson.Value -> Aeson.Value) ->
  IO FilePath
preparePParams chainConfig stateDir paramsDecorator = do
  let cardanoLedgerProtocolParametersFile = stateDir </> "protocol-parameters.json"
  case chainConfig of
    Offline _ ->
      readConfigFile "protocol-parameters.json"
        >>= writeFileBS cardanoLedgerProtocolParametersFile
    Cardano CardanoChainConfig{chainBackendOptions} -> do
      protocolParameters <- case chainBackendOptions of
        Direct DirectOptions{networkId, nodeSocket} ->
          -- NOTE: This implicitly tests of cardano-cli with hydra-node
          cliQueryProtocolParameters nodeSocket networkId
        Blockfrost BlockfrostOptions{projectPath} -> do
          prj <- Blockfrost.projectFromFile projectPath
          toJSON <$> Blockfrost.runBlockfrostM prj Blockfrost.queryProtocolParameters
      Aeson.encodeFile cardanoLedgerProtocolParametersFile $
        protocolParameters
          & atKey "txFeeFixed" ?~ toJSON (Number 0)
          & atKey "txFeePerByte" ?~ toJSON (Number 0)
          & key "executionUnitPrices" . atKey "priceMemory" ?~ toJSON (Number 0)
          & key "executionUnitPrices" . atKey "priceSteps" ?~ toJSON (Number 0)
          & atKey "utxoCostPerByte" ?~ toJSON (Number 0)
          & atKey "treasuryCut" ?~ toJSON (Number 0)
          & atKey "minFeeRefScriptCostPerByte" ?~ toJSON (Number 0)
          & paramsDecorator
  pure cardanoLedgerProtocolParametersFile

-- | Prepare 'RunOptions' to run a hydra-node with given 'ChainConfig' and using the config from
-- config/.
prepareHydraNode ::
  HasCallStack =>
  ChainConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  [VerificationKey HydraKey] ->
  [Int] ->
  (Aeson.Value -> Aeson.Value) ->
  IO RunOptions
prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds paramsDecorator = do
  -- NOTE: AirPlay on MacOS uses 5000 and we must avoid it.
  when (os == "darwin") $ port `shouldNotBe` (5_000 :: Network.PortNumber)
  let stateDir = workDir </> "state-" <> show hydraNodeId
  createDirectoryIfMissing True stateDir
  cardanoLedgerProtocolParametersFile <- preparePParams chainConfig stateDir paramsDecorator
  let hydraSigningKey = stateDir </> "me.sk"
  void $ writeFileTextEnvelope (File hydraSigningKey) Nothing hydraSKey
  hydraVerificationKeys <- forM (zip [1 ..] hydraVKeys) $ \(i :: Int, vKey) -> do
    let filepath = stateDir </> ("other-" <> show i <> ".vk")
    filepath <$ writeFileTextEnvelope (File filepath) Nothing vKey
  pure $
    RunOptions
      { verbosity = Verbose "HydraNode"
      , nodeId = NodeId $ show hydraNodeId
      , listen = Host "0.0.0.0" (fromIntegral $ 5_000 + hydraNodeId)
      , advertise = Nothing
      , peers
      , apiHost = "0.0.0.0"
      , apiPort = fromIntegral $ 4_000 + hydraNodeId
      , tlsCertPath = Nothing
      , tlsKeyPath = Nothing
      , monitoringPort = Just $ fromIntegral $ 6_000 + hydraNodeId
      , hydraSigningKey
      , hydraVerificationKeys
      , persistenceDir = stateDir
      , persistenceRotateAfter = Nothing
      , chainConfig
      , whichEtcd = EmbeddedEtcd
      , ledgerConfig =
          CardanoLedgerConfig
            { cardanoLedgerProtocolParametersFile
            }
      , apiTransactionTimeout = 100000
      }
 where
  port = fromIntegral $ 5_000 + hydraNodeId
  -- NOTE: See comment above about 0.0.0.0 vs 127.0.0.1
  peers =
    [ Host
      { Network.hostname = "0.0.0.0"
      , Network.port = fromIntegral $ 5_000 + i
      }
    | i <- allNodeIds
    , i /= hydraNodeId
    ]

-- | Run a hydra-node with given 'RunOptions' and in sync with chain backend.
withPreparedHydraNodeInSync ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraClient -> IO a) ->
  IO a
withPreparedHydraNodeInSync tracer chainConfig workDir hydraNodeId runOptions action =
  withPreparedHydraNode tracer workDir hydraNodeId runOptions action'
 where
  waitFactor = 5
  blockTime = case chainConfig of
    Offline _ -> 1
    Cardano CardanoChainConfig{chainBackendOptions} -> case chainBackendOptions of
      Direct DirectOptions{networkId} -> case networkId of
        Mainnet -> 60
        Testnet (NetworkMagic 42) -> 1
        Testnet _ -> 20
      Blockfrost _ -> 60
  waitTime = blockTime * waitFactor
  action' client = do
    waitForNodesSynced tracer waitTime $ client :| []
    action client

-- | Run a hydra-node with given 'RunOptions'.
withPreparedHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraClient -> IO a) ->
  IO a
withPreparedHydraNode tracer workDir hydraNodeId runOptions action =
  withLogFile logFilePath $ \logFileHandle -> do
    let cmd =
          (proc "hydra-node" . toArgs $ runOptions)
            & setStdout (useHandleOpen logFileHandle)
            & setStderr createPipe

    traceWith tracer $ HydraNodeCommandSpec $ show cmd

    withProcessTerm cmd $ \p -> do
      -- NOTE: exit code thread gets cancelled if 'action' terminates first
      raceLabelled
        ("collect-check-process-exit-code", collectAndCheckExitCode p)
        ("with-connection-to-node", withConnectionToNode tracer hydraNodeId action)
        <&> either absurd id
 where
  collectAndCheckExitCode p = do
    let h = getStderr p
    waitExitCode p >>= \case
      ExitSuccess -> failure "hydra-node stopped early"
      ExitFailure ec -> do
        err <- hGetContents h
        failure . toString $
          unlines
            [ "hydra-node (nodeId = " <> show hydraNodeId <> ") exited with failure code: " <> show ec
            , decodeUtf8 err
            ]

  logFilePath = workDir </> "logs" </> "hydra-node-" <> show hydraNodeId <.> "log"

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config/.
withHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  [VerificationKey HydraKey] ->
  [Int] ->
  (HydraClient -> IO a) ->
  IO a
withHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds action = do
  opts <- prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds id
  withPreparedHydraNodeInSync tracer chainConfig workDir hydraNodeId opts action

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config and catching up with chain backend/.
withHydraNodeCatchingUp ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  SigningKey HydraKey ->
  [VerificationKey HydraKey] ->
  [Int] ->
  (HydraClient -> IO a) ->
  IO a
withHydraNodeCatchingUp tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds action = do
  opts <- prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys allNodeIds id
  withPreparedHydraNode tracer workDir hydraNodeId opts action

withConnectionToNode :: forall a. Tracer IO HydraNodeLog -> Int -> (HydraClient -> IO a) -> IO a
withConnectionToNode tracer hydraNodeId =
  withConnectionToNodeHost tracer hydraNodeId Host{hostname, port} (Just "/?history=yes")
 where
  hostname = "127.0.0.1"
  port = fromInteger $ 4_000 + toInteger hydraNodeId

withConnectionToNodeHost :: forall a. Tracer IO HydraNodeLog -> Int -> Host -> Maybe String -> (HydraClient -> IO a) -> IO a
withConnectionToNodeHost tracer hydraNodeId apiHost@Host{hostname, port} mQueryParams action = do
  connectedOnce <- newIORef False
  (retries, delay) <-
    getHydraBackend >>= \case
      DirectBackendType -> pure (200, 0.1)
      BlockfrostBackendType -> pure (300, 1)
  tryConnect connectedOnce (retries :: Int) delay
 where
  tryConnect connectedOnce n delay
    | n == 0 = failure $ "Timed out waiting for connection to hydra-node " <> show hydraNodeId
    | otherwise = do
        let
          retryOrThrow :: forall proxy e. Exception e => proxy e -> e -> IO a
          retryOrThrow _ e =
            readIORef connectedOnce >>= \case
              False -> threadDelay delay >> tryConnect connectedOnce (n - 1) delay
              True -> throwIO e
        doConnect connectedOnce
          `catches` [ Handler $ retryOrThrow (Proxy @IOException)
                    , Handler $ retryOrThrow (Proxy @HandshakeException)
                    ]

  queryParams = fromMaybe "/" mQueryParams

  doConnect connectedOnce = runClient (T.unpack hostname) (fromInteger . toInteger $ port) queryParams $
    \connection -> do
      atomicWriteIORef connectedOnce True
      traceWith tracer (NodeStarted hydraNodeId)
      res <- action $ HydraClient{hydraNodeId, apiHost, connection, tracer}
      sendClose connection ("Bye" :: Text)
      pure res

waitForNodesConnected :: Tracer IO HydraNodeLog -> NominalDiffTime -> NonEmpty HydraClient -> IO ()
waitForNodesConnected tracer delay clients =
  waitFor tracer delay (toList clients) $
    output "NetworkConnected" []

waitForNodesDisconnected :: Tracer IO HydraNodeLog -> NominalDiffTime -> NonEmpty HydraClient -> IO ()
waitForNodesDisconnected tracer delay clients =
  waitFor tracer delay (toList clients) $
    output "NetworkDisconnected" []

waitForNodesSynced :: NominalDiffTime -> NonEmpty HydraClient -> IO ()
waitForNodesSynced delay clients = do
  waitForAllMatch delay (toList clients) $ \v -> do
    guard $ v ^? key "tag" == Just "NodeSynced"

data HydraNodeLog
  = HydraNodeCommandSpec {cmd :: Text}
  | NodeStarted {nodeId :: Int}
  | SentMessage {nodeId :: Int, message :: Aeson.Value}
  | StartWaiting {nodeIds :: [Int], messages :: [Aeson.Value]}
  | ReceivedMessage {nodeId :: Int, message :: Aeson.Value}
  | EndWaiting {nodeId :: Int}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
