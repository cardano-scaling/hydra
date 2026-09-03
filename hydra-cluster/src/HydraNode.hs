{-# LANGUAGE DuplicateRecordFields #-}

module HydraNode (
  module HydraNode,
  HydraNodeLog (..),
) where

import Hydra.Cardano.Api hiding (getVerificationKey)
import Hydra.Prelude hiding (STM, delete)

import Cardano.Binary (serialize')
import CardanoNode (HydraNodeLog (..), cliQueryProtocolParameters)
import Control.Concurrent.Async (forConcurrently_)
import Control.Concurrent.Class.MonadSTM (modifyTVar', readTVarIO)
import Control.Exception (Handler (..), IOException, catches)
import Control.Lens ((?~), (^?))
import Control.Monad.Class.MonadAsync (forConcurrently)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Lens (atKey, key, _String)
import Data.Aeson.Types (Pair)
import Data.ByteString (hGetContents)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Secret (Secret, withSecret)
import Data.Text qualified as T
import Hydra.API.ClientInput (ClientInput)
import Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..))
import Hydra.API.ServerOutput (ApiEncoding (..), ApiMessage)
import Hydra.API.WireFormat (decodeWire)
import Hydra.Chain.Blockfrost.Client qualified as Blockfrost
import Hydra.Cluster.Util (Timing (..), readConfigFile)
import Hydra.Logging (Tracer, Verbosity (..), traceWith)
import Hydra.Network (Host (Host), NodeId (NodeId), WhichEtcd (SystemEtcd))
import Hydra.Network qualified as Network
import Hydra.Network.Etcd (peerPortToClientPort)
import Hydra.Options (BlockfrostOptions (..), CardanoChainConfig (..), ChainBackendOptions (..), ChainConfig (..), DirectOptions (..), LedgerConfig (..), RunOptions (..), defaultCardanoChainConfig, defaultDirectOptions, nodeSocket, toArgs)
import Hydra.Tx (ConfirmedSnapshot)
import Hydra.Tx.Crypto (HydraKey, getVerificationKey)
import Network.HTTP.Conduit (parseUrlThrow)
import Network.HTTP.Req (GET (..), HttpException, JsonResponse, NoReqBody (..), POST (..), ReqBodyJson (..), defaultHttpConfig, responseBody, runReq, (/:))
import Network.HTTP.Req qualified as Req
import Network.HTTP.Simple (getResponseBody, httpJSON, httpLbs, setRequestBodyJSON)
import Network.WebSockets (Connection, ConnectionException, HandshakeException, receiveData, runClient, sendBinaryData, sendClose, sendTextData)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)
import System.Process.Typed (
  ExitCode (..),
  createPipe,
  getStderr,
  proc,
  setCloseFds,
  setEnv,
  setStderr,
  setStdout,
  useHandleOpen,
  waitExitCode,
  withProcessTerm,
 )
import Test.Hydra.Prelude (failure, shouldBe)
import Test.Hydra.Prelude qualified as Prelude
import Test.Network.Ports (randomUnusedTCPPorts, randomUnusedTCPPortsWithDerived)
import Prelude qualified

-- * Client to interact with a hydra-node

data HydraClient = HydraClient
  { hydraNodeId :: Int
  , apiHost :: Host
  , monitoringPort :: Maybe Network.PortNumber
  -- ^ Port the hydra-node exposes Prometheus metrics on, if enabled.
  , connection :: Connection
  , tracer :: Tracer IO HydraNodeLog
  , apiEncoding :: ApiEncoding
  -- ^ Which wire encoding was negotiated for 'connection' (via the
  -- @encoding=cbor@ query param). 'send' and 'waitNext' translate between
  -- CBOR on the wire and the 'Aeson.Value's used by test assertions.
  , workDir :: Maybe FilePath
  -- ^ Work directory of the spawned hydra-node, when this test spawned it;
  -- used to point at its logs in failure messages.
  }

-- | Create an input as expected by 'send'.
input :: Text -> [Pair] -> Aeson.Value
input tag pairs = object $ ("tag" .= tag) : pairs

send :: HydraClient -> Aeson.Value -> IO ()
send HydraClient{tracer, hydraNodeId, connection, apiEncoding} v = do
  case apiEncoding of
    JsonEncoding -> sendTextData connection (Aeson.encode v)
    CborEncoding ->
      -- Convert the 'Aeson.Value' to a typed 'ClientInput' and send its CBOR.
      case Aeson.fromJSON v of
        Aeson.Error err -> failure $ "send: cannot convert to ClientInput for CBOR encoding: " <> err
        Aeson.Success (clientInput :: ClientInput Tx) ->
          sendBinaryData connection (serialize' clientInput)
  traceWith tracer $ SentMessage hydraNodeId v

waitNext :: HasCallStack => HydraClient -> IO Aeson.Value
waitNext HydraClient{connection, apiEncoding} = do
  -- NOTE: We delay on connection errors to give other assertions the chance to
  -- provide more detail (e.g. checkProcessHasNotDied) before this fails.
  bytes <-
    try (receiveData connection) >>= \case
      Left (err :: ConnectionException) -> do
        threadDelay 1
        failure $ "waitNext: " <> show err
      Right msg -> pure msg
  case apiEncoding of
    JsonEncoding ->
      case Aeson.eitherDecode' bytes of
        Left err -> failure $ "WaitNext failed to decode msg: " <> err
        Right value -> pure value
    CborEncoding ->
      -- Decode the typed message and re-encode as a JSON 'Aeson.Value', so
      -- that existing lens-based assertions keep working unchanged.
      case decodeWire CborEncoding bytes of
        Left err -> failure $ "WaitNext failed to decode CBOR msg: " <> err
        Right (msg :: ApiMessage Tx) -> pure $ toJSON msg

-- | Create an output as expected by 'waitFor' and 'waitForAll'.
output :: Text -> [Pair] -> Aeson.Value
output tag pairs = object $ ("tag" .= tag) : pairs

-- | Scale a wait budget to the environment. Blockfrost runs triple it (the
-- follower observes ~1 block behind tip plus one poll interval, on a network
-- with much longer block times than the devnet timings most waits are written
-- for) and HYDRA_TEST_WAIT_MULTIPLIER multiplies further; CI sets it to
-- compensate for slow shared runners, local runs default to 1. Only failure
-- latency is affected: a passing wait returns as soon as its message arrives.
--
-- Budgets get a constant floor: the many @N * blockTime@ waits come to well
-- under a second on the 0.1s devnet, underestimating the fixed costs they
-- also cover (tx submission, observation, node processing). Sub-second waits
-- fired exactly when several suites shared one machine.
scaleWaitTime :: NominalDiffTime -> IO NominalDiffTime
scaleWaitTime d = do
  bf <-
    Prelude.getHydraNetwork >>= \case
      Prelude.Blockfrost -> pure 3
      _backend -> pure 1
  multiplier <- maybe 1 (realToFrac @Double) . (readMaybe =<<) <$> lookupEnv "HYDRA_TEST_WAIT_MULTIPLIER"
  pure $ max 5 (d * bf * multiplier)

-- | 'failAfter' with the budget scaled like 'scaleWaitTime'. Use for
-- whole-test backstops in end-to-end tests.
scaledFailAfter :: HasCallStack => NominalDiffTime -> IO a -> IO a
scaledFailAfter seconds action = scaleWaitTime seconds >>= (`Prelude.failAfter` action)

-- | Wait some time for a single API server output from each of given nodes.
-- This function waits for @delay@ seconds for message @expected@  to be seen by all
-- given @nodes@.
waitFor :: HasCallStack => Tracer IO HydraNodeLog -> NominalDiffTime -> [HydraClient] -> Aeson.Value -> IO ()
waitFor tracer delay nodes v = waitForAll tracer delay nodes [v]

-- | Wait up to some time and succeed if no API server output matches the given predicate.
-- The window is deliberately NOT scaled by 'scaleWaitTime': the timeout here
-- is the success path, so scaling it would slow every passing run.
waitNoMatch :: HasCallStack => NominalDiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO ()
waitNoMatch delay client match = do
  result <- try (void $ waitMatchWith delay client match) :: IO (Either SomeException ())
  case result of
    Left _ -> pure () -- Success: waitMatch failed to find a match
    Right _ -> failure "waitNoMatch: A match was found when none was expected"

-- | Wait up to some time for an API server output to match the given predicate.
-- The budget is scaled to the environment, see 'scaleWaitTime'.
waitMatch :: HasCallStack => NominalDiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO a
waitMatch delay' client match = do
  delay <- scaleWaitTime delay'
  waitMatchWith delay client match

-- | Like 'waitMatch' but with the given wall-clock budget, unscaled.
waitMatchWith :: HasCallStack => NominalDiffTime -> HydraClient -> (Aeson.Value -> Maybe a) -> IO a
waitMatchWith delay client@HydraClient{tracer, hydraNodeId, workDir} match = do
  seenMsgs <- newLabelledTVarIO "wait-match-seen-msgs" []
  timeout (realToFrac delay) (go seenMsgs) >>= \case
    Just x -> pure x
    Nothing -> do
      msgs <- readTVarIO seenMsgs
      failure $
        toString $
          unlines
            [ "waitMatch did not match a message within " <> show delay
            , padRight ' ' 20 "  nodeId:" <> show hydraNodeId
            , padRight ' ' 20 "  node logs:" <> maybe "<not spawned by this test>" (\d -> toText (d </> "logs")) workDir
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
  delay <- scaleWaitTime d
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

-- | Wait for the node's confirmed snapshot to hold exactly the given 'UTxO'.
--
-- NOTE: @/snapshot/utxo@ serves the latest confirmed snapshot, which can still
-- predate an increment or decommit just after its finalisation event. Sampling
-- it once therefore races on a slow machine, reporting the UTxO as it was one
-- snapshot ago. The last value seen is kept so that a genuine mismatch is still
-- reported as a mismatch, rather than as a bare timeout.
waitForSnapshotUTxO :: HasCallStack => NominalDiffTime -> HydraClient -> UTxO -> IO ()
waitForSnapshotUTxO delay node expected = do
  lastSeen <- newIORef mempty
  void . timeout (realToFrac delay) $ poll lastSeen
  readIORef lastSeen >>= (`shouldBe` expected)
 where
  poll lastSeen = do
    utxo <- getSnapshotUTxO node
    writeIORef lastSeen utxo
    unless (utxo == expected) $ threadDelay 0.1 >> poll lastSeen

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

getMetrics :: HasCallStack => HydraClient -> IO ByteString
getMetrics HydraClient{hydraNodeId, apiHost = Host{hostname}, monitoringPort} = do
  metricsPort <- case monitoringPort of
    Just p -> pure p
    Nothing -> failure $ "Cannot fetch metrics: hydra-node " <> show hydraNodeId <> " has no monitoringPort configured."
  Prelude.failAfter 3 $
    try (runReq defaultHttpConfig (request metricsPort)) >>= \case
      Left (e :: HttpException) -> failure $ "Request for hydra-node metrics failed: " <> show e
      Right body -> pure $ Req.responseBody body
 where
  request metricsPort =
    Req.req
      GET
      (Req.http hostname /: "metrics")
      NoReqBody
      Req.bsResponse
      (Req.port (fromIntegral metricsPort))

-- * Start / connect to a cluster of nodes

-- XXX: The two lists need to be of same length. Also the verification keys can
-- be derived from the signing keys.
withHydraCluster ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  Timing ->
  FilePath ->
  SocketPath ->
  -- | First node id
  -- This sets the starting point for assigning ports
  Int ->
  -- | NOTE: This decides on the size of the cluster!
  [(VerificationKey PaymentKey, Secret (SigningKey PaymentKey))] ->
  [Secret (SigningKey HydraKey)] ->
  -- | Transaction ids at which Hydra scripts should have been published.
  [TxId] ->
  (NonEmpty HydraClient -> IO a) ->
  IO a
withHydraCluster = withHydraClusterWith Nothing id

-- | Like 'withHydraCluster' but connecting each node's API client with the
-- given query string (e.g. "/?history=yes&snapshot-utxo=no") instead of the
-- default "/?history=yes", and adjusting each node's 'RunOptions' (e.g. to
-- disable logging) before it is started.
withHydraClusterWith ::
  HasCallStack =>
  Maybe String ->
  (RunOptions -> RunOptions) ->
  Tracer IO HydraNodeLog ->
  Timing ->
  FilePath ->
  SocketPath ->
  Int ->
  [(VerificationKey PaymentKey, Secret (SigningKey PaymentKey))] ->
  [Secret (SigningKey HydraKey)] ->
  [TxId] ->
  (NonEmpty HydraClient -> IO a) ->
  IO a
withHydraClusterWith mQueryParams mapOptions tracer timing workDir nodeSocket firstNodeId allKeys hydraKeys hydraScriptsTxId action = do
  when (clusterSize == 0) $
    failure "Cannot run a cluster with 0 number of nodes"
  when (length allKeys /= length hydraKeys) $
    failure "Not matching number of cardano/hydra keys"

  forM_ (zip allKeys allNodeIds) $ \((vk, sk), ix) -> do
    let vkFile = File $ workDir </> show ix <.> "vk"
    let skFile = File $ workDir </> show ix <.> "sk"
    void $ writeFileTextEnvelope vkFile Nothing vk
    void $ withSecret sk (writeFileTextEnvelope skFile Nothing)
  nodePorts <- allocateHydraNodePortsFor allNodeIds
  startNodes nodePorts [] allNodeIds
 where
  clusterSize = length allKeys

  allNodeIds = [firstNodeId .. firstNodeId + clusterSize - 1]

  startNodes nodePorts clients = \case
    [] -> action (fromList $ reverse clients)
    (nodeId : rest) -> do
      let hydraSigningKey = hydraKeys Prelude.!! (nodeId - firstNodeId)
          hydraVerificationKeys =
            [getVerificationKey sk | sk <- hydraKeys, sk /= hydraSigningKey]
          cardanoSigningKey = workDir </> show nodeId <.> "sk"
          cardanoVerificationKeys = [workDir </> show i <.> "vk" | i <- allNodeIds, i /= nodeId]
          chainConfig =
            Cardano
              defaultCardanoChainConfig
                { hydraScriptsTxId
                , cardanoSigningKey
                , cardanoVerificationKeys
                , contestationPeriod
                , depositPeriod
                , depositActivation
                , chainBackendOptions =
                    Direct
                      defaultDirectOptions
                        { nodeSocket = nodeSocket
                        }
                }
      withHydraNodeWith
        mQueryParams
        mapOptions
        tracer
        blockTime
        chainConfig
        workDir
        nodeId
        hydraSigningKey
        hydraVerificationKeys
        nodePorts
        (\c -> startNodes nodePorts (c : clients) rest)

  Timing{blockTime, contestationPeriod, depositPeriod, depositActivation} = timing

-- * Start / connect to a hydra-node

-- | The three ports a hydra-node binds: API, peer-to-peer listen, and the
-- optional Prometheus monitoring endpoint. Callers allocate these via
-- 'allocateHydraNodePorts' (or pre-allocate a full cluster) and thread them
-- through 'prepareHydraNode' / 'withHydraNode'.
data HydraNodePorts = HydraNodePorts
  { apiPort :: Network.PortNumber
  , listenPort :: Network.PortNumber
  , monitoringPort :: Network.PortNumber
  }
  deriving stock (Show, Eq)

-- | Allocate three unused TCP ports from the OS for a single hydra-node.
allocateHydraNodePorts :: IO HydraNodePorts
allocateHydraNodePorts = do
  m <- allocateHydraNodePortsFor [0]
  case Map.lookup 0 m of
    Just ports -> pure ports
    Nothing -> Prelude.error "allocateHydraNodePorts: empty allocation"

-- | Allocate ports for every node in a cluster up front. The returned map
-- must be passed to every 'prepareHydraNode' / 'withHydraNode' call for
-- nodes in this cluster so peers can be addressed correctly.
--
-- Listen ports are taken via 'randomUnusedTCPPortsWithDerived' so each
-- listen port's derived etcd /client/ port — 'peerPortToClientPort' — is
-- actually held bound at allocation time. That defends against two
-- failure modes: an unrelated process on the host occupying the derived
-- port (which a plain 'randomUnusedTCPPorts' would not catch and which
-- explodes as @EADDRINUSE@ the moment etcd starts), and an unlucky draw
-- where the derived port lands on top of another node's api/monitoring
-- port (which used to make the GRPC client talk to Warp instead of
-- etcd).
--
-- Api and monitoring ports are then acquired in a second batch and
-- checked to be disjoint from both the listen ports and the derived
-- client ports; on collision we retry that second batch.
allocateHydraNodePortsFor :: [Int] -> IO (Map Int HydraNodePorts)
allocateHydraNodePortsFor nodeIds = do
  listenPorts <- randomUnusedTCPPortsWithDerived peerPortToClientPort n
  let derivedClientPorts =
        [ fromIntegral (peerPortToClientPort (fromIntegral p))
        | p <- listenPorts
        ]
      reserved = listenPorts <> derivedClientPorts
  apiAndMonPorts <- acquireDisjoint reserved (20 :: Int)
  let apiPorts = take n apiAndMonPorts
      monPorts = drop n apiAndMonPorts
      assigned = Prelude.zipWith3 mkPorts apiPorts listenPorts monPorts
  pure $ Map.fromList $ zip nodeIds assigned
 where
  n = length nodeIds

  acquireDisjoint :: [Int] -> Int -> IO [Int]
  acquireDisjoint _ 0 =
    fail
      "allocateHydraNodePortsFor: ran out of retries trying to keep the api/monitoring ports disjoint from the derived etcd client ports"
  acquireDisjoint reserved remaining = do
    ps <- randomUnusedTCPPorts (2 * n)
    if null (ps `List.intersect` reserved)
      then pure ps
      else acquireDisjoint reserved (remaining - 1)

  mkPorts :: Int -> Int -> Int -> HydraNodePorts
  mkPorts a l m =
    HydraNodePorts
      { apiPort = fromIntegral a
      , listenPort = fromIntegral l
      , monitoringPort = fromIntegral m
      }

-- | Process-global cache mapping each @(workDir, nodeId)@ to its allocated
-- ports. The cache exists because restart-style tests (re-running
-- 'withSoloHydraNode' or similar against the same @workDir@) depend on
-- etcd's persistent cluster state, which is keyed by the listen URL. If a
-- restart picked fresh ports, etcd would refuse to start.
{-# NOINLINE soloHydraNodePortsCache #-}
soloHydraNodePortsCache :: IORef (Map (FilePath, Int) HydraNodePorts)
soloHydraNodePortsCache = unsafePerformIO (newIORef mempty)

-- | Allocate ports for a single hydra-node, memoizing the result for the
-- given @(workDir, nodeId)@ so that subsequent calls reuse the same ports.
soloHydraNodePortsFor :: FilePath -> Int -> IO HydraNodePorts
soloHydraNodePortsFor workDir nodeId = do
  cache <- readIORef soloHydraNodePortsCache
  case Map.lookup (workDir, nodeId) cache of
    Just ports -> pure ports
    Nothing -> do
      ports <- allocateHydraNodePorts
      atomicModifyIORef' soloHydraNodePortsCache $ \m ->
        (Map.insert (workDir, nodeId) ports m, ())
      pure ports

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
--
-- The @nodePorts@ map must contain an entry for this @hydraNodeId@ and for
-- every peer this node should connect to. Use 'allocateHydraNodePortsFor' to
-- build it for a cluster, or 'allocateHydraNodePorts' + a singleton map for a
-- standalone node.
prepareHydraNode ::
  HasCallStack =>
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  Map Int HydraNodePorts ->
  (Aeson.Value -> Aeson.Value) ->
  IO RunOptions
prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts paramsDecorator = do
  HydraNodePorts{apiPort, listenPort, monitoringPort} <-
    maybe
      (failure $ "prepareHydraNode: no port allocation for node " <> show hydraNodeId)
      pure
      (Map.lookup hydraNodeId nodePorts)
  -- NOTE: AirPlay on MacOS uses 5000 and we must avoid it.
  when (os == "darwin") $ listenPort `Prelude.shouldNotBe` (5_000 :: Network.PortNumber)
  let stateDir = workDir </> "state-" <> show hydraNodeId
  createDirectoryIfMissing True stateDir
  cardanoLedgerProtocolParametersFile <- preparePParams chainConfig stateDir paramsDecorator
  let hydraSigningKey = stateDir </> "me.sk"
  void $ withSecret hydraSKey $ writeFileTextEnvelope (File hydraSigningKey) Nothing
  hydraVerificationKeys <- forM (zip [1 ..] hydraVKeys) $ \(i :: Int, vKey) -> do
    let filepath = stateDir </> ("other-" <> show i <> ".vk")
    filepath <$ writeFileTextEnvelope (File filepath) Nothing vKey
  pure $
    RunOptions
      { verbosity = Verbose "HydraNode"
      , nodeId = NodeId $ show hydraNodeId
      , listen = Host "0.0.0.0" listenPort
      , advertise = Nothing
      , peers = peersFromMap
      , apiHost = "0.0.0.0"
      , apiPort
      , tlsCertPath = Nothing
      , tlsKeyPath = Nothing
      , monitoringPort = Just monitoringPort
      , hydraSigningKey
      , hydraVerificationKeys
      , persistenceDir = stateDir
      , persistenceRotateAfter = Nothing
      , chainConfig
      , -- NOTE: Use the system etcd to avoid ETXTBSY races where multiple
        -- parallel tests extract the embedded etcd binary into their own
        -- tempdirs and execve while another thread still holds the
        -- write-fd. The dev-shell and CI both provide etcd in $PATH.
        whichEtcd = SystemEtcd
      , ledgerConfig =
          CardanoLedgerConfig
            { cardanoLedgerProtocolParametersFile
            }
      , apiTransactionTimeout = 100000
      }
 where
  -- NOTE: See comment above about 0.0.0.0 vs 127.0.0.1
  peersFromMap =
    [ Host{Network.hostname = "0.0.0.0", Network.port = listenPort p}
    | (i, p) <- Map.toList nodePorts
    , i /= hydraNodeId
    ]

-- | Run a hydra-node with given 'RunOptions'.
withPreparedHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraClient -> IO a) ->
  IO a
withPreparedHydraNode = withPreparedHydraNodeWithQuery Nothing []

-- | Like 'withPreparedHydraNode' but with extra environment entries for the
-- hydra-node process (also inherited by its etcd child). Use this instead of
-- a process-global 'setEnv', which would leak into every other concurrently
-- spawned node.
withPreparedHydraNodeWithEnv ::
  HasCallStack =>
  [(String, String)] ->
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraClient -> IO a) ->
  IO a
withPreparedHydraNodeWithEnv = withPreparedHydraNodeWithQuery Nothing

-- | Like 'withPreparedHydraNode' but connecting the API client with the given
-- query string instead of the default "/?history=yes".
withPreparedHydraNodeWithQuery ::
  HasCallStack =>
  Maybe String ->
  [(String, String)] ->
  Tracer IO HydraNodeLog ->
  FilePath ->
  Int ->
  RunOptions ->
  (HydraClient -> IO a) ->
  IO a
withPreparedHydraNodeWithQuery mQueryParams extraEnv tracer workDir hydraNodeId runOptions action =
  Prelude.withLogFile logFilePath $ \logFileHandle -> do
    applyExtraEnv <-
      if null extraEnv
        then pure id
        else do
          baseEnv <- getEnvironment
          pure $ setEnv (extraEnv <> filter ((`notElem` map fst extraEnv) . fst) baseEnv)
    -- Benchmark-only hook: HYDRA_NODE_RTS_FLAGS appends '+RTS <flags> -RTS' to
    -- the spawned node (e.g. "-N2 -T"). Deliberately not GHCRTS, which every
    -- GHC binary in the environment would inherit. Unset means byte-identical
    -- spawns.
    rtsFlags <- maybe [] (map toString . words . toText) <$> lookupEnv "HYDRA_NODE_RTS_FLAGS"
    let extraArgs = if null rtsFlags then [] else ["+RTS"] <> rtsFlags <> ["-RTS"]
    let cmd =
          proc "hydra-node" (toArgs runOptions <> extraArgs)
            & setStdout (useHandleOpen logFileHandle)
            & setStderr createPipe
            & setCloseFds True
            & applyExtraEnv

    traceWith tracer $ HydraNodeCommandSpec $ show cmd

    withProcessTerm cmd $ \p -> do
      -- NOTE: exit code thread gets cancelled if 'action' terminates first
      raceLabelled
        ("collect-check-process-exit-code", collectAndCheckExitCode p)
        ("with-connection-to-node", withConnectionToNodeHost tracer hydraNodeId apiAddress monPort (mQueryParams <|> Just "/?history=yes") (\client -> action client{workDir = Just workDir}))
        <&> either absurd id
 where
  apiAddress =
    case runOptions of
      RunOptions{apiPort = p} ->
        Host{Network.hostname = "127.0.0.1", Network.port = p}

  monPort = case runOptions of
    RunOptions{monitoringPort = mp} -> mp

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

-- | Convenience: run a single hydra-node with no peers and freshly
-- allocated dynamic ports. Equivalent to 'withHydraNode' with a singleton
-- port map.
withSoloHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  NominalDiffTime ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  (HydraClient -> IO a) ->
  IO a
withSoloHydraNode tracer blockTime chainConfig workDir hydraNodeId hydraSKey hydraVKeys action = do
  ports <- soloHydraNodePortsFor workDir hydraNodeId
  withHydraNode tracer blockTime chainConfig workDir hydraNodeId hydraSKey hydraVKeys (Map.singleton hydraNodeId ports) action

-- | Convenience: 'withUnsyncedHydraNode' for a single node with freshly
-- allocated dynamic ports.
withUnsyncedSoloHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  (HydraClient -> IO a) ->
  IO a
withUnsyncedSoloHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys action = do
  ports <- soloHydraNodePortsFor workDir hydraNodeId
  withUnsyncedHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys (Map.singleton hydraNodeId ports) action

-- | Convenience: 'withHydraNodeCatchingUp' for a single node with freshly
-- allocated dynamic ports.
withSoloHydraNodeCatchingUp ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  (HydraClient -> IO a) ->
  IO a
withSoloHydraNodeCatchingUp tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys action = do
  ports <- soloHydraNodePortsFor workDir hydraNodeId
  withHydraNodeCatchingUp tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys (Map.singleton hydraNodeId ports) action

-- | Run a hydra-node just like `withHydraNode`; but before running any
-- action, observe a `Greetings` message with the node in sync first. NOTE
-- that importantly, any messages seen BEFORE we observe this will be lost;
-- i.e. unobservable by subsequent `waitFor`s.
--
-- See 'prepareHydraNode' for how to build the port map.
withHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  NominalDiffTime ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  Map Int HydraNodePorts ->
  (HydraClient -> IO a) ->
  IO a
withHydraNode = withHydraNodeWith Nothing id

-- | Like 'withHydraNode' but connecting the API client with the given query
-- string instead of the default "/?history=yes", and adjusting the node's
-- 'RunOptions' before it is started.
withHydraNodeWith ::
  HasCallStack =>
  Maybe String ->
  (RunOptions -> RunOptions) ->
  Tracer IO HydraNodeLog ->
  NominalDiffTime ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  Map Int HydraNodePorts ->
  (HydraClient -> IO a) ->
  IO a
withHydraNodeWith mQueryParams mapOptions tracer blockTime chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts action = do
  opts <- prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts id
  withPreparedHydraNodeWithQuery mQueryParams [] tracer workDir hydraNodeId (mapOptions opts) action'
 where
  waitTime = blockTime * 5
  action' client = do
    waitForNodesSynced waitTime [client]
    action client

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config/, but, importantly, do NOT wait for the sync status to be reported.
withUnsyncedHydraNode ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  Map Int HydraNodePorts ->
  (HydraClient -> IO a) ->
  IO a
withUnsyncedHydraNode tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts action = do
  opts <- prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts id
  withPreparedHydraNode tracer workDir hydraNodeId opts action

-- | Run a hydra-node with given 'ChainConfig' and using the config from
-- config and catching up with chain backend/.
withHydraNodeCatchingUp ::
  HasCallStack =>
  Tracer IO HydraNodeLog ->
  ChainConfig ->
  FilePath ->
  Int ->
  Secret (SigningKey HydraKey) ->
  [VerificationKey HydraKey] ->
  Map Int HydraNodePorts ->
  (HydraClient -> IO a) ->
  IO a
withHydraNodeCatchingUp tracer chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts action = do
  opts <- prepareHydraNode chainConfig workDir hydraNodeId hydraSKey hydraVKeys nodePorts id
  withPreparedHydraNode tracer workDir hydraNodeId opts action

withConnectionToNode :: forall a. Tracer IO HydraNodeLog -> Int -> Host -> Maybe Network.PortNumber -> (HydraClient -> IO a) -> IO a
withConnectionToNode tracer hydraNodeId apiHost monitoringPort =
  withConnectionToNodeHost tracer hydraNodeId apiHost monitoringPort (Just "/?history=yes")

withConnectionToNodeHost :: forall a. Tracer IO HydraNodeLog -> Int -> Host -> Maybe Network.PortNumber -> Maybe String -> (HydraClient -> IO a) -> IO a
withConnectionToNodeHost tracer hydraNodeId apiHost@Host{hostname, port} monitoringPort mQueryParams action = do
  connectedOnce <- newIORef False
  (retries, delay) <-
    Prelude.getHydraNetwork >>= \case
      Prelude.LocalDevnet -> pure (200, 0.1)
      Prelude.Mainnet -> pure (7200, 1)
      _ -> pure (300, 1)
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

  -- NOTE: Derived from the query string, so callers opt into CBOR by adding
  -- @encoding=cbor@ to their query params.
  apiEncoding
    | "encoding=cbor" `List.isInfixOf` queryParams = CborEncoding
    | otherwise = JsonEncoding

  doConnect connectedOnce = runClient (T.unpack hostname) (fromInteger . toInteger $ port) queryParams $
    \connection -> do
      atomicWriteIORef connectedOnce True
      traceWith tracer (NodeStarted hydraNodeId)
      res <- action $ HydraClient{hydraNodeId, apiHost, monitoringPort, connection, tracer, apiEncoding, workDir = Nothing}
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

waitForNodesSynced :: HasCallStack => NominalDiffTime -> [HydraClient] -> IO ()
waitForNodesSynced delay clients = do
  -- Wait for Greetings from each client. Greetings is always sent AFTER
  -- historical replay, so receiving it means we've consumed all historical
  -- messages. This prevents tests from matching on historical HeadIsOpen or
  -- NodeSynced messages from previous runs when using a persistent state dir.
  syncedStatuses <- forConcurrently (toList clients) $ \client ->
    waitMatch delay client $ \v -> do
      guard $ v ^? key "tag" == Just "Greetings"
      v ^? key "chainSyncedStatus" . _String
  -- If any node is still catching up, additionally wait for a fresh NodeSynced
  when ("CatchingUp" `elem` syncedStatuses) $
    forConcurrently_ (toList clients) $ \client ->
      waitMatch delay client $ \v ->
        guard $ v ^? key "tag" == Just "NodeSynced"
