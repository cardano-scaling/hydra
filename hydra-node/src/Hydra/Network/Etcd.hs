{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Implements a Hydra network component using [etcd](https://etcd.io/).
--
-- While implementing a basic broadcast protocol over a distributed key-value
-- store is quite an overkill, the [Raft](https://raft.github.io/) consensus of
-- etcd provides our application with a crash-recovery fault-tolerant "atomic
-- broadcast" out-of-the-box. As a nice side-effect, the network layer becomes
-- very introspectable, while it would also support features like TLS or service
-- discovery.
--
-- The component installs, starts and configures an `etcd` instance and connects
-- to it using a GRPC client. We can only write and read from the cluster while
-- connected to the majority cluster.
--
-- Broadcasting is implemented using @put@ to some well-known key, while message
-- delivery is done by using @watch@ on the same 'msg' prefix. We keep a last known
-- revision, also stored on disk, to start 'watch' with that revision (+1) and
-- only deliver messages that were not seen before. In case we are not connected
-- to our 'etcd' instance or not enough peers (= on a minority cluster), we
-- retry sending, but also store messages to broadcast in a 'PersistentQueue',
-- which makes the node resilient against crashes while sending.
--
-- Connectivity and compatibility with other nodes on the cluster is tracked
-- using the key-value service as well:
--
--   * network connectivity is determined by being able to fetch the member list
--   * peer connectivity is tracked (best effort, not authorized) using an entry
--     at 'alive-\<advertise\>' keys with individual leases and repeated keep-alives
--   * each node compare-and-swaps its `version` into a key of the same name to
--     check compatibility (not updatable)
--
-- Note that the etcd cluster is configured to compact revisions down to 1000
-- every 5 minutes. This prevents infinite growth of the key-value store, but
-- also limits how long a node can be disconnected without missing out. 1000
-- should be more than enough for our use-case as the Hydra protocol will not
-- advance unless all participants are present.
module Hydra.Network.Etcd where

import Hydra.Prelude

import Cardano.Binary (decodeFull', serialize')
import Cardano.Crypto.Hash (SHA256, hashToStringAsHex, hashWithSerialiser)
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Concurrent.Class.MonadSTM (
  isFullTBQueue,
  modifyTVar',
  peekTBQueue,
  readTBQueue,
  readTVarIO,
  swapTVar,
  tryPeekTBQueue,
  tryReadTBQueue,
  unGetTBQueue,
  writeTBQueue,
  writeTVar,
 )
import Control.Exception (IOException)
import Control.Lens ((^.), (^..), (^?))
import Data.Aeson (decodeFileStrict', encodeFile)
import Data.Aeson qualified as Aeson
import Data.Aeson.Lens qualified as Aeson
import Data.Aeson.Types (Value)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List ((\\))
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (
  Connectivity (..),
  Host (..),
  Network (..),
  NetworkCallback (..),
  NetworkComponent,
  NetworkConfiguration (..),
  ProtocolVersion,
 )
import Hydra.Network.EtcdBinary (getEtcdBinary)
import Network.GRPC.Client (
  Address (..),
  ConnParams (..),
  Connection,
  ReconnectPolicy (..),
  ReconnectTo (ReconnectToOriginal),
  Server (..),
  ServerDisconnected,
  Timeout (..),
  TimeoutUnit (..),
  TimeoutValue (..),
  rpc,
  withConnection,
 )
import Network.GRPC.Client.StreamType.IO (biDiStreaming, nonStreaming)
import Network.GRPC.Common (GrpcError (..), GrpcException (..), HTTP2Settings (..), NextElem (..), def, defaultHTTP2Settings)
import Network.GRPC.Common.Protobuf (Proto (..), Protobuf, defMessage, (.~))
import Network.GRPC.Etcd (
  Compare'CompareResult (..),
  Compare'CompareTarget (..),
  KV,
  Lease,
  Watch,
 )
import Network.HTTP2.Client (HTTP2Error)
import Network.Socket (PortNumber)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import System.Environment.Blank (getEnvironment)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError, isEOFError)
import System.Process (interruptProcessGroupOf)
import System.Process.Typed (
  Process,
  ProcessConfig,
  createPipe,
  getStderr,
  proc,
  setCreateGroup,
  setEnv,
  setStderr,
  startProcess,
  stopProcess,
  unsafeProcessHandle,
  waitExitCode,
 )

-- | Concrete network component that broadcasts messages to an etcd cluster and
-- listens for incoming messages.
withEtcdNetwork ::
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO EtcdLog ->
  ProtocolVersion ->
  -- TODO: check if all of these needed?
  NetworkConfiguration ->
  NetworkComponent IO msg msg ()
withEtcdNetwork tracer protocolVersion config callback action = do
  etcdBinPath <- getEtcdBinary persistenceDir whichEtcd
  -- TODO: fail if cluster config / members do not match --peer
  -- configuration? That would be similar to the 'acks' persistence
  -- bailing out on loading.
  envVars <- Map.fromList <$> getEnvironment
  withProcessInterrupt (etcdCmd etcdBinPath envVars) $ \p -> do
    raceLabelled_
      ( "etcd-waitExitCode"
      , do
          waitExitCode p >>= \ec -> fail $ "Sub-process etcd exited with: " <> show ec
      )
      ( "etcd-callback-1"
      , raceLabelled_
          ("etcd-traceStderr", traceStderr p callback)
          ( "etcd-callback-2"
          , do
              -- NOTE: The connection to the server is set up asynchronously; the
              -- first rpc call will block until the connection has been established.
              withConnection (connParams tracer Nothing) (grpcServer config) $ \conn -> do
                -- REVIEW: checkVersion blocks if used on main thread - why?
                withAsyncLabelled ("etcd-checkVersion", checkVersion tracer conn protocolVersion callback) $ \_ -> do
                  raceLabelled_
                    ("etcd-pollConnectivity", pollConnectivity tracer conn advertise callback)
                    ( "etcd-callback-3"
                    , raceLabelled_
                        ("etcd-waitMessages", waitMessages tracer conn persistenceDir callback)
                        ( "etcd-callback-4"
                        , do
                            queue <- newPersistentQueue tracer (persistenceDir </> "pending-broadcast") 100
                            raceLabelled_
                              ("etcd-broadcastMessages", broadcastMessages tracer config advertise queue)
                              ( "etcd-network-component-action"
                              , do
                                  action
                                    Network
                                      { broadcast = writePersistentQueue tracer queue
                                      }
                              )
                        )
                    )
          )
      )
 where
  clientHost = Host{hostname = "127.0.0.1", port = getClientPort config}
  traceStderr p NetworkCallback{onConnectivity} =
    let loop = do
          bs <- BS.hGetLine (getStderr p)
          case Aeson.eitherDecodeStrict bs of
            Left err -> traceWith tracer FailedToDecodeLog{log = decodeUtf8 bs, reason = show err}
            Right v -> do
              let expectedClusterMismatch = do
                    level' <- bs ^? Aeson.key "level" . Aeson.nonNull
                    msg' <- bs ^? Aeson.key "msg" . Aeson.nonNull
                    pure (level', msg')
              case expectedClusterMismatch of
                Just (Aeson.String "error", Aeson.String "request sent was ignored due to cluster ID mismatch") ->
                  onConnectivity ClusterIDMismatch{clusterPeers = T.pack clusterPeers}
                _ -> traceWith tracer $ EtcdLog{etcd = v}
          loop
     in -- When etcd's stderr pipe closes (because the etcd subprocess exited),
        -- 'BS.hGetLine' raises 'hGetLine: end of file'. We don't want that
        -- naked IOException racing the descriptive "Sub-process etcd exited
        -- with: ExitFailure N" from 'etcd-waitExitCode' below — on slower
        -- machines the EOF often wins, and the IOException then gets caught
        -- by 'withAPIServer's IOException handler and re-thrown as
        -- 'RunServerException', stripping every mention of etcd from the
        -- final error. Block on EOF instead so 'etcd-waitExitCode' is always
        -- the one that fires.
        loop `catch` \e ->
          if isEOFError e
            then forever (threadDelay 60)
            else throwIO e

  -- XXX: Could use TLS to secure peer connections
  -- XXX: Could use discovery to simplify configuration
  -- NOTE: Configured using guides: https://etcd.io/docs/v3.5/op-guide
  etcdCmd etcdBinPath envVars =
    -- NOTE: We map prefers the left; so we need to mappend default at the end.
    setEnv (Map.toList $ envVars <> defaultEnv)
      . setCreateGroup True -- Prevents interrupt of main process when we send SIGINT to etcd
      . setStderr createPipe
      . proc etcdBinPath
      $ concat
        [ -- NOTE: Must be used in clusterPeers
          ["--name", show advertise]
        , ["--data-dir", persistenceDir </> "etcd" </> hashToStringAsHex (hashWithSerialiser @SHA256 toCBOR $ BS8.pack clusterPeers)]
        , ["--listen-peer-urls", httpUrl listen]
        , ["--initial-advertise-peer-urls", httpUrl advertise]
        , ["--listen-client-urls", httpUrl clientHost]
        , -- Pick a random port for http api (and use above only for grpc)
          ["--listen-client-http-urls", "http://localhost:0"]
        , -- Client access only on configured 'host' interface.
          ["--advertise-client-urls", httpUrl clientHost]
        , -- XXX: Could use unique initial-cluster-tokens to isolate clusters
          ["--initial-cluster-token", "hydra-network-1"]
        , ["--initial-cluster", clusterPeers]
        ]

  defaultEnv :: Map.Map String String
  defaultEnv =
    -- Keep up to 1000 revisions. See also:
    -- https://etcd.io/docs/v3.5/op-guide/maintenance/#auto-compaction
    Map.fromList
      [ ("ETCD_AUTO_COMPACTION_MODE", "revision")
      , ("ETCD_AUTO_COMPACTION_RETENTION", "1000")
      ]

  -- NOTE: Building a canonical list of labels from the advertised addresses
  clusterPeers =
    intercalate ","
      . map (\h -> show h <> "=" <> httpUrl h)
      $ (advertise : peers)

  httpUrl (Host h p) = "http://" <> toString h <> ":" <> show p

  NetworkConfiguration{persistenceDir, listen, advertise, peers, whichEtcd} = config

connParams :: Tracer IO EtcdLog -> Maybe Timeout -> ConnParams
connParams tracer to =
  def
    { connReconnectPolicy = reconnectPolicy
    , -- NOTE: Do not rate limit pings or settings from our trusted, local etcd
      -- node; see the comments on both override fields. The two guards trip on
      -- the same thing: grpc-go raises its receive window as inbound volume
      -- grows, pinging to measure the round-trip and sending a SETTINGS frame
      -- per step. Sustained broadcast walks more steps than the default
      -- 4 SETTINGS/s allows, and http2 then kills the connection (#2817).
      connHTTP2Settings =
        defaultHTTP2Settings
          { http2OverridePingRateLimit = Just maxBound
          , http2OverrideSettingsRateLimit = Just maxBound
          }
    , connDefaultTimeout = to
    }
 where
  reconnectPolicy = ReconnectAfter ReconnectToOriginal $ do
    threadDelay 1
    traceWith tracer Reconnecting
    pure reconnectPolicy

grpcServer :: NetworkConfiguration -> Server
grpcServer config =
  ServerInsecure $
    Address
      { addressHost = toString $ hostname clientHost
      , addressPort = port clientHost
      , addressAuthority = Nothing
      }
 where
  clientHost = Host{hostname = "127.0.0.1", port = getClientPort config}

-- | Get the client port corresponding to a listen address.
--
-- The client port used by the started etcd port is offset by the same amount as
-- the listen address is offset by the default port 5001. This will result in
-- the default client port 2379 be used by default still.
getClientPort :: NetworkConfiguration -> PortNumber
getClientPort NetworkConfiguration{listen} = peerPortToClientPort (port listen)

-- | Derive the etcd client port from a configured peer (listen) port.
--
-- Exposed separately from 'getClientPort' so test fixtures can pre-allocate
-- both the peer and the derived client port without constructing a full
-- 'NetworkConfiguration'. Keep this and 'getClientPort' in lockstep — any
-- change to the offset must happen here, in one place.
peerPortToClientPort :: PortNumber -> PortNumber
peerPortToClientPort listenPort = 2379 + listenPort - 5001

-- | Check and write version on etcd cluster. This will retry until we are on a
-- majority cluster and succeed. If the version does not match a corresponding
-- 'Connectivity' message is sent via 'NetworkCallback'.
checkVersion ::
  Tracer IO EtcdLog ->
  Connection ->
  ProtocolVersion ->
  NetworkCallback msg IO ->
  IO ()
checkVersion tracer conn ourVersion NetworkCallback{onConnectivity} = do
  -- Get or write our version into kv store
  res <-
    nonStreaming conn (rpc @(Protobuf KV "txn")) $
      defMessage
        & #compare .~ [versionExists]
        & #success .~ [getVersion]
        & #failure .~ [putVersion]

  -- Check version if version was already present
  if res ^. #succeeded
    then forM_ (res ^.. #responses . traverse . #responseRange . #kvs . traverse) $ \kv ->
      case decodeFull' $ kv ^. #value of
        Left err -> do
          traceWith tracer $
            FailedToDecodeValue
              { key = decodeUtf8 $ kv ^. #key
              , value = encodeBase16 $ kv ^. #value
              , reason = show err
              }
          onConnectivity VersionMismatch{ourVersion, theirVersion = Nothing}
        Right theirVersion ->
          unless (theirVersion == ourVersion) $
            onConnectivity VersionMismatch{ourVersion, theirVersion = Just theirVersion}
    else traceWith tracer $ MatchingProtocolVersion{version = ourVersion}
 where
  versionKey = "version"

  -- exists = create_revision of key 'version' > 0
  versionExists =
    defMessage
      & #result .~ Proto Compare'GREATER
      & #target .~ Proto Compare'VERSION
      & #key .~ versionKey
      & #version .~ 0

  getVersion =
    defMessage & #requestRange .~ (defMessage & #key .~ versionKey)

  putVersion =
    defMessage
      & #requestPut
        .~ ( defMessage
              & #key .~ versionKey
              & #value .~ serialize' ourVersion
           )

-- | Broadcast messages from a queue to the etcd cluster.
--
-- Retries on failure to 'putMessage' in case we are on a minority cluster or
-- when the grpc call timeouts.
--
-- Idempotent under transient 'GrpcDeadlineExceeded': 'putMessage' uses an
-- etcd transaction conditioned on the key's current 'mod_revision' matching
-- the last revision we successfully wrote. If a deadline-exceeded retry
-- arrives at etcd after the original request already committed, the compare
-- fails server-side (the mod_revision has advanced), the failure branch's
-- range tells us what the new revision is, and we move on without writing a
-- second time. So a retry whose original committed creates zero extra etcd
-- revisions and the watcher on each peer sees exactly one event per logical
-- broadcast. Same key namespace as master ('msg-\<host\>'), no disk growth.
broadcastMessages ::
  Tracer IO EtcdLog ->
  NetworkConfiguration ->
  -- | Used to identify sender.
  Host ->
  PersistentQueue IO msg ->
  IO ()
broadcastMessages tracer config ourHost queue = do
  -- Seed 'lastModRev' from etcd. With this in place the in-memory value
  -- always matches the server's view of our key when the loop starts:
  --
  --   * fresh process + fresh etcd → key absent → @lastModRev = 0@;
  --     'putMessage' compares against 0 (which etcd treats as "key does
  --     not exist") and the success branch creates the key.
  --   * fresh process + persisted etcd (e.g. Carol restart) → key
  --     present with some non-zero @mod_revision@ → @lastModRev@ starts
  --     at that revision; 'putMessage' compares against it and the
  --     success branch advances.
  --
  -- The init query removes the ambiguity that the older code had on
  -- @lastModRev == 0 + compare-fail@: with seeding, any compare-fail is
  -- unambiguously this peer's own deadline-exceeded retry, so the
  -- failure branch just adopts the new baseline and pops.
  initialModRev <- retryInitQuery
  lastModRevVar <- newLabelledTVarIO "etcd-broadcast-last-mod-rev" initialModRev
  inFlightVar <- newLabelledTVarIO "etcd-broadcast-in-flight" Nothing
  withGrpcContext "broadcastMessages" . forever $ do
    -- Block for work before opening a connection, then keep it for
    -- 'maxPutsPerConnection' puts, waiting on it while the queue is dry. A
    -- handshake per burst costs ~0.5ms on the hot path and churns ephemeral
    -- ports. Messages are only popped after a successful put, so a recycled or
    -- failed connection never loses one.
    void $ peekPersistentQueue queue
    catchJust retryableEtcdError (sendPending lastModRevVar inFlightVar) $ \reason -> do
      traceWith tracer $ BroadcastFailed{reason}
      threadDelay 1
 where
  sendPending lastModRevVar inFlightVar =
    withConnection (connParams tracer (Just . Timeout Second $ TimeoutValue 3)) (grpcServer config) $ \conn ->
      let go n
            | n <= 0 = pure ()
            | otherwise = do
                -- Waiting here, rather than returning on an empty queue, is what
                -- makes a connection last 'maxPutsPerConnection' puts and not a
                -- single burst.
                void $ peekPersistentQueue queue
                nextPendingBatch inFlightVar queue maxBatchCount maxBatchBytes >>= \case
                  -- Unreachable: we are the only consumer and just peeked.
                  Nothing -> go n
                  Just batch -> do
                    putMessage tracer conn ourHost lastModRevVar (batchValue $ snd <$> batch)
                    popBatchPersistentQueue tracer queue batch
                    atomically $ writeTVar inFlightVar Nothing
                    go (n - 1)
       in go maxPutsPerConnection

  -- Hedge against a connection wedging without raising. Not needed for
  -- https://github.com/cardano-scaling/hydra/issues/2167: that was seen while
  -- broadcast shared the connection with 'waitMessages'' watch stream, and a
  -- dedicated one sustains 20k puts.
  maxPutsPerConnection = 1000 :: Int

  maxBatchCount = 50

  -- Keeps the value comfortably below etcd's default 1.5MiB request limit.
  maxBatchBytes = 256 * 1024
  -- Same retry shape as the broadcast loop, so we survive an etcd cluster that
  -- is still electing or that we briefly cannot reach.
  retryInitQuery =
    catchJust retryableEtcdError (queryInitialModRev tracer config ourHost) $ \reason -> do
      traceWith tracer $ BroadcastFailed{reason}
      threadDelay 1
      retryInitQuery

-- | Query etcd for the current 'mod_revision' of this peer's broadcast key.
-- Returns 0 if the key does not yet exist. Used by 'broadcastMessages' to
-- seed its in-memory baseline so subsequent @compare mod_revision@ checks
-- match etcd's reality from the very first 'putMessage'.
queryInitialModRev ::
  Tracer IO EtcdLog ->
  NetworkConfiguration ->
  Host ->
  IO Int64
queryInitialModRev tracer config ourHost =
  withConnection (connParams tracer (Just . Timeout Second $ TimeoutValue 3)) (grpcServer config) $ \conn -> do
    res <- nonStreaming conn (rpc @(Protobuf KV "range")) req
    pure $ fromMaybe 0 (res ^? #kvs . traverse . #modRevision)
 where
  key = encodeUtf8 @Text $ "msg-" <> show ourHost
  req = defMessage & #key .~ key

-- | Broadcast a message to the etcd cluster.
--
-- Wraps the etcd @put@ in a @Txn@:
--
--   * compare: @mod_revision(key) == lastModRev@
--   * success: @put(key, value)@
--   * failure: @range(key)@ (so we can learn the actual @mod_revision@ if our
--     compare failed, e.g. because a previous attempt of ours committed
--     server-side after returning 'GrpcDeadlineExceeded' to the client)
--
-- 'lastModRevVar' is updated in both branches: from the response header on
-- a successful put, from the range result on a compare failure. Retries of
-- the same @msg@ after a deadline-exceeded converge to a single effective
-- revision rather than producing duplicate deliveries. Because
-- 'broadcastMessages' seeds 'lastModRev' from etcd at startup
-- ('queryInitialModRev'), a compare failure unambiguously means "this
-- peer's earlier attempt already wrote a later revision than we have
-- recorded" — i.e. the message is already delivered and the caller can pop.
--
-- __Cluster-reset behaviour (intentionally fatal):__ if the compare fails
-- and the @range@ branch returns no kvs, the etcd cluster has lost the
-- key we wrote against — either the data dir was wiped or the cluster
-- was replaced underneath us. We do not silently reseed: this is a
-- node-level event that should surface, not be papered over. 'putMessage'
-- calls 'fail', which kills the broadcast loop, propagates up to take
-- down the node, and on restart 'queryInitialModRev' re-seeds
-- 'lastModRev' from whatever state etcd actually has.
putMessage ::
  Tracer IO EtcdLog ->
  -- | Connection provided (and recycled) by 'broadcastMessages'.
  Connection ->
  -- | Used to identify sender.
  Host ->
  -- | The peer's last observed 'mod_revision' on its own broadcast key.
  TVar IO Int64 ->
  -- | Value to write, a batch of serialized messages (see 'batchValue').
  ByteString ->
  IO ()
putMessage tracer conn ourHost lastModRevVar value = do
  lastModRev <- readTVarIO lastModRevVar
  res <- nonStreaming conn (rpc @(Protobuf KV "txn")) (txnReq lastModRev)
  if res ^. #succeeded
    then
      -- Our compare matched and the put ran. The new mod_revision on
      -- our key equals the cluster revision returned in the response
      -- header.
      atomically $ writeTVar lastModRevVar (res ^. #header . #revision)
    else case res ^? #responses . traverse . #responseRange . #kvs . traverse . #modRevision of
      Just observedModRev -> do
        -- Compare failed. Since 'broadcastMessages' seeded 'lastModRev'
        -- from etcd at startup and we are the only writer to our key,
        -- the only way 'mod_revision' moved past 'lastModRev' is an
        -- earlier attempt of ours committing server-side despite a
        -- 'GrpcDeadlineExceeded' to the client. The message has been
        -- delivered; adopt the new baseline and let the outer loop pop.
        traceWith tracer BroadcastDeduped{previousModRev = lastModRev, observedModRev}
        atomically $ writeTVar lastModRevVar observedModRev
      Nothing ->
        -- Compare failed AND range came back empty: etcd has no record
        -- of our key. Unreachable in normal operation (only we write to
        -- our key; nothing deletes it). If we ever do hit this, the
        -- safe move is to crash loudly — the surrounding race kills the
        -- node and a fresh start re-runs 'queryInitialModRev' against
        -- whatever state etcd actually has.
        fail $
          "putMessage: compare against mod_revision "
            <> show lastModRev
            <> " failed but our broadcast key has no current value in etcd"
 where
  key = encodeUtf8 @Text $ "msg-" <> show ourHost

  -- Compare: mod_revision(key) == lastModRev
  modRevMatches lastModRev =
    defMessage
      & #result .~ Proto Compare'EQUAL
      & #target .~ Proto Compare'MOD
      & #key .~ key
      & #modRevision .~ lastModRev

  putReqOp =
    defMessage
      & #requestPut
        .~ ( defMessage
              & #key .~ key
              & #value .~ value
           )

  rangeReqOp =
    defMessage & #requestRange .~ (defMessage & #key .~ key)

  txnReq lastModRev =
    defMessage
      & #compare .~ [modRevMatches lastModRev]
      & #success .~ [putReqOp]
      & #failure .~ [rangeReqOp]

-- | Assemble the etcd value for a batch of already-serialized messages: a
-- CBOR list reusing each message's encoding verbatim. Uses the same
-- indefinite-length list format as cardano-binary's list instances, so the
-- value decodes as @[msg]@ on the receiving side.
batchValue :: [ByteString] -> ByteString
batchValue encodedItems =
  CBOR.toStrictByteString $
    CBOR.encodeListLenIndef
      <> foldMap CBOR.encodePreEncoded encodedItems
      <> CBOR.encodeBreak

-- | Fetch and wait for messages from the etcd cluster.
waitMessages ::
  forall msg.
  FromCBOR msg =>
  Tracer IO EtcdLog ->
  Connection ->
  FilePath ->
  NetworkCallback msg IO ->
  IO ()
waitMessages tracer conn directory NetworkCallback{deliver} =
  withGrpcContext "waitMessages" . forever $ do
    -- Restart a failed watch from the last known revision, same as one that
    -- ended cleanly. Without this any connection blip kills the node.
    catchJust retryableEtcdError watch $ \reason ->
      traceWith tracer WatchFailed{reason}
    -- Wait before re-trying
    threadDelay 1
 where
  -- NOTE: We have not observed the watch (subscription) fail even when peers
  -- leave and we end up on a minority cluster.
  watch =
    biDiStreaming conn (rpc @(Protobuf Watch "watch")) $ \send recv -> do
      revision <- getLastKnownRevision directory
      let startRevision = fromIntegral (revision + 1)
      traceWith tracer WatchMessagesStartRevision{startRevision}
      -- NOTE: Request all keys starting with 'msg'. See also section KeyRanges
      -- in https://etcd.io/docs/v3.5/learning/api/#key-value-api
      let watchRequest =
            defMessage
              & #key .~ "msg"
              & #rangeEnd .~ "msh" -- NOTE: g+1 to query prefixes
              & #startRevision .~ fromIntegral (revision + 1)
      send . NextElem $ defMessage & #createRequest .~ watchRequest
      loop send recv

  loop send recv =
    recv >>= \case
      NoNextElem -> pure ()
      NextElem res ->
        if res ^. #canceled
          then do
            let compactRevision = res ^. #compactRevision
            traceWith tracer WatchMessagesFallbackTo{compactRevision}
            putLastKnownRevision directory . fromIntegral $ (compactRevision - 1) `max` 0
            -- Gracefully close watch stream
            send NoNextElem
          else do
            let revision = res ^. #header . #revision
            putLastKnownRevision directory . fromIntegral $ revision `max` 0
            forM_ (res ^. #events) process
            loop send recv

  process event = do
    let value = event ^. #kv . #value
    -- Broadcast values carry a batch of messages per revision (see
    -- 'batchValue'). Watch catch-up can still replay single-message values
    -- written before an upgrade, so fall back to decoding one message.
    case decodeFull' @[msg] value of
      Right msgs -> forM_ msgs deliver
      Left err ->
        case decodeFull' value of
          Right msg -> deliver msg
          Left _ ->
            traceWith
              tracer
              FailedToDecodeValue
                { key = decodeUtf8 $ event ^. #kv . #key
                , value = encodeBase16 value
                , reason = show err
                }

getLastKnownRevision :: MonadIO m => FilePath -> m Natural
getLastKnownRevision directory = do
  liftIO $
    try (decodeFileStrict' $ directory </> "last-known-revision") >>= \case
      Right rev -> do
        pure $ fromMaybe 0 rev
      Left (e :: IOException)
        | isDoesNotExistError e -> pure 0
        | otherwise -> do
            fail $ "Failed to load last known revision: " <> show e

putLastKnownRevision :: MonadIO m => FilePath -> Natural -> m ()
putLastKnownRevision directory rev = do
  liftIO $ encodeFile (directory </> "last-known-revision") rev

-- | Write a well-known key to indicate being alive, keep it alive using a lease
-- and poll other peers entries to yield connectivity events. While doing so,
-- overall network connectivity is determined from the ability to read/write to
-- the cluster.
pollConnectivity ::
  Tracer IO EtcdLog ->
  Connection ->
  -- | Local host
  Host ->
  NetworkCallback msg IO ->
  IO ()
pollConnectivity tracer conn advertise NetworkCallback{onConnectivity} = do
  seenAliveVar <- newLabelledTVarIO "etcd-seen-alive" []
  withGrpcContext "pollConnectivity" . forever $
    catchJust retryableEtcdError (poll seenAliveVar) (onConnectionLost seenAliveVar)
 where
  poll seenAliveVar = do
    leaseId <- createLease
    -- If we can create a lease, we are connected
    onConnectivity NetworkConnected
    -- Write our alive key using lease
    writeAlive leaseId
    traceWith tracer CreatedLease{leaseId}
    withKeepAlive leaseId (aliveLoop seenAliveVar)

  aliveLoop seenAliveVar keepAlive = do
    -- Keep our lease alive
    ttlRemaining <- keepAlive
    if ttlRemaining <= 0
      then
        -- The keep alive did not work as no time to live remaining. Get a new lease instead
        traceWith tracer LowLeaseTTL{ttlRemaining}
      else do
        -- Determine alive peers
        alive <- getAlive
        let othersAlive = alive \\ [advertise]
        seenAlive <- atomically $ swapTVar seenAliveVar othersAlive
        forM_ (othersAlive \\ seenAlive) $ onConnectivity . PeerConnected
        forM_ (seenAlive \\ othersAlive) $ onConnectivity . PeerDisconnected
        threadDelay 1
        aliveLoop seenAliveVar keepAlive

  -- Report the network as down and let the outer loop take a fresh lease.
  onConnectionLost seenAliveVar _reason = do
    onConnectivity NetworkDisconnected
    atomically $ writeTVar seenAliveVar []
    threadDelay 1

  createLease = withGrpcContext "createLease" $ do
    leaseResponse <-
      nonStreaming conn (rpc @(Protobuf Lease "leaseGrant")) $
        defMessage & #ttl .~ 3
    pure $ leaseResponse ^. #id

  withKeepAlive leaseId action = do
    biDiStreaming conn (rpc @(Protobuf Lease "leaseKeepAlive")) $ \send recv -> do
      void . action $ do
        send $ NextElem $ defMessage & #id .~ leaseId
        recv >>= \case
          NextElem res -> pure $ res ^. #ttl
          NoNextElem -> do
            traceWith tracer NoKeepAliveResponse
            pure 0

  writeAlive leaseId = withGrpcContext "writeAlive" $ do
    void . nonStreaming conn (rpc @(Protobuf KV "put")) $
      defMessage
        & #key .~ "alive-" <> show advertise
        & #value .~ serialize' advertise
        & #lease .~ leaseId

  getAlive = withGrpcContext "getAlive" $ do
    res <-
      nonStreaming conn (rpc @(Protobuf KV "range")) $
        defMessage
          & #key .~ "alive"
          & #rangeEnd .~ "alivf" -- NOTE: e+1 to query prefixes
    flip mapMaybeM (res ^.. #kvs . traverse) $ \kv -> do
      let value = kv ^. #value
      case decodeFull' value of
        Left err -> do
          traceWith
            tracer
            FailedToDecodeValue
              { key = decodeUtf8 $ kv ^. #key
              , value = encodeBase16 value
              , reason = show err
              }
          pure Nothing
        Right x -> pure $ Just x

-- | Predicate for gRPC errors that we treat as transient — connection blips
-- or etcd-side disruption from which a retry is expected to recover. Anything
-- outside this set is escalated by re-throwing.
--
-- 'GrpcNotFound' is included specifically for lease loss: when etcd's RAFT
-- leader changes under network stress, in-flight leases are revoked, and the
-- next operation referencing one ('pollConnectivity.writeAlive') comes back
-- with NOT_FOUND. The recovery is to mark the network as disconnected and let
-- the outer loop recreate the lease, not crash the node.
isTransientGrpcError :: GrpcError -> Bool
isTransientGrpcError =
  (`elem` [GrpcUnavailable, GrpcDeadlineExceeded, GrpcCancelled, GrpcNotFound])

-- | Classify a failure talking to our local etcd, giving a reason to trace when
-- retrying is the right response.
--
-- Connection-level failures qualify: etcd is a subprocess we started and
-- 'connParams' reconnects, so a lost connection is a blip rather than a reason
-- to take the node down. Those arrive either as 'HTTP2Error' (straight from the
-- @http2@ client, e.g. the SETTINGS rate limit of #2817) or 'ServerDisconnected'
-- (grapesy's wrapper when a call outlives its connection). Everything else
-- escalates, including 'putMessage's cluster-reset 'fail'.
retryableEtcdError :: SomeException -> Maybe Text
retryableEtcdError e
  | Just GrpcException{grpcError, grpcErrorMessage} <- fromException e =
      if isTransientGrpcError grpcError
        then Just $ fromMaybe (show grpcError) grpcErrorMessage
        else Nothing
  | Just (http2Error :: HTTP2Error) <- fromException e = Just $ show http2Error
  | Just (disconnected :: ServerDisconnected) <- fromException e = Just $ show disconnected
  | otherwise = Nothing

-- | Add context to the 'grpcErrorMessage' of any 'GrpcException' raised.
withGrpcContext :: MonadCatch m => Text -> m a -> m a
withGrpcContext context action =
  action `catch` \e@GrpcException{grpcErrorMessage} ->
    throwIO
      e
        { grpcErrorMessage =
            case grpcErrorMessage of
              Nothing -> Just context
              Just msg -> Just $ context <> ": " <> msg
        }

-- | Like 'withProcessTerm', but sends first SIGINT and only SIGTERM if not
-- stopped within 5 seconds.
withProcessInterrupt ::
  (MonadIO m, MonadThrow m) =>
  ProcessConfig stdin stdout stderr ->
  (Process stdin stdout stderr -> m a) ->
  m a
withProcessInterrupt config =
  bracket (startProcess $ config & setCreateGroup True) signalAndStopProcess
 where
  signalAndStopProcess :: MonadIO m => Process stdin stdout stderr -> m ()
  signalAndStopProcess p = liftIO $ do
    interruptProcessGroupOf (unsafeProcessHandle p)
    raceLabelled_
      ("etcd-signalAndStopProcess-waitExitCode", void $ waitExitCode p)
      ("etcd-signalAndStopProcess-stopProcess", threadDelay 5 >> stopProcess p)

-- * Persistent queue

-- | Queue elements carry the item's CBOR serialization, produced once on
-- write and reused for both the on-disk file and the etcd value, so
-- broadcasting does not serialize twice.
data PersistentQueue m a = PersistentQueue
  { queue :: TBQueue m (Natural, a, ByteString)
  , nextIx :: TVar m Natural
  , directory :: FilePath
  }

-- | Create a new persistent queue at file path and given capacity.
newPersistentQueue ::
  (MonadLabelledSTM m, MonadIO m, FromCBOR a, MonadCatch m, MonadFail m) =>
  Tracer IO EtcdLog ->
  FilePath ->
  Natural ->
  m (PersistentQueue m a)
newPersistentQueue tracer path capacity = do
  paths <- liftIO $ do
    createDirectoryIfMissing True path
    sort . mapMaybe readMaybe <$> listDirectory path
  queue <- newLabelledTBQueueIO "persistent-queue" $ max (fromIntegral $ length paths) capacity
  highestId <-
    try (loadExisting queue paths) >>= \case
      Left (e :: IOException) -> do
        liftIO $ do
          traceWith tracer PersistentQueueLoadFailed{reason = show e}
          createDirectoryIfMissing True path
        pure 0
      Right highest -> pure highest
  nextIx <- newLabelledTVarIO "persistent-next-ix" $ highestId + 1
  pure PersistentQueue{queue, nextIx, directory = path}
 where
  loadExisting queue = \case
    [] -> pure 0
    idxs -> do
      forM_ idxs $ \(idx :: Natural) -> do
        bs <- readFileBS (path </> show idx)
        case decodeFull' bs of
          Left err ->
            fail $ "Failed to decode item: " <> show err
          Right item ->
            atomically $ writeTBQueue queue (idx, item, bs)
      pure $ List.last idxs

-- | Write a value to the queue, blocking if the queue is full.
writePersistentQueue :: (ToCBOR a, MonadSTM m, MonadIO m) => Tracer IO EtcdLog -> PersistentQueue m a -> a -> m ()
writePersistentQueue tracer PersistentQueue{queue, nextIx, directory} item = do
  next <- atomically $ do
    next <- readTVar nextIx
    modifyTVar' nextIx (+ 1)
    pure next
  let !bytes = serialize' item
  writeFileBS (directory </> show next) bytes
  full <- atomically $ isFullTBQueue queue
  when full $ liftIO $ traceWith tracer PersistentQueueFull
  atomically $ writeTBQueue queue (next, item, bytes)

-- | Get the next value from the queue without removing it, blocking if the
-- queue is empty.
peekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m a
peekPersistentQueue PersistentQueue{queue} = do
  (\(_, item, _) -> item) <$> atomically (peekTBQueue queue)

-- | Like 'peekPersistentQueue', but returns 'Nothing' instead of blocking
-- when the queue is empty.
tryPeekPersistentQueue :: MonadSTM m => PersistentQueue m a -> m (Maybe a)
tryPeekPersistentQueue PersistentQueue{queue} = do
  fmap (\(_, item, _) -> item) <$> atomically (tryPeekTBQueue queue)

-- | Get all pending values and their serializations, up to the given count
-- and total byte limits, blocking until at least one is available. Values
-- are not removed; use 'popBatchPersistentQueue' after they were sent.
peekBatchPersistentQueue :: MonadSTM m => PersistentQueue m a -> Int -> Int -> m [(a, ByteString)]
peekBatchPersistentQueue PersistentQueue{queue} maxCount maxBytes = atomically $ do
  first' <- readTBQueue queue
  -- Collected in reverse consumption order
  rest <- go (remainingAfter first') []
  -- Restore everything we consumed: 'unGetTBQueue' pushes to the front, so
  -- restoring newest-first re-establishes the original queue order.
  forM_ (rest <> [first']) $ unGetTBQueue queue
  pure $ (\(_, item, bytes) -> (item, bytes)) <$> (first' : reverse rest)
 where
  go budget acc
    | length acc >= maxCount - 1 = pure acc
    | otherwise =
        tryReadTBQueue queue >>= \case
          Nothing -> pure acc
          Just next@(_, _, bytes)
            | BS.length bytes > budget -> do
                unGetTBQueue queue next
                pure acc
            | otherwise -> go (budget - BS.length bytes) (next : acc)

  remainingAfter (_, _, bytes) = maxBytes - BS.length bytes

-- | Get the batch to broadcast next: the batch already in flight if there is
-- one, otherwise a fresh one peeked from the queue (and recorded as in
-- flight). Returns 'Nothing' when nothing is pending. The caller must clear
-- the in-flight var after popping a successfully sent batch.
--
-- Pinning the in-flight batch across transient retries matters for the
-- compare-fail dedup in 'putMessage': a put can commit server-side while the
-- client sees e.g. 'GrpcDeadlineExceeded'. The retry must send (and
-- afterwards pop) exactly the content of the committed attempt. Re-peeking
-- on retry could pick up messages enqueued in the meantime; the dedup branch
-- would then declare the grown batch delivered and the never-sent tail would
-- be popped and lost.
nextPendingBatch ::
  MonadSTM m =>
  TVar m (Maybe [(a, ByteString)]) ->
  PersistentQueue m a ->
  Int ->
  Int ->
  m (Maybe [(a, ByteString)])
nextPendingBatch inFlightVar queue maxCount maxBytes =
  readTVarIO inFlightVar >>= \case
    Just batch -> pure (Just batch)
    Nothing ->
      tryPeekPersistentQueue queue >>= \case
        Nothing -> pure Nothing
        Just _ -> do
          -- All pending messages (bounded by the given limits) are sent as a
          -- single etcd value, so a whole batch costs one Raft commit
          -- instead of one per message.
          batch <- peekBatchPersistentQueue queue maxCount maxBytes
          atomically $ writeTVar inFlightVar (Just batch)
          pure (Just batch)

-- | Remove a batch previously returned by 'peekBatchPersistentQueue'. Pops
-- unconditionally, one item per batch entry: this thread is the sole
-- consumer, so the queue head still holds exactly the peeked items (an
-- item-matching guard could only silently no-op and wedge the queue, see
-- #2742).
popBatchPersistentQueue :: (MonadSTM m, MonadIO m) => Tracer IO EtcdLog -> PersistentQueue m a -> [(a, ByteString)] -> m ()
popBatchPersistentQueue tracer PersistentQueue{queue, directory} batch = do
  indices <- atomically $ forM batch $ \_ -> (\(ix, _, _) -> ix) <$> readTBQueue queue
  forM_ indices $ removeQueueFile tracer directory

-- | Delete the backing file of a popped queue item. Failing to delete is
-- traced but not fatal: the message was already broadcast, so a leftover
-- file only means it may be re-broadcast after a restart (at-least-once
-- delivery, same as the crash-recovery path).
removeQueueFile :: MonadIO m => Tracer IO EtcdLog -> FilePath -> Natural -> m ()
removeQueueFile tracer directory ix =
  liftIO $
    removeFile (directory </> show ix) `catch` \e ->
      unless (isDoesNotExistError e) $
        traceWith tracer PersistentQueueDeleteFailed{index = ix, reason = show e}

-- * Tracing

data EtcdLog
  = EtcdLog {etcd :: Value}
  | Reconnecting
  | BroadcastFailed {reason :: Text}
  | FailedToDecodeLog {log :: Text, reason :: Text}
  | FailedToDecodeValue {key :: Text, value :: Text, reason :: Text}
  | CreatedLease {leaseId :: Int64}
  | LowLeaseTTL {ttlRemaining :: Int64}
  | NoKeepAliveResponse
  | MatchingProtocolVersion {version :: ProtocolVersion}
  | WatchMessagesStartRevision {startRevision :: Int64}
  | WatchMessagesFallbackTo {compactRevision :: Int64}
  | -- | The watch stream failed; it is restarted from the last known revision.
    WatchFailed {reason :: Text}
  | -- | The etcd transaction wrapping a broadcast 'put' found that our
    -- key's @mod_revision@ had moved past what we last recorded — the
    -- expected outcome when a 'GrpcDeadlineExceeded'-retried put already
    -- committed server-side. No second put was issued.
    BroadcastDeduped {previousModRev :: Int64, observedModRev :: Int64}
  | -- | Failed to load persisted queue items from disk on startup. The queue
    -- starts empty; any in-flight messages from before the crash are lost.
    PersistentQueueLoadFailed {reason :: Text}
  | -- | The persistent queue has reached capacity. The calling thread will
    -- block until the broadcast loop drains at least one item.
    PersistentQueueFull
  | -- | Failed to delete the backing file of an already-broadcast item. The
    -- queue keeps operating; the leftover file only means the message may be
    -- re-broadcast after a restart.
    PersistentQueueDeleteFailed {index :: Natural, reason :: Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)
