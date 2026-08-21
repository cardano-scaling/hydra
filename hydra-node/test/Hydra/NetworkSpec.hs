{-# LANGUAGE OverloadedRecordDot #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Cardano.Binary (serialize')
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  readTBQueue,
  readTQueue,
  readTVarIO,
  writeTBQueue,
  writeTQueue,
 )
import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (Envelope (message), showLogsOnFailure, traceInTVar)
import Hydra.Network (
  Connectivity (..),
  Host (..),
  Network (..),
  NetworkCallback (..),
  ProtocolVersion (..),
  WhichEtcd (..),
 )
import Hydra.Network.Etcd (EtcdLog (..), LastKnownRevisionException (..), batchValue, connParams, getClientPort, getLastKnownRevision, grpcServer, isTransientGrpcError, peerPortToClientPort, putLastKnownRevision, putMessage, queryInitialModRev, retryableEtcdError, withEtcdNetwork)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import Network.GRPC.Client (Address (..), Server (..), ServerDisconnected (..), withConnection)
import Network.GRPC.Common (GrpcError (..), GrpcException (..))
import Network.HTTP2.Client (ErrorCode (..), HTTP2Error (..))
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  bind,
  close,
  connect,
  defaultProtocol,
  setSocketOption,
  socket,
  socketPort,
  tupleToHostAddress,
 )
import Network.Socket qualified as Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.IO.Error (userError)
import System.Process.Typed (readProcessStdout_, runProcess_, shell)
import Test.Aeson.GenericSpecs (Settings (..), defaultSettings, roundtripAndGoldenADTSpecsWithSettings)
import Test.Hydra.Ledger.Simple ()
import Test.Hydra.Network.Message ()
import Test.Hydra.Node.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Network.Ports (randomUnusedTCPPortsWithDerived, withFreePortAndDerived)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Util (noopCallback, waitEq, waitMatch)

spec :: Spec
spec = do
  -- TODO: add tests about advertise being honored
  --
  -- Per-test 'failAfter' budgets in this block are deliberately generous
  -- (60s). The previous 15–30s budgets fired too eagerly when an etcd
  -- election or a gRPC reconnect happened to land on the same scheduler
  -- tick as a CI slowdown. The 'network-test.yaml' workflow (pumba
  -- packet loss) is the operating point this guards against; tighten
  -- only if you've confirmed it still passes there.
  describe "isTransientGrpcError" $ do
    -- The loss-90 CI failure traced back to 'pollConnectivity.writeAlive'
    -- raising 'GrpcNotFound' when etcd's RAFT leader changed and the lease
    -- was revoked. The error must be classified as transient so the outer
    -- loop can recreate the lease instead of letting the exception escape
    -- and crash the hydra-node process.
    it "treats GrpcNotFound as transient (lease loss during leader change)" $
      isTransientGrpcError GrpcNotFound `shouldBe` True
    it "still treats the original transient errors as transient" $ do
      isTransientGrpcError GrpcUnavailable `shouldBe` True
      isTransientGrpcError GrpcDeadlineExceeded `shouldBe` True
      isTransientGrpcError GrpcCancelled `shouldBe` True
    it "does not treat unrelated errors as transient" $ do
      isTransientGrpcError GrpcInvalidArgument `shouldBe` False
      isTransientGrpcError GrpcPermissionDenied `shouldBe` False

  describe "retryableEtcdError" $ do
    -- #2817: the SETTINGS rate limit kills the connection with a bare
    -- 'HTTP2Error', which is not a 'GrpcException' and so escaped every handler
    -- in the module and took the node down.
    it "retries http2 connection errors" $
      retryableEtcdError (toException $ ConnectionErrorIsSent EnhanceYourCalm 0 "too many settings")
        `shouldSatisfy` isJust
    it "retries a connection lost under an in-flight call" $
      retryableEtcdError (toException $ ServerDisconnected (toException ConnectionIsClosed) callStack)
        `shouldSatisfy` isJust
    it "retries transient grpc errors" $
      retryableEtcdError (toException GrpcException{grpcError = GrpcUnavailable, grpcErrorMessage = Just "etcd is electing", grpcErrorDetails = Nothing, grpcErrorMetadata = []})
        `shouldBe` Just "etcd is electing"
    it "escalates other grpc errors" $
      retryableEtcdError (toException GrpcException{grpcError = GrpcInvalidArgument, grpcErrorMessage = Nothing, grpcErrorDetails = Nothing, grpcErrorMetadata = []})
        `shouldBe` Nothing
    -- 'putMessage' fails this way when etcd has lost the key we wrote against;
    -- that has to keep taking the node down.
    it "escalates anything else" $
      retryableEtcdError (toException $ userError "our broadcast key has no current value in etcd")
        `shouldBe` Nothing
    -- Retrying cannot make a bad file readable, so this must escalate rather
    -- than spin in 'waitMessages'.
    it "escalates an unusable last known revision" $
      retryableEtcdError (toException $ InvalidLastKnownRevision "peer/last-known-revision")
        `shouldBe` Nothing

  -- A file that holds no revision must not silently become revision 0: that
  -- rewinds the watch to the start of history. It takes the node down instead,
  -- so the error has to say how to recover.
  describe "last known revision" $ do
    it "round-trips a revision" $
      withTempDir "test-etcd-revision" $ \tmp -> do
        putLastKnownRevision tmp 42
        getLastKnownRevision tmp `shouldReturn` 42

    it "starts from scratch when there is no file" $
      withTempDir "test-etcd-revision" $ \tmp ->
        getLastKnownRevision tmp `shouldReturn` 0

    it "rejects a file that holds no revision" $
      withTempDir "test-etcd-revision" $ \tmp -> do
        let file = tmp </> "last-known-revision"
        writeFileBS file ""
        let isInvalid = \case
              InvalidLastKnownRevision f -> f == file
              UnreadableLastKnownRevision{} -> False
        getLastKnownRevision tmp `shouldThrow` isInvalid

    it "says how to recover from a file that holds no revision" $
      withTempDir "test-etcd-revision" $ \tmp -> do
        let file = tmp </> "last-known-revision"
        writeFileBS file "not a revision"
        try (getLastKnownRevision tmp) >>= \case
          Right (rev :: Natural) -> failure $ "expected a failure, got revision " <> show rev
          Left (e :: LastKnownRevisionException) ->
            displayException e `shouldContain` ("Delete " <> file)

  describe "Serialisation" $ do
    prop "can roundtrip CBOR encoding/decoding of Hydra Message" $ prop_canRoundtripCBOREncoding @(Message SimpleTx)

    roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(Message SimpleTx)

-- | The etcd tests, kept out of 'spec' so that test/Main.hs can turn them
-- into a tasty group of their own and run them one at a time.
--
-- They must not run concurrently: each starts etcd on a port obtained by
-- binding a socket, reading the assigned port and closing it again, so two at
-- once can be handed the same port, or one test's peer port can be another's
-- derived client port. Whichever etcd binds second dies with EADDRINUSE, which
-- 'withEtcdNetwork' turns straight into a failure.
etcdSpec :: Spec
etcdSpec =
  around (showLogsOnFailure "NetworkSpec") $ do
    let v1 = ProtocolVersion 1

    it "broadcasts to self" $ \tracer -> do
      failAfter 30 $
        withTempDir "test-etcd" $ \tmp -> do
          withFreePortAndDerived peerPortToClientPort $ \port -> do
            let config =
                  NetworkConfiguration
                    { listen = Host lo port
                    , advertise = Host lo port
                    , signingKey = aliceSk
                    , otherParties = []
                    , peers = []
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    , whichEtcd = SystemEtcd
                    }
            (recordingCallback, waitNext, _) <- newRecordingCallback
            withEtcdNetwork tracer v1 config recordingCallback $ \n -> do
              broadcast n ("asdf" :: Text)
              waitNext `shouldReturn` "asdf"

    -- Exercises 'putMessage's compare-failure branch directly. The
    -- scenario it mimics: a previous 'putMessage' committed
    -- server-side but the gRPC client returned
    -- 'GrpcDeadlineExceeded', so the in-memory 'lastModRev' is
    -- stale on the retry. The Txn's compare(modRev == stale) must
    -- fail, the range branch must observe the new revision, the
    -- 'BroadcastDeduped' event must be traced, and most importantly
    -- /no second write/ should land — so the receiver does not see
    -- a duplicate.
    it "putMessage dedups when lastModRev is stale" $ \tracer -> do
      failAfter 30 $
        withTempDir "test-etcd" $ \tmp -> do
          withFreePortAndDerived peerPortToClientPort $ \port -> do
            let host = Host lo port
                config =
                  NetworkConfiguration
                    { listen = host
                    , advertise = host
                    , signingKey = aliceSk
                    , otherParties = []
                    , peers = []
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    , whichEtcd = SystemEtcd
                    }
            (recordingCallback, waitNext, _) <- newRecordingCallback
            withEtcdNetwork @Int tracer v1 config recordingCallback $ \n -> do
              -- Real broadcast advances msg-<host>'s mod_revision.
              broadcast n 1
              waitNext `shouldReturn` 1
              -- Capture EtcdLog events from a direct putMessage call.
              traces <- newLabelledTVarIO "putMessage-dedup-traces" []
              let captureTracer = traceInTVar traces "putMessageDedupSpec"
              staleVar <- newLabelledTVarIO "stale-last-mod-rev" 0
              -- Compare against 0 must fail (real modRev > 0); the
              -- failure branch should adopt the observed revision
              -- and trace BroadcastDeduped instead of writing.
              withConnection (connParams captureTracer Nothing) (grpcServer config) $ \conn ->
                putMessage captureTracer conn host staleVar (batchValue [serialize' (99 :: Int)])
              captured <- map message <$> readTVarIO traces
              captured
                `shouldSatisfy` any
                  ( \case
                      BroadcastDeduped{} -> True
                      _ -> False
                  )
              updatedModRev <- readTVarIO staleVar
              updatedModRev `shouldSatisfy` (> 0)
              -- Send a fresh marker. If 99 had actually been
              -- written by the deduped call, the receiver would
              -- see it before 2 (etcd revisions are monotonic and
              -- the watch is in-order).
              broadcast n 2
              waitNext `shouldReturn` 2

    -- Regression test for #2817. etcd's grpc-go server raises its receive
    -- window as inbound volume grows, emitting a SETTINGS frame per step, and
    -- 'http2' rate limits inbound non-ACK SETTINGS to 4/s per connection
    -- (CVE-2019-9515), killing the connection on the 5th. Under real load the
    -- ramp is walked gradually as the broadcast queue backs up, which is why
    -- it only bit some runs; here the burst is injected rather than provoked,
    -- see 'withSettingsBurstProxy'.
    it "survives a burst of SETTINGS frames from etcd" $ \tracer -> do
      failAfter 60 $
        withTempDir "test-etcd" $ \tmp -> do
          withFreePortAndDerived peerPortToClientPort $ \port -> do
            let host = Host lo port
                config =
                  NetworkConfiguration
                    { listen = host
                    , advertise = host
                    , signingKey = aliceSk
                    , otherParties = []
                    , peers = []
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    , whichEtcd = SystemEtcd
                    }
            withEtcdNetwork @Text tracer v1 config noopCallback $ \_ ->
              -- One more than http2's limit of 4/s.
              withSettingsBurstProxy (getClientPort config) 5 $ \proxyPort settingsSeen -> do
                -- 'withEtcdNetwork' returns before etcd accepts clients, so
                -- gate on a direct query, whose reconnect policy waits.
                -- Doubles as the 'lastModRev' seed.
                lastModRevVar <-
                  newLabelledTVarIO "settings-burst-last-mod-rev"
                    =<< queryInitialModRev tracer config host
                let proxied =
                      ServerInsecure
                        Address
                          { addressHost = lo
                          , addressPort = proxyPort
                          , addressAuthority = Nothing
                          }
                    -- Big enough that the burst lands while the put is still
                    -- streaming, which is when it arrives in production.
                    value = batchValue [serialize' $ T.replicate (512 * 1024) "a"]
                withConnection (connParams tracer Nothing) proxied $ \conn ->
                  putMessage tracer conn host lastModRevVar value
                -- Not a vacuous pass: 5 injected plus etcd's own handshake
                -- frame, and possibly more from its window ramp.
                settingsSeen >>= (`shouldSatisfy` (>= 6))

    -- Note: This test is disabled as it takes took long; but it is
    -- important to keep around. Successfully completion of this test looks
    -- like either a "mvcc database size exceeded" error; or no error at
    -- all. Failures looks like complete blocking
    around_ onlyLocal $ xit "broadcasts 100KiB messages 1M times" $ \tracer ->
      withTempDir "test-etcd" $ \tmp -> do
        putStrLn $ "Folder " ++ show tmp
        PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
        (recordReceived, waitNext, _) <- newRecordingCallback
        -- Create a 100KiB message (100 * 1024 characters)
        let largeMessage = toText $ replicate (100 * 1024) 'a'
        withEtcdNetwork @Text tracer v1 aliceConfig recordReceived $ \n1 -> do
          withEtcdNetwork @Text tracer v1 bobConfig noopCallback $ \_ -> do
            forM_ [1 :: Integer .. 1000000] $ \i -> do
              let msgWithId = largeMessage <> " - Message #" <> show i
              when (i `mod` 10000 == 0) $
                putStrLn $
                  "Broadcasting 100KiB message #" <> show i <> " (size: " <> show (length (toString msgWithId)) <> " chars)"
              broadcast n1 msgWithId
              _ <- waitNext
              threadDelay 0.02

    it "broadcasts messages to single connected peer" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 30 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 -> do
            (recordReceived, waitNext, _) <- newRecordingCallback
            withEtcdNetwork @Int tracer v1 bobConfig recordReceived $ \_n2 -> do
              broadcast n1 123
              waitNext `shouldReturn` 123

    it "handles broadcast to minority" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          (recordReceived, waitNext, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig recordReceived $ \n1 -> do
            -- Bob and carol start and stop
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              withEtcdNetwork @Int tracer v1 carolConfig noopCallback $ \_ -> do
                pure ()
            -- Alice sends a message while she is the only one online (= minority)
            broadcast n1 123
          -- Now, alice stops too!
          -- Start alice, bob and carol again
          withEtcdNetwork @Int tracer v1 aliceConfig recordReceived $ \_ -> do
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              withEtcdNetwork @Int tracer v1 carolConfig noopCallback $ \_ -> do
                -- Alice should see her own message eventually (when part of majority again)
                waitNext `shouldReturn` 123

    it "handles broadcast to majority" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          (recordReceived, waitNext, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 ->
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              withEtcdNetwork @Int tracer v1 carolConfig recordReceived $ \_ -> do
                -- Alice sends a message while Carol is online
                broadcast n1 123
                waitNext `shouldReturn` 123
              -- Alice sends a message while Carol is offline
              broadcast n1 456
              -- Carol starts again
              withEtcdNetwork @Int tracer v1 carolConfig recordReceived $ \_ -> do
                -- Carol should receive messages sent by alice while offline
                -- (without duplication of 123)
                waitNext `shouldReturn` 456

    it "emits connectivity events" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          -- Record and assert connectivity events from alice's perspective
          (recordReceived, _, waitConnectivity) <- newRecordingCallback
          let
            waitFor :: HasCallStack => Connectivity -> IO ()
            waitFor = waitEq waitConnectivity 60
          withEtcdNetwork @Int tracer v1 aliceConfig recordReceived $ \_ -> do
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              -- Alice now on majority cluster
              waitFor NetworkConnected
              waitFor $ PeerConnected bobConfig.advertise
              withEtcdNetwork @Int tracer v1 carolConfig noopCallback $ \_ -> do
                waitFor $ PeerConnected carolConfig.advertise
                -- Carol stops
                pure ()
              waitFor $ PeerDisconnected carolConfig.advertise
              -- Bob stops
              pure ()
            -- We are now in minority
            waitFor NetworkDisconnected
            -- Carol starts again and we reach a majority
            withEtcdNetwork @Int tracer v1 carolConfig noopCallback $ \_ -> do
              waitFor NetworkConnected
              waitFor $ PeerConnected carolConfig.advertise

    it "handles expired lease" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 30 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          -- Record and assert connectivity events from alice's perspective
          (recordReceived, _, waitConnectivity) <- newRecordingCallback
          let
            waitFor :: HasCallStack => Connectivity -> IO ()
            waitFor = waitEq waitConnectivity 60
          withEtcdNetwork @Int tracer v1 aliceConfig recordReceived $ \_ -> do
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              waitFor NetworkConnected
              waitFor $ PeerConnected bobConfig.advertise
              -- Expire all leases manually to simulate a keepAlive coming too
              -- late. Note that we do not distinguish which is which so
              -- alice's lease will also be killed, but does not matter here.
              -- NOTE: etcdctl talks to the etcd /client/ port, not the peer
              -- port. Using @listen aliceConfig@ here used to work on etcd
              -- 3.5 (whose peer listener happened to answer client RPCs too)
              -- but hangs on etcd 3.6, where the peer port no longer serves
              -- the lease API.
              let endpoints = "--endpoints=127.0.0.1:" <> show (getClientPort aliceConfig)
              output <- readProcessStdout_ . shell $ "etcdctl lease list " <> endpoints
              let leases = drop 1 $ lines $ decodeUtf8 output
              forM_ leases $ \lease ->
                runProcess_ . shell $ "etcdctl lease revoke " <> endpoints <> " " <> toString lease
              -- Alice sees bob disconnected and connected again
              waitFor $ PeerConnected bobConfig.advertise

    it "checks protocol version" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          let v2 = ProtocolVersion 2
          (recordAlice, _, waitAlice) <- newRecordingCallback
          (recordBob, _, waitBob) <- newRecordingCallback
          let aliceSees = waitEq waitAlice 30
              bobSees = waitEq waitBob 30
          withEtcdNetwork @Int tracer v1 aliceConfig recordAlice $ \_ -> do
            withEtcdNetwork @Int tracer v2 bobConfig recordBob $ \_ -> do
              -- Both will try to write to the cluster at the same time
              -- Hence, either one or the other will see the mismatch
              raceLabelled_
                ("alice-sees", aliceSees VersionMismatch{ourVersion = v1, theirVersion = Just v2})
                ("bob-sees", bobSees VersionMismatch{ourVersion = v2, theirVersion = Just v1})

    it "resends messages" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        -- Sends 1000 messages through a 3-node etcd cluster; the
        -- 20s budget was too tight under parallel CI load.
        failAfter 60 $ do
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          (recordBob, waitBob, _) <- newRecordingCallback
          (recordCarol, waitCarol, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 ->
            withEtcdNetwork @Int tracer v1 bobConfig recordBob $ \_ -> do
              let messages = [1 .. 1000]
              -- Bob should see messages as we go
              forM_ messages $ \msg -> do
                broadcast n1 msg
                waitBob `shouldReturn` msg
              -- Carol only starts now and should see all messages delivered
              withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                forM_ messages $ \msg ->
                  waitCarol `shouldReturn` msg
              -- Carol only delivers new messages even after restart
              withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                broadcast n1 1001
                waitCarol `shouldReturn` 1001

    -- Sequential broadcasts force one etcd put each (batching only kicks
    -- in when the queue backs up), so 5000 sends cross the broadcast
    -- connection recycle boundary several times. Guards against the
    -- long-lived connection blocking (issue #2167) with the reused
    -- connection. Takes a few minutes, hence local-only.
    around_ onlyLocal $ it "sustains sequential broadcasts across connection recycles" $ \tracer ->
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 600 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          (recordBob, waitBob, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 ->
            withEtcdNetwork @Int tracer v1 bobConfig recordBob $ \_ ->
              forM_ [1 .. 5000 :: Int] $ \msg -> do
                broadcast n1 msg
                waitBob `shouldReturn` msg

    it "batches queued messages and delivers them in order" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          (recordBob, waitBob, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 ->
            withEtcdNetwork @Int tracer v1 bobConfig recordBob $ \_ -> do
              -- Broadcast without waiting in between, so the sender's queue
              -- accumulates and messages travel as multi-message batch
              -- values. Delivery must still be in order, exactly once.
              forM_ [1 .. 200] $ broadcast n1
              forM_ [1 .. 200] $ \msg -> waitBob `shouldReturn` msg

    it "delivers legacy single-message values" $ \tracer -> do
      failAfter 30 $
        withTempDir "test-etcd" $ \tmp -> do
          withFreePortAndDerived peerPortToClientPort $ \port -> do
            let host = Host lo port
                config =
                  NetworkConfiguration
                    { listen = host
                    , advertise = host
                    , signingKey = aliceSk
                    , otherParties = []
                    , peers = []
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    , whichEtcd = SystemEtcd
                    }
            (recordingCallback, waitNext, _) <- newRecordingCallback
            withEtcdNetwork @Int tracer v1 config recordingCallback $ \n -> do
              broadcast n 1
              waitNext `shouldReturn` 1
              -- Write a value in the pre-batching wire format (a single
              -- CBOR message, not a list); the watch must deliver it
              -- through the legacy fallback decoder.
              rev <- queryInitialModRev tracer config host
              revVar <- newLabelledTVarIO "legacy-value-mod-rev" rev
              withConnection (connParams tracer Nothing) (grpcServer config) $ \conn ->
                putMessage tracer conn host revVar (serialize' (7 :: Int))
              waitNext `shouldReturn` 7

    it "handles compaction and lost local state" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          (recordBob, waitBob, _) <- newRecordingCallback
          (recordCarol, waitCarol, _) <- newRecordingCallback
          withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 ->
            withEtcdNetwork @Int tracer v1 bobConfig recordBob $ \_ -> do
              -- First we send 5 messages with carol online
              withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                forM_ [1 .. 5] $ \msg -> do
                  broadcast n1 msg
                  waitBob `shouldReturn` msg
                  waitCarol `shouldReturn` msg
              -- Carol stopped and we continue sending messages
              forM_ [5 .. 100] $ \msg -> do
                broadcast n1 msg
                waitBob `shouldReturn` msg
              -- Even while carol is down, the etcd component would
              -- "auto-compact" messages. By default down to 1000 messages
              -- after/every 5 minutes. This is interesting as it should
              -- result in carol never some messages, but is hard to test
              -- (without waiting 5 minutes). Instead we issue a direct etcd
              -- command to compact everything before revision 50.
              runProcess_ . shell $
                "etcdctl compact 50 --endpoints=127.0.0.1:" <> show (getClientPort aliceConfig)
              -- When carol starts now we would expect it to start catching up
              -- from the earliest possible revision 50. While missing some
              -- messages.
              withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                -- NOTE: Revision 50 may not correspond to message 50, so we
                -- only assert its some message bigger than 25 and expect to
                -- see all further messages to 100.
                firstMsg <- waitCarol
                firstMsg `shouldSatisfy` (> 25)
                forM_ [firstMsg + 1 .. 100] $ \msg ->
                  waitCarol `shouldReturn` msg
                -- Carol should be able to receive new messages just fine.
                forM_ [101 .. 105] $ \msg -> do
                  broadcast n1 msg
                  waitCarol `shouldReturn` msg
              -- Similarly, should carol lose its local state, we expect it to
              -- see everything from the last compacted revision 50. We can
              -- enforce this by removing the corresponding file (an internal
              -- implementation detail)
              removeFile (persistenceDir carolConfig </> "last-known-revision")
              withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                -- NOTE: Revision 50 may not correspond to message 50, so we
                -- only assert its some message bigger than 25 and expect to
                -- see all further messages to 105.
                firstMsg <- waitCarol
                firstMsg `shouldSatisfy` (> 25)
                forM_ [firstMsg + 1 .. 105] $ \msg -> do
                  waitCarol `shouldReturn` msg

    it "emits cluster id mismatch" $ \tracer -> do
      withTempDir "test-etcd" $ \tmp -> do
        failAfter 60 $ do
          PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
          let v2 = ProtocolVersion 2
          (recordAlice, _, waitAlice) <- newRecordingCallback
          (recordBob, _, waitBob) <- newRecordingCallback
          let aliceSees = waitMatch waitAlice 30
          let bobSees = waitMatch waitBob 30
          let bobConfig' = bobConfig{peers = []}
          withEtcdNetwork @Int tracer v1 aliceConfig recordAlice $ \_ ->
            withEtcdNetwork @Int tracer v2 bobConfig' recordBob $ \_ ->
              raceLabelled_
                ("bob-sees", bobSees $ \case ClusterIDMismatch{} -> Just (); _ -> Nothing)
                ("alice-sees", aliceSees $ \case ClusterIDMismatch{} -> Just (); _ -> Nothing)

lo :: IsString s => s
lo = "127.0.0.1"

-- | Run a TCP proxy in front of an etcd client port that reproduces #2817's
-- SETTINGS burst deterministically.
--
-- Both directions are forwarded verbatim, and once the client starts sending
-- request data (so an RPC is in flight, as in production, where the data being
-- sent is what makes etcd's window ramp fire) the proxy pushes @burst@ empty
-- SETTINGS frames at the client. Those are legal at any point; the client ACKs
-- each and etcd ignores stray ACKs.
--
-- Injecting beats provoking: how many frames grpc-go's estimator emits depends
-- on how much data lands in one round-trip, which on an idle machine stops at
-- 3, one under the limit. Reaching 5 needs the scheduling latency of the
-- reporter's six-nodes-on-one-host setup.
withSettingsBurstProxy ::
  -- | Upstream etcd client port.
  PortNumber ->
  -- | How many SETTINGS frames to burst at the client.
  Int ->
  -- | Given the proxy's port and the count of non-ACK SETTINGS frames
  -- forwarded to the client so far.
  (PortNumber -> IO Int -> IO a) ->
  IO a
withSettingsBurstProxy upstreamPort burst action = do
  settingsSeen <- newLabelledTVarIO "settings-burst-seen" 0
  bracket listenLoopback close $ \server -> do
    proxyPort <- socketPort server
    withAsyncLabelled ("settings-burst-proxy", acceptLoop settingsSeen server) $ \_ ->
      action proxyPort (readTVarIO settingsSeen)
 where
  loopback = tupleToHostAddress (127, 0, 0, 1)

  listenLoopback = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock $ SockAddrInet 0 loopback
    Socket.listen sock 5
    pure sock

  acceptLoop settingsSeen server = forever $ do
    (client, _) <- accept server
    -- Socket teardown at the end of a test races the relay threads; a dead
    -- proxy connection has nothing left to report, so let it go quietly.
    void . asyncLabelled "settings-burst-proxy-connection" $
      (void . try @_ @SomeException $ relay settingsSeen client) `finally` close client

  relay settingsSeen client =
    bracket connectUpstream close $ \upstream -> do
      -- Single writer towards the client, so forwarded bytes and the injected
      -- burst cannot interleave mid-frame.
      toClient <- newLabelledTBQueueIO "settings-burst-to-client" 100
      raceLabelled_
        ("settings-burst-writer", writeToClient settingsSeen toClient client "")
        ( "settings-burst-readers"
        , raceLabelled_
            ("settings-burst-upstream-reader", readInto upstream (atomically . writeTBQueue toClient))
            ("settings-burst-client-reader", readFromClient toClient client upstream 0)
        )

  connectUpstream = do
    sock <- socket AF_INET Stream defaultProtocol
    connect sock $ SockAddrInet upstreamPort loopback
    pure sock

  readInto from sink = do
    bs <- recv from chunkSize
    unless (BS.null bs) $ do
      sink bs
      readInto from sink

  writeToClient settingsSeen toClient client buffer = do
    bs <- atomically $ readTBQueue toClient
    sendAll client bs
    buffer' <- countSettingsFrames settingsSeen (buffer <> bs)
    writeToClient settingsSeen toClient client buffer'

  -- Trigger off bytes sent rather than parsing client frames: anything past the
  -- 24-byte preface and the SETTINGS + HEADERS frames is request body.
  readFromClient toClient client upstream sent = do
    bs <- recv client chunkSize
    unless (BS.null bs) $ do
      sendAll upstream bs
      let sent' = sent + BS.length bs
      when (sent < burstAfterBytes && sent' >= burstAfterBytes) $
        atomically . writeTBQueue toClient . BS.concat $
          replicate burst emptySettingsFrame
      readFromClient toClient client upstream sent'

  -- Walk whole HTTP/2 frames out of the buffer, counting the non-ACK SETTINGS
  -- ones, and hand back the trailing partial frame.
  countSettingsFrames settingsSeen buffer
    | BS.length buffer < frameHeaderSize = pure buffer
    | otherwise = do
        let frameSize = frameHeaderSize + BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 (BS.take 3 buffer)
            frameType = BS.index buffer 3
            flags = BS.index buffer 4
        if BS.length buffer < frameSize
          then pure buffer
          else do
            when (frameType == settingsFrameType && not (testBit flags 0)) $
              atomically $
                modifyTVar' settingsSeen (+ 1)
            countSettingsFrames settingsSeen (BS.drop frameSize buffer)

  -- SETTINGS, empty payload, no flags, stream 0.
  emptySettingsFrame = BS.pack [0, 0, 0, settingsFrameType, 0, 0, 0, 0, 0]

  settingsFrameType = 4 :: Word8

  frameHeaderSize = 9

  burstAfterBytes = 16 * 1024

  chunkSize = 65536

data PeerConfig2 = PeerConfig2
  { aliceConfig :: NetworkConfiguration
  , bobConfig :: NetworkConfiguration
  }

setup2Peers :: FilePath -> IO PeerConfig2
setup2Peers tmp = do
  -- Allocate peer ports whose derived etcd client ports are also free at
  -- allocation time — otherwise etcd dies on startup with "bind: address
  -- already in use" for the client port. See 'peerPortToClientPort'.
  [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPortsWithDerived peerPortToClientPort 2
  let aliceHost = Host lo port1
  let bobHost = Host lo port2
  pure
    PeerConfig2
      { aliceConfig =
          NetworkConfiguration
            { listen = Host lo port1
            , advertise = Host lo port1
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , peers = [bobHost]
            , nodeId = "alice"
            , persistenceDir = tmp </> "alice"
            , whichEtcd = SystemEtcd
            }
      , bobConfig =
          NetworkConfiguration
            { listen = Host lo port2
            , advertise = Host lo port2
            , signingKey = bobSk
            , otherParties = [alice, carol]
            , peers = [aliceHost]
            , nodeId = "bob"
            , persistenceDir = tmp </> "bob"
            , whichEtcd = SystemEtcd
            }
      }

data PeerConfig3 = PeerConfig3
  { aliceConfig :: NetworkConfiguration
  , bobConfig :: NetworkConfiguration
  , carolConfig :: NetworkConfiguration
  }

setup3Peers :: FilePath -> IO PeerConfig3
setup3Peers tmp = do
  -- See note in 'setup2Peers' about the derived client port.
  [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPortsWithDerived peerPortToClientPort 3
  let aliceHost = Host lo port1
  let bobHost = Host lo port2
  let carolHost = Host lo port3
  pure
    PeerConfig3
      { aliceConfig =
          NetworkConfiguration
            { listen = Host lo port1
            , advertise = Host lo port1
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , peers = [bobHost, carolHost]
            , nodeId = "alice"
            , persistenceDir = tmp </> "alice"
            , whichEtcd = SystemEtcd
            }
      , bobConfig =
          NetworkConfiguration
            { listen = Host lo port2
            , advertise = Host lo port2
            , signingKey = bobSk
            , otherParties = [alice, carol]
            , peers = [aliceHost, carolHost]
            , nodeId = "bob"
            , persistenceDir = tmp </> "bob"
            , whichEtcd = SystemEtcd
            }
      , carolConfig =
          NetworkConfiguration
            { listen = Host lo port3
            , advertise = Host lo port3
            , signingKey = carolSk
            , otherParties = [alice, bob]
            , peers = [aliceHost, bobHost]
            , nodeId = "carol"
            , persistenceDir = tmp </> "carol"
            , whichEtcd = SystemEtcd
            }
      }

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a

newRecordingCallback :: MonadLabelledSTM m => m (NetworkCallback msg m, m msg, m Connectivity)
newRecordingCallback = do
  received <- newLabelledTQueueIO "received"
  connectivity <- newLabelledTQueueIO "connectivity"
  pure
    ( NetworkCallback
        { deliver = atomically . writeTQueue received
        , onConnectivity = atomically . writeTQueue connectivity
        }
    , atomically $ readTQueue received
    , atomically $ readTQueue connectivity
    )
