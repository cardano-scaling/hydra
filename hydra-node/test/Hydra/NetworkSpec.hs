{-# LANGUAGE OverloadedRecordDot #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent.Class.MonadSTM (
  readTQueue,
  readTVarIO,
  writeTQueue,
 )
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
import Hydra.Network.Etcd (EtcdLog (..), getClientPort, peerPortToClientPort, putMessage, withEtcdNetwork)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import System.Directory (removeFile)
import System.FilePath ((</>))
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
  -- Each Etcd test spins up a 2- or 3-node etcd cluster on loopback ports.
  -- Running these in parallel (tasty's default on developer machines) means
  -- multiple subprocesses fight for ports and CPU at the same instant, which
  -- can cause failures. Force sequential execution within this describe;
  -- CI already pins '--num-threads=1' for the whole hydra-node
  -- suite, so this only changes behaviour for local runs.
  --
  -- Per-test 'failAfter' budgets in this block are deliberately generous
  -- (60s). The previous 15–30s budgets fired too eagerly when an etcd
  -- election or a gRPC reconnect happened to land on the same scheduler
  -- tick as a CI slowdown. The 'network-test.yaml' workflow (pumba
  -- packet loss) is the operating point this guards against; tighten
  -- only if you've confirmed it still passes there.
  describe "Etcd" . sequential $
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
                      , joinExistingCluster = False
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
                      , joinExistingCluster = False
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
                putMessage @Int captureTracer config host staleVar 99
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

  describe "Serialisation" $ do
    prop "can roundtrip CBOR encoding/decoding of Hydra Message" $ prop_canRoundtripCBOREncoding @(Message SimpleTx)

    roundtripAndGoldenADTSpecsWithSettings defaultSettings{sampleSize = 1} $ Proxy @(Message SimpleTx)

lo :: IsString s => s
lo = "127.0.0.1"

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
            , joinExistingCluster = False
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
            , joinExistingCluster = False
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
            , joinExistingCluster = False
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
            , joinExistingCluster = False
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
            , joinExistingCluster = False
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
