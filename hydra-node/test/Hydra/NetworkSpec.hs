-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent.Class.MonadSTM (
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
 )
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (nullTracer, showLogsOnFailure)
import Hydra.Network (Host (..), Network, NetworkCallback (..))
import Hydra.Network.Etcd (withEtcdNetwork)
import Hydra.Network.Message (
  HydraHandshakeRefused (..),
  HydraVersionedProtocolNumber (..),
  Message (..),
 )
import Hydra.Network.Ouroboros (HydraNetworkConfig (..), broadcast, withOuroborosNetwork)
import Hydra.Network.Reliability (MessagePersistence (..))
import Hydra.Node.Network (NetworkConfiguration (..), configureMessagePersistence)
import Hydra.Node.ParameterMismatch (ParameterMismatch)
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Node.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Network.Ports (randomUnusedTCPPorts, withFreePort)
import Test.QuickCheck (
  Property,
  (===),
 )
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = do
  -- TODO: Generalize tests corresponding to properties of network layer, e.g. validity
  describe "Etcd" $
    around (showLogsOnFailure "NetworkSpec") $ do
      it "broadcasts to self" $ \tracer -> do
        failAfter 5 $
          withTempDir "test-etcd" $ \tmp -> do
            withFreePort $ \port -> do
              let config =
                    NetworkConfiguration
                      { host = lo
                      , port = port
                      , signingKey = aliceSk
                      , otherParties = []
                      , peers = []
                      , nodeId = "alice"
                      , persistenceDir = tmp </> "alice"
                      }
              (recordingCallback, waitNext) <- newRecordingCallback
              withEtcdNetwork tracer config recordingCallback $ \n -> do
                broadcast n ("asdf" :: Text)
                waitNext `shouldReturn` "asdf"

      it "broadcasts messages to single connected peer" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 5 $ do
            [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
            let aliceConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port1
                    , signingKey = aliceSk
                    , otherParties = [bob]
                    , peers = [Host lo port2]
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    }
            let bobConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port2
                    , signingKey = bobSk
                    , otherParties = [alice]
                    , peers = [Host lo port1]
                    , nodeId = "bob"
                    , persistenceDir = tmp </> "bob"
                    }
            withEtcdNetwork @Int tracer aliceConfig noopCallback $ \n1 -> do
              (recordReceived, waitNext) <- newRecordingCallback
              withEtcdNetwork @Int tracer bobConfig recordReceived $ \_n2 -> do
                broadcast n1 123
                waitNext `shouldReturn` 123

      it "handles broadcast to minority" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 20 $ do
            [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
            let aliceConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port1
                    , signingKey = aliceSk
                    , otherParties = [bob, carol]
                    , peers = [Host lo port2, Host lo port3]
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    }
            let bobConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port2
                    , signingKey = bobSk
                    , otherParties = [alice, carol]
                    , peers = [Host lo port1, Host lo port3]
                    , nodeId = "bob"
                    , persistenceDir = tmp </> "bob"
                    }
            let carolConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port3
                    , signingKey = carolSk
                    , otherParties = [alice, bob]
                    , peers = [Host lo port1, Host lo port2]
                    , nodeId = "carol"
                    , persistenceDir = tmp </> "carol"
                    }
            (recordReceived, waitNext) <- newRecordingCallback
            withEtcdNetwork @Int tracer aliceConfig recordReceived $ \n1 -> do
              -- Bob and carol start and stop
              withEtcdNetwork @Int tracer bobConfig noopCallback $ \_ -> do
                withEtcdNetwork @Int tracer carolConfig noopCallback $ \_ -> do
                  pure ()
              -- Alice sends a message while she is the only one online (= minority)
              broadcast n1 123
            -- Now, alice stops too!
            -- Start alice, bob and carol again
            withEtcdNetwork @Int tracer aliceConfig recordReceived $ \_ -> do
              withEtcdNetwork @Int tracer bobConfig noopCallback $ \_ -> do
                withEtcdNetwork @Int tracer carolConfig noopCallback $ \_ -> do
                  -- Alice should see her own message eventually (when part of majority again)
                  waitNext `shouldReturn` 123

      it "handles broadcast to majority" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 20 $ do
            [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
            let aliceConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port1
                    , signingKey = aliceSk
                    , otherParties = [bob, carol]
                    , peers = [Host lo port2, Host lo port3]
                    , nodeId = "alice"
                    , persistenceDir = tmp </> "alice"
                    }
            let bobConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port2
                    , signingKey = bobSk
                    , otherParties = [alice, carol]
                    , peers = [Host lo port1, Host lo port3]
                    , nodeId = "bob"
                    , persistenceDir = tmp </> "bob"
                    }
            let carolConfig =
                  NetworkConfiguration
                    { host = lo
                    , port = port3
                    , signingKey = carolSk
                    , otherParties = [alice, bob]
                    , peers = [Host lo port1, Host lo port2]
                    , nodeId = "carol"
                    , persistenceDir = tmp </> "carol"
                    }
            (recordReceived, waitNext) <- newRecordingCallback
            withEtcdNetwork @Int tracer aliceConfig noopCallback $ \n1 ->
              withEtcdNetwork @Int tracer bobConfig noopCallback $ \_ -> do
                withEtcdNetwork @Int tracer carolConfig recordReceived $ \_ -> do
                  -- Alice sends a message while Carol is online
                  broadcast n1 123
                  waitNext `shouldReturn` 123
                -- Alice sends a message while Carol is offline
                broadcast n1 456
                -- Carol starts again
                withEtcdNetwork @Int tracer carolConfig recordReceived $ \_ -> do
                  -- Carol should receive messages sent by alice while offline
                  -- (without duplication of 123)
                  waitNext `shouldReturn` 456

  describe "Ouroboros Network" $ do
    around (showLogsOnFailure "NetworkSpec") $ do
      it "broadcasts messages to single connected peer" $ \tracer -> do
        received <- atomically newTQueue
        let recordReceived = NetworkCallback{deliver = atomically . writeTQueue received}
        failAfter 30 $ do
          [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
          let node1Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port1
                  , remoteHosts = [Host lo port2]
                  }
              node2Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port2
                  , remoteHosts = [Host lo port1]
                  }
          withOuroborosNetwork @Integer tracer node1Config (const $ pure ()) noopCallback $ \hn1 ->
            withOuroborosNetwork @Integer tracer node2Config (const $ pure ()) recordReceived $ \_ -> do
              withNodeBroadcastingForever hn1 1 $
                atomically (readTQueue received) `shouldReturn` 1

      it "handshake failures should call the handshakeCallback" $ \tracer -> do
        failAfter 30 $ do
          [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
          let node1Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port1
                  , remoteHosts = [Host lo port2]
                  }
              node2Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 1
                  , localHost = Host lo port2
                  , remoteHosts = [Host lo port1]
                  }
              createHandshakeCallback :: IO (HydraHandshakeRefused -> IO (), IO [Host])
              createHandshakeCallback = do
                x <- newTVarIO []
                let f (HydraHandshakeRefused{remoteHost}) = atomically $ modifyTVar' x (remoteHost :)
                let g = readTVarIO x
                pure (f, g)

          (handshakeCallback1, getHandshakeFailures1) <- createHandshakeCallback
          (handshakeCallback2, getHandshakeFailures2) <- createHandshakeCallback

          withOuroborosNetwork @Integer @Integer tracer node1Config handshakeCallback1 noopCallback $ \_ ->
            withOuroborosNetwork @Integer tracer node2Config handshakeCallback2 noopCallback $ \_ -> do
              threadDelay 0.1
              getHandshakeFailures1 `shouldReturn` [Host lo port2]
              getHandshakeFailures2 `shouldReturn` [Host lo port1]

      it "broadcasts messages between 3 connected peers" $ \tracer -> do
        node1received <- atomically newTQueue
        node2received <- atomically newTQueue
        node3received <- atomically newTQueue
        let recordReceivedIn tq = NetworkCallback{deliver = atomically . writeTQueue tq}
        failAfter 30 $ do
          [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
          let node1Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port1
                  , remoteHosts = [Host lo port2, Host lo port3]
                  }
              node2Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port2
                  , remoteHosts = [Host lo port1, Host lo port3]
                  }
              node3Config =
                HydraNetworkConfig
                  { protocolVersion = MkHydraVersionedProtocolNumber 0
                  , localHost = Host lo port3
                  , remoteHosts = [Host lo port2, Host lo port1]
                  }
          withOuroborosNetwork @Integer tracer node1Config (const $ pure ()) (recordReceivedIn node1received) $ \hn1 ->
            withOuroborosNetwork tracer node2Config (const $ pure ()) (recordReceivedIn node2received) $ \hn2 -> do
              withOuroborosNetwork tracer node3Config (const $ pure ()) (recordReceivedIn node3received) $ \hn3 -> do
                withNodesBroadcastingForever [(hn1, 1), (hn2, 2), (hn3, 3)] $
                  assertAllnodesReceivedMessagesFromAllOtherNodes [(node1received, 1), (node2received, 2), (node3received, 3)]

  describe "Serialisation" $ do
    prop "can roundtrip CBOR encoding/decoding of Hydra Message" $ prop_canRoundtripCBOREncoding @(Message SimpleTx)
    roundtripAndGoldenSpecs (Proxy @(Message SimpleTx))

  describe "configureMessagePersistence" $ do
    it "throws ParameterMismatch when configuring given number of acks does not match number of parties" $ do
      withTempDir "persistence" $ \dir -> do
        MessagePersistence{saveAcks} <- configureMessagePersistence @_ @Int nullTracer dir 3
        saveAcks (fromList [0, 0, 0])
        configureMessagePersistence @_ @Int nullTracer dir 4 `shouldThrow` (const True :: Selector ParameterMismatch)

lo :: IsString s => s
lo = "127.0.0.1"

withNodeBroadcastingForever :: Network IO Integer -> Integer -> IO b -> IO b
withNodeBroadcastingForever node value = withNodesBroadcastingForever [(node, value)]

withNodesBroadcastingForever :: [(Network IO Integer, Integer)] -> IO b -> IO b
withNodesBroadcastingForever [] continuation = continuation
withNodesBroadcastingForever ((node, value) : rest) continuation =
  withAsync
    (forever $ threadDelay 0.1 >> broadcast node value)
    $ \_ -> withNodesBroadcastingForever rest continuation

assertAllnodesReceivedMessagesFromAllOtherNodes :: [(TQueue IO Integer, Integer)] -> IO ()
assertAllnodesReceivedMessagesFromAllOtherNodes messagesFromNodes =
  sequence_ $
    [ shouldEventuallyReceive thisQueue otherValue
    | (thisQueue, thisValue) <- messagesFromNodes
    , (_otherQueue, otherValue) <- messagesFromNodes
    , otherValue /= thisValue
    ]

shouldEventuallyReceive :: TQueue IO Integer -> Integer -> Expectation
shouldEventuallyReceive queue expectedValue = do
  receivedValue <- atomically $ readTQueue queue
  unless (receivedValue == expectedValue) $ shouldEventuallyReceive queue expectedValue

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a

noopCallback :: Applicative m => NetworkCallback msg m
noopCallback = NetworkCallback{deliver = \_ -> pure ()}

newRecordingCallback :: MonadSTM m => m (NetworkCallback msg m, m msg)
newRecordingCallback = do
  received <- atomically newTQueue
  let recordReceived = NetworkCallback{deliver = atomically . writeTQueue received}
  pure
    ( recordReceived
    , atomically $ readTQueue received
    )
