{-# LANGUAGE OverloadedRecordDot #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent.Class.MonadSTM (
  newTQueue,
  readTQueue,
  writeTQueue,
 )
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (
  Connectivity (..),
  Host (..),
  Network (..),
  NetworkCallback (..),
  ProtocolVersion (..),
  WhichEtcd (..),
 )
import Hydra.Network.Etcd (withEtcdNetwork)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import System.Directory (removeFile)
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (Settings (..), defaultSettings, roundtripAndGoldenADTSpecsWithSettings)
import Test.Hydra.Node.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Network.Ports (randomUnusedTCPPorts, withFreePort)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Util (noopCallback, waitEq)

spec :: Spec
spec = do
  -- TODO: add tests about advertise being honored
  describe "Etcd" $
    around (showLogsOnFailure "NetworkSpec") $ do
      let v1 = ProtocolVersion 1

      it "broadcasts to self" $ \tracer -> do
        failAfter 5 $
          withTempDir "test-etcd" $ \tmp -> do
            withFreePort $ \port -> do
              let config =
                    NetworkConfiguration
                      { listen = Host lo port
                      , advertise = Host lo port
                      , signingKey = aliceSk
                      , otherParties = []
                      , peers = []
                      , nodeId = "alice"
                      , persistenceDir = tmp </> "alice"
                      , whichEtcd = EmbeddedEtcd
                      }
              (recordingCallback, waitNext, _) <- newRecordingCallback
              withEtcdNetwork tracer v1 config recordingCallback $ \n -> do
                broadcast n ("asdf" :: Text)
                waitNext `shouldReturn` "asdf"

      it "broadcasts messages to single connected peer" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 5 $ do
            PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
            withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 -> do
              (recordReceived, waitNext, _) <- newRecordingCallback
              withEtcdNetwork @Int tracer v1 bobConfig recordReceived $ \_n2 -> do
                broadcast n1 123
                waitNext `shouldReturn` 123

      it "handles broadcast to minority" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 20 $ do
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
          failAfter 20 $ do
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

      it "checks protocol version" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 10 $ do
            PeerConfig2{aliceConfig, bobConfig} <- setup2Peers tmp
            let v2 = ProtocolVersion 2
            (recordAlice, _, waitAlice) <- newRecordingCallback
            (recordBob, _, waitBob) <- newRecordingCallback
            let aliceSees = waitEq waitAlice 5
                bobSees = waitEq waitBob 5
            withEtcdNetwork @Int tracer v1 aliceConfig recordAlice $ \_ -> do
              withEtcdNetwork @Int tracer v2 bobConfig recordBob $ \_ -> do
                -- Both will try to write to the cluster at the same time
                -- Hence, either one or the other will see the mismatch
                race_
                  (aliceSees VersionMismatch{ourVersion = v1, theirVersion = Just v2})
                  (bobSees VersionMismatch{ourVersion = v2, theirVersion = Just v1})

      it "resends messages" $ \tracer -> do
        withTempDir "test-etcd" $ \tmp -> do
          failAfter 20 $ do
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
                -- We can reset the last known view (internal implementation detail)
                removeFile (persistenceDir carolConfig </> "last-known-revision")
                withEtcdNetwork @Int tracer v1 carolConfig recordCarol $ \_ -> do
                  -- NOTE: The etcd component would "auto-compact" messages down
                  -- to 1000 messages after 5 minutes. This would result in
                  -- starting at 1001 here, but is hard to test (without waiting
                  -- 5 minutes).
                  forM_ messages $ \msg ->
                    waitCarol `shouldReturn` msg

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
  [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
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
            , whichEtcd = EmbeddedEtcd
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
            , whichEtcd = EmbeddedEtcd
            }
      }

data PeerConfig3 = PeerConfig3
  { aliceConfig :: NetworkConfiguration
  , bobConfig :: NetworkConfiguration
  , carolConfig :: NetworkConfiguration
  }

setup3Peers :: FilePath -> IO PeerConfig3
setup3Peers tmp = do
  [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
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
            , whichEtcd = EmbeddedEtcd
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
            , whichEtcd = EmbeddedEtcd
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
            , whichEtcd = EmbeddedEtcd
            }
      }

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a

newRecordingCallback :: MonadSTM m => m (NetworkCallback msg m, m msg, m Connectivity)
newRecordingCallback = do
  received <- atomically newTQueue
  connectivity <- atomically newTQueue
  pure
    ( NetworkCallback
        { deliver = atomically . writeTQueue received
        , onConnectivity = atomically . writeTQueue connectivity
        }
    , atomically $ readTQueue received
    , atomically $ readTQueue connectivity
    )
