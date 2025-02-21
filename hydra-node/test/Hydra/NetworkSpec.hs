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
  HydraVersionedProtocolNumber (..),
  KnownHydraVersions (..),
  Network (..),
  NetworkCallback (..),
 )
import Hydra.Network.Etcd (withEtcdNetwork)
import Hydra.Network.Message (Message (..))
import Hydra.Node.Network (NetworkConfiguration (..))
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Node.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk)
import Test.Network.Ports (randomUnusedTCPPorts, withFreePort)
import Test.QuickCheck (Property, (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Util (waitEq)

spec :: Spec
spec = do
  -- TODO: Generalize tests corresponding to properties of network layer, e.g. validity
  describe "Etcd" $
    around (showLogsOnFailure "NetworkSpec") $ do
      let v1 = MkHydraVersionedProtocolNumber 1

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
              (recordingCallback, waitNext, _) <- newRecordingCallback
              withEtcdNetwork tracer v1 config recordingCallback $ \n -> do
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
          PeerConfig3{aliceConfig, bobConfig, carolConfig} <- setup3Peers tmp
          let bobHost = Host (show bobConfig.host) bobConfig.port
          let carolHost = Host (show carolConfig.host) carolConfig.port
          -- Record and assert connectivity events from alice's perspective
          (recordReceived, _, waitConnectivity) <- newRecordingCallback
          let
            waitFor :: HasCallStack => Connectivity -> IO ()
            waitFor = waitEq waitConnectivity 10
          withEtcdNetwork @Int tracer v1 aliceConfig recordReceived $ \_ -> do
            withEtcdNetwork @Int tracer v1 bobConfig noopCallback $ \_ -> do
              -- Alice now on majority cluster
              waitFor NetworkConnected
              waitFor $ Connected bobHost
              withEtcdNetwork @Int tracer v1 carolConfig noopCallback $ \_ -> do
                waitFor $ Connected carolHost
                -- Carol stops
                pure ()
              waitFor $ Disconnected carolHost
              -- Bob stops
              pure ()
            waitFor NetworkDisconnected

      it "checks protocol version" $ \tracer -> do
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
            let v2 = MkHydraVersionedProtocolNumber 2
            withEtcdNetwork @Int tracer v1 aliceConfig noopCallback $ \n1 -> do
              (recordReceived, _, waitConnectivity) <- newRecordingCallback
              withEtcdNetwork @Int tracer v2 bobConfig recordReceived $ \_n2 -> do
                broadcast n1 123

                waitEq waitConnectivity 10 $
                  HandshakeFailure
                    { remoteHost = Host "???" port1
                    , ourVersion = v2
                    , theirVersions = KnownHydraVersions [v1]
                    }

      it "throws ParameterMismatch when configuration does not match persistence" $ \_ -> do
        -- FIXME: Make this equivalent to before?
        pendingWith "TODO: not implemented"

  describe "Serialisation" $ do
    prop "can roundtrip CBOR encoding/decoding of Hydra Message" $ prop_canRoundtripCBOREncoding @(Message SimpleTx)
    roundtripAndGoldenSpecs (Proxy @(Message SimpleTx))

lo :: IsString s => s
lo = "127.0.0.1"

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
            { host = lo
            , port = port1
            , signingKey = aliceSk
            , otherParties = [bob, carol]
            , peers = [bobHost, carolHost]
            , nodeId = "alice"
            , persistenceDir = tmp </> "alice"
            }
      , bobConfig =
          NetworkConfiguration
            { host = lo
            , port = port2
            , signingKey = bobSk
            , otherParties = [alice, carol]
            , peers = [aliceHost, carolHost]
            , nodeId = "bob"
            , persistenceDir = tmp </> "bob"
            }
      , carolConfig =
          NetworkConfiguration
            { host = lo
            , port = port3
            , signingKey = carolSk
            , otherParties = [alice, bob]
            , peers = [aliceHost, bobHost]
            , nodeId = "carol"
            , persistenceDir = tmp </> "carol"
            }
      }

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a, Show a) => a -> Property
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) === Right a

noopCallback :: Applicative m => NetworkCallback msg m
noopCallback =
  NetworkCallback
    { deliver = \_ -> pure ()
    , onConnectivity = const $ pure ()
    }

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
