{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad.Class.MonadSTM (newEmptyTMVarIO, newTMVarIO, newTQueue, readTMVar, readTQueue, writeTQueue)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network, NetworkException, PortNumber, readHost, showHost)
import Hydra.Network.Message (Message (..))
import Hydra.Network.Ouroboros (broadcast, withOuroborosNetwork)
import Hydra.Network.Topology (NetworkTopology (..), withConfigWatcher)
import Hydra.Prelude
import System.FilePath ((</>))
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Hydra.Prelude
import Test.Network.Ports (randomUnusedTCPPorts)
import Test.QuickCheck (
  property,
 )
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = parallel $
  describe "Networking layer" $
    do
      let lo = "127.0.0.1"

      describe "Ouroboros Network" $ do
        it "raises exception when broadcasting given no peer is given" $ do
          showLogsOnFailure $ \tracer -> failAfter 30 $ do
            [port1] <- fmap fromIntegral <$> randomUnusedTCPPorts 1
            peers <- newEmptyTMVarIO
            withOuroborosNetwork tracer (Host lo port1) peers (const @_ @Integer $ pure ()) $ \hn1 ->
              broadcast hn1 1 `shouldThrow` \(_ :: NetworkException) -> True

        it "broadcasts messages to single connected peer" $ do
          received <- atomically newTQueue
          showLogsOnFailure $ \tracer -> failAfter 30 $ do
            [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
            peers1 <- newTMVarIO $ NetworkTopology [Host lo port2]
            peers2 <- newTMVarIO $ NetworkTopology [Host lo port1]
            withOuroborosNetwork tracer (Host lo port1) peers1 (const @_ @Integer $ pure ()) $ \hn1 ->
              withOuroborosNetwork @Integer tracer (Host lo port2) peers2 (atomically . writeTQueue received) $ \_ -> do
                withAsync (1 `broadcastFrom` hn1) $ \_ ->
                  atomically (readTQueue received) `shouldReturn` 1

        it "broadcasts messages between 3 connected peers" $ do
          node1received <- atomically newTQueue
          node2received <- atomically newTQueue
          node3received <- atomically newTQueue
          showLogsOnFailure $ \tracer -> failAfter 30 $ do
            [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
            peers1 <- newTMVarIO $ NetworkTopology [Host lo port2, Host lo port3]
            peers2 <- newTMVarIO $ NetworkTopology [Host lo port1, Host lo port3]
            peers3 <- newTMVarIO $ NetworkTopology [Host lo port1, Host lo port2]
            withOuroborosNetwork @Integer tracer (Host lo port1) peers1 (atomically . writeTQueue node1received) $ \hn1 ->
              withOuroborosNetwork tracer (Host lo port2) peers2 (atomically . writeTQueue node2received) $ \hn2 -> do
                withOuroborosNetwork tracer (Host lo port3) peers3 (atomically . writeTQueue node3received) $ \hn3 -> do
                  assertAllNodesBroadcast
                    [ (port1, hn1, node1received)
                    , (port2, hn2, node2received)
                    , (port3, hn3, node3received)
                    ]

        describe "Serialisation" $ do
          it "can roundtrip CBOR encoding/decoding of Hydra Message" $ property $ prop_canRoundtripCBOREncoding @(Message SimpleTx)
          roundtripAndGoldenSpecs (Proxy @(Message SimpleTx))

      describe "Host" $ do
        prop "rountrip showHost/readHost" $ \(h :: Host) ->
          readHost (showHost h) == Just h

      describe "Config watcher" $ do
        it "writes decoded network topology to shared variable" $ do
          failAfter 5 $ do
            peers <- newEmptyTMVarIO
            withTempDir "network" $ \dir ->
              withConfigWatcher (dir </> "topology.json") peers $ do
                let networkTopology = NetworkTopology{hosts = [Host lo 45678]}
                LBS.writeFile (dir </> "topology.json") $ Aeson.encode networkTopology
                atomically (readTMVar peers) `shouldReturn` networkTopology

assertAllNodesBroadcast ::
  [(PortNumber, Network IO Integer, TQueue IO Integer)] ->
  Expectation
assertAllNodesBroadcast networks =
  parallelBroadcast checkQueues networks
 where
  parallelBroadcast :: IO () -> [(PortNumber, Network IO Integer, TQueue IO Integer)] -> IO ()
  parallelBroadcast check [] = check
  parallelBroadcast check ((port, network, _) : rest) =
    withAsync (fromIntegral port `broadcastFrom` network) $ \_ -> parallelBroadcast check rest

  checkQueues :: IO ()
  checkQueues =
    sequence_ $
      [ shouldEventuallyReceive receiver myPort (fromIntegral otherPort)
      | (myPort, _, receiver) <- networks
      , (otherPort, _, _) <- networks
      , otherPort /= myPort
      ]

broadcastFrom :: a -> Network IO a -> IO ()
broadcastFrom requestTx network =
  forever $ threadDelay 0.1 >> broadcast network requestTx

shouldEventuallyReceive :: TQueue IO Integer -> PortNumber -> Integer -> Expectation
shouldEventuallyReceive queue numNode value = do
  val <- atomically $ readTQueue queue
  unless (val == value) $ shouldEventuallyReceive queue numNode value

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a
