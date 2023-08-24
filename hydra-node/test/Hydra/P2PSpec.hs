{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.P2PSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Concurrent.Class.MonadSTM (newTQueue, readTQueue, writeTQueue)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network)
import Hydra.Network.Message (Message (..))
import Hydra.Network.Ouroboros (broadcast)
import Hydra.Network.P2P.P2P (withP2PNetwork)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Network.Ports (randomUnusedTCPPorts)
import Test.QuickCheck (
  Property,
  (===),
 )
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = do
  let lo = "127.0.0.1"

  describe "P2P Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 30 $ do
        [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
        withP2PNetwork tracer (Host lo port1) [Host lo port2] (const @_ @Integer $ pure ()) $ \hn1 ->
          withP2PNetwork @Integer tracer (Host lo port2) [Host lo port1] (atomically . writeTQueue received) $ \_ -> do
            withNodeBroadcastingForever hn1 1 $
              atomically (readTQueue received) `shouldReturn` 1

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- atomically newTQueue
      node2received <- atomically newTQueue
      node3received <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 30 $ do
        [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
        withP2PNetwork @Integer tracer (Host lo port1) [Host lo port2, Host lo port3] (atomically . writeTQueue node1received) $ \hn1 ->
          withP2PNetwork tracer (Host lo port2) [Host lo port1, Host lo port3] (atomically . writeTQueue node2received) $ \hn2 -> do
            withP2PNetwork tracer (Host lo port3) [Host lo port1, Host lo port2] (atomically . writeTQueue node3received) $ \hn3 -> do
              withNodesBroadcastingForever [(hn1, 1), (hn2, 2), (hn3, 3)] $
                assertAllnodesReceivedMessagesFromAllOtherNodes [(node1received, 1), (node2received, 2), (node3received, 3)]

  describe "Serialisation" $ do
    prop "can roundtrip CBOR encoding/decoding of Hydra Message" $ prop_canRoundtripCBOREncoding @(Message SimpleTx)
    roundtripAndGoldenSpecs (Proxy @(Message SimpleTx))

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
