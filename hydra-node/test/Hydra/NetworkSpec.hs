{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (DSIGNAlgorithm (signDSIGN), genKeyDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad.Class.MonadSTM (newTQueue, readTQueue, writeTQueue)
import Hydra.Ledger (Signed (UnsafeSigned))
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network, PortNumber)
import Hydra.Network.Message (Message (..))
import Hydra.Network.Ouroboros (broadcast, withOuroborosNetwork)
import Hydra.Network.Ports (randomUnusedTCPPorts)
import Hydra.Network.ZeroMQ (withZeroMQNetwork)
import Hydra.Test.Prelude (failAfter)
import Test.Hspec (Expectation, Spec, describe, it, shouldReturn)
import Test.QuickCheck (
  oneof,
  property,
  vectorOf,
 )
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Instances.ByteString ()

spec :: Spec
spec = describe "Networking layer" $ do
  let lo = "127.0.0.1"

  describe "Ouroboros Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 30 $ do
        [port1, port2] <- fmap fromIntegral <$> randomUnusedTCPPorts 2
        withOuroborosNetwork tracer (Host lo port1) [Host lo port2] (const @_ @Integer $ pure ()) $ \hn1 ->
          withOuroborosNetwork @Integer tracer (Host lo port2) [Host lo port1] (atomically . writeTQueue received) $ \_ -> do
            withAsync (1 `broadcastFrom` hn1) $ \_ ->
              atomically (readTQueue received) `shouldReturn` 1

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- atomically newTQueue
      node2received <- atomically newTQueue
      node3received <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 30 $ do
        [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
        withOuroborosNetwork @Integer tracer (Host lo port1) [Host lo port2, Host lo port3] (atomically . writeTQueue node1received) $ \hn1 ->
          withOuroborosNetwork tracer (Host lo port2) [Host lo port1, Host lo port3] (atomically . writeTQueue node2received) $ \hn2 -> do
            withOuroborosNetwork tracer (Host lo port3) [Host lo port1, Host lo port2] (atomically . writeTQueue node3received) $ \hn3 -> do
              assertAllNodesBroadcast
                [ (port1, hn1, node1received)
                , (port2, hn2, node2received)
                , (port3, hn3, node3received)
                ]

  describe "0MQ Network" $
    it "broadcasts messages between 3 connected peers" $ do
      node1received <- atomically newTQueue
      node2received <- atomically newTQueue
      node3received <- atomically newTQueue
      showLogsOnFailure $ \tracer -> failAfter 10 $ do
        [port1, port2, port3] <- fmap fromIntegral <$> randomUnusedTCPPorts 3
        withZeroMQNetwork tracer (Host lo port1) [Host lo port2, Host lo port3] (atomically . writeTQueue node1received) $ \hn1 ->
          withZeroMQNetwork tracer (Host lo port2) [Host lo port1, Host lo port3] (atomically . writeTQueue node2received) $ \hn2 ->
            withZeroMQNetwork tracer (Host lo port3) [Host lo port1, Host lo port2] (atomically . writeTQueue node3received) $ \hn3 -> do
              assertAllNodesBroadcast
                [ (port1, hn1, node1received)
                , (port2, hn2, node2received)
                , (port3, hn3, node3received)
                ]

  describe "Serialisation" $
    it "can roundtrip CBOR encoding/decoding of Hydra Message" $ property $ prop_canRoundtripCBOREncoding @(Message SimpleTx)

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

-- | Some random signature, for any type 'a'.
genSignature :: Gen (Signed a)
genSignature = do
  key <- genKeyDSIGN . mkSeedFromBytes . fromList <$> vectorOf 8 arbitrary
  a <- arbitrary @ByteString
  pure . UnsafeSigned $ signDSIGN () a key

-- TODO(SN): can we write a generic 'Arbitrary (Message tx)' instance and then
-- move it to the data type?
instance Arbitrary (Message SimpleTx) where
  arbitrary =
    oneof
      [ ReqTx <$> arbitrary <*> arbitrary
      , ReqSn <$> arbitrary <*> arbitrary <*> vectorOf 10 arbitrary
      , AckSn <$> arbitrary <*> genSignature <*> arbitrary
      , Connected <$> arbitrary
      , Disconnected <$> arbitrary
      ]

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a
