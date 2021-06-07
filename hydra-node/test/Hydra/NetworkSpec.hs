{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude hiding (threadDelay)

import Cardano.Binary (FromCBOR, ToCBOR, fromCBOR, toCBOR)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Class.MonadTime (DiffTime)
import Control.Monad.Class.MonadTimer (timeout)
import Hydra.HeadLogic (HydraMessage (..), NetworkEvent (MessageReceived, NetworkConnected))
import Hydra.Logging (nullTracer)
import Hydra.Network.Ouroboros (broadcast, withOuroborosHydraNetwork)
import Hydra.Network.ZeroMQ (withZeroMQHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, pendingWith, shouldReturn)
import Test.QuickCheck (Arbitrary (..), arbitrary, oneof, property)

type MockTx = ()

spec :: Spec
spec = describe "Networking layer" $ do
  let requestTx :: HydraMessage Integer
      requestTx = ReqTx 1

      lo = "127.0.0.1"

  describe "Ouroboros Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- newEmptyMVar
      failAfter 2 $ do
        withOuroborosHydraNetwork (lo, 45678) [(lo, 45679)] (const $ pure ()) $ \hn1 ->
          withOuroborosHydraNetwork (lo, 45679) [(lo, 45678)] (putMVar received) $ \_ -> do
            takeMVar received `shouldReturn` NetworkConnected
            broadcast hn1 requestTx
            takeMVar received `shouldReturn` MessageReceived requestTx

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyMVar
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 3 $ do
        withOuroborosHydraNetwork (lo, 45678) [(lo, 45679), (lo, 45680)] (putMVar node1received) $ \hn1 ->
          withOuroborosHydraNetwork (lo, 45679) [(lo, 45678), (lo, 45680)] (putMVar node2received) $ \hn2 -> do
            withOuroborosHydraNetwork (lo, 45680) [(lo, 45678), (lo, 45679)] (putMVar node3received) $ \hn3 -> do
              failAfter 1 $ takeMVar node1received `shouldReturn` NetworkConnected
              failAfter 1 $ takeMVar node2received `shouldReturn` NetworkConnected
              failAfter 1 $ takeMVar node3received `shouldReturn` NetworkConnected

              broadcast hn1 requestTx
              failAfter 1 $ takeMVar node2received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node3received `shouldReturn` MessageReceived requestTx

              broadcast hn2 requestTx
              failAfter 1 $ takeMVar node1received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node3received `shouldReturn` MessageReceived requestTx

              broadcast hn3 requestTx
              failAfter 1 $ takeMVar node1received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node2received `shouldReturn` MessageReceived requestTx

  describe "0MQ Network" $
    it "broadcasts messages between 3 connected peers" $ do
      pendingWith "missing network callback"
      node1received <- newEmptyMVar
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 3 $ do
        withZeroMQHydraNetwork (lo, 55677) [(lo, 55678), (lo, 55679)] nullTracer (putMVar node1received) $ \hn1 ->
          withZeroMQHydraNetwork (lo, 55678) [(lo, 55677), (lo, 55679)] nullTracer (putMVar node2received) $ \hn2 ->
            withZeroMQHydraNetwork (lo, 55679) [(lo, 55677), (lo, 55678)] nullTracer (putMVar node3received) $ \hn3 -> do
              failAfter 1 $ takeMVar node1received `shouldReturn` NetworkConnected
              failAfter 1 $ takeMVar node2received `shouldReturn` NetworkConnected
              failAfter 1 $ takeMVar node3received `shouldReturn` NetworkConnected

              broadcast hn1 requestTx
              failAfter 1 $ takeMVar node2received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node3received `shouldReturn` MessageReceived requestTx

              broadcast hn2 requestTx
              failAfter 1 $ takeMVar node1received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node3received `shouldReturn` MessageReceived requestTx

              broadcast hn3 requestTx
              failAfter 1 $ takeMVar node1received `shouldReturn` MessageReceived requestTx
              failAfter 1 $ takeMVar node2received `shouldReturn` MessageReceived requestTx

  describe "Serialisation" $ do
    it "can roundtrip serialisation of HydraMessage" $ property $ prop_canRoundtripSerialise @(HydraMessage Integer)
    it "can roundtrip CBOR encoding/decoding of HydraMessage" $ property $ prop_canRoundtripCBOREncoding @(HydraMessage Integer)

instance Arbitrary (HydraMessage Integer) where
  arbitrary =
    oneof
      [ ReqTx <$> arbitrary
      , AckTx <$> fmap (fromIntegral @Int) arbitrary <*> arbitrary
      , pure ConfTx
      , pure ReqSn
      , pure AckSn
      , pure ConfSn
      ]

prop_canRoundtripSerialise :: (Serialise a, Eq a) => a -> Bool
prop_canRoundtripSerialise a =
  let encoded = serialise a
   in deserialiseOrFail encoded == Right a

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a

failAfter :: HasCallStack => DiffTime -> IO () -> IO ()
failAfter seconds action =
  timeout seconds action >>= \case
    Nothing -> expectationFailure $ "Test timed out after " <> show seconds <> " seconds"
    Just _ -> pure ()
