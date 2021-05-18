{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude hiding (threadDelay)

import Cardano.Binary (FromCBOR, ToCBOR, fromCBOR, toCBOR)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Hydra.Logic (HydraMessage (..))
import Hydra.Network.Ouroboros (broadcast, withOuroborosHydraNetwork)
import Hydra.Network.ZeroMQ (withZeroMQHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn, xit)
import Test.QuickCheck (Arbitrary (..), arbitrary, elements, property)

type MockTx = ()

spec :: Spec
spec = describe "Networking layer" $ do
  describe "Ouroboros Network" $ do
    xit "broadcasts messages to single connected peer" $ do
      received <- newEmptyMVar
      failAfter2Seconds $ do
        withOuroborosHydraNetwork ("127.0.0.1", "45678") [("127.0.0.1", "45679")] (const $ pure ()) $ \hn1 ->
          withOuroborosHydraNetwork ("127.0.0.1", "45679") [("127.0.0.1", "45678")] (putMVar received) $ \_ -> do
            broadcast hn1 ReqTx
            takeMVar received `shouldReturn` ReqTx

  describe "0MQ Network" $ do
    it "broadcasts messages to 2 connected peers" $ do
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter2Seconds $ do
        withZeroMQHydraNetwork ("127.0.0.1", "55677") [("127.0.0.1", "55678"), ("127.0.0.1", "55679")] (const $ pure ()) $ \hn1 ->
          withZeroMQHydraNetwork ("127.0.0.1", "55678") [("127.0.0.1", "55677"), ("127.0.0.1", "55679")] (putMVar node2received) $ \_ ->
            withZeroMQHydraNetwork ("127.0.0.1", "55679") [("127.0.0.1", "55677"), ("127.0.0.1", "55678")] (putMVar node3received) $ \_ -> do
              threadDelay 1 -- This is needed to wait for all nodes to be up
              broadcast hn1 ReqTx
              takeMVar node2received `shouldReturn` ReqTx
              takeMVar node3received `shouldReturn` ReqTx

  describe "Serialisation" $ do
    it "can roundtrip serialisation of HydraMessage" $ property $ prop_canRoundtripSerialise @HydraMessage
    it "can roundtrip CBOR encoding/decoding of HydraMessage" $ property $ prop_canRoundtripCBOREncoding @HydraMessage

instance Arbitrary HydraMessage where
  arbitrary = elements [ReqTx, AckTx, ConfTx, ReqSn, AckSn, ConfSn]

prop_canRoundtripSerialise :: (Serialise a, Eq a) => a -> Bool
prop_canRoundtripSerialise a =
  let encoded = serialise a
   in deserialiseOrFail encoded == Right a

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a

failAfter2Seconds :: IO () -> IO ()
failAfter2Seconds action =
  timeout 2 action >>= \case
    Nothing -> expectationFailure "Test timed out after 2 seconds"
    Just _ -> pure ()
