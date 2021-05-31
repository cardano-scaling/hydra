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
import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Hydra.Logging (nullTracer)
import Hydra.Logic (HydraMessage (..))
import Hydra.Network.Ouroboros (broadcast, withOuroborosHydraNetwork)
import Hydra.Network.ZeroMQ (withZeroMQHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)
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
          withOuroborosHydraNetwork (lo, 45679) [] (putMVar received) $ \_ -> do
            broadcast hn1 requestTx
            takeMVar received `shouldReturn` requestTx

    it "broadcasts messages to 2 connected peer" $ do
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 3 $ do
        withOuroborosHydraNetwork (lo, 45678) [(lo, 45679), (lo, 45680)] (const $ pure ()) $ \hn1 ->
          withOuroborosHydraNetwork (lo, 45679) [] (putMVar node2received) $ \_ -> do
            withOuroborosHydraNetwork (lo, 45680) [] (putMVar node3received) $ \_ -> do
              broadcast hn1 requestTx
              failAfter 1 $ takeMVar node2received `shouldReturn` requestTx
              failAfter 1 $ takeMVar node3received `shouldReturn` requestTx

  describe "0MQ Network" $
    it "broadcasts messages to 2 connected peers" $ do
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 2 $ do
        withZeroMQHydraNetwork (lo, 55677) [(lo, 55678), (lo, 55679)] nullTracer (const $ pure ()) $ \hn1 ->
          withZeroMQHydraNetwork (lo, 55678) [(lo, 55677), (lo, 55679)] nullTracer (putMVar node2received) $ \_ ->
            withZeroMQHydraNetwork (lo, 55679) [(lo, 55677), (lo, 55678)] nullTracer (putMVar node3received) $ \_ -> do
              threadDelay 1 -- This is needed to wait for all nodes to be up
              broadcast hn1 requestTx
              takeMVar node2received `shouldReturn` requestTx
              takeMVar node3received `shouldReturn` requestTx

  describe "Serialisation" $ do
    it "can roundtrip serialisation of HydraMessage" $ property $ prop_canRoundtripSerialise @(HydraMessage Integer)
    it "can roundtrip CBOR encoding/decoding of HydraMessage" $ property $ prop_canRoundtripCBOREncoding @(HydraMessage Integer)

instance Arbitrary (HydraMessage Integer) where
  arbitrary = oneof [ReqTx <$> arbitrary, pure AckTx, pure ConfTx, pure ReqSn, pure AckSn, pure ConfSn]

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
    Nothing -> expectationFailure "Test timed out after 2 seconds"
    Just _ -> pure ()
