{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude hiding (threadDelay)

import Cardano.Binary (FromCBOR, ToCBOR, fromCBOR, toCBOR)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadTime (DiffTime)
import Control.Monad.Class.MonadTimer (timeout)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Logging (nullTracer)
import Hydra.Network (HydraNetwork)
import Hydra.Network.Ouroboros (broadcast, withOuroborosHydraNetwork)
import Hydra.Network.ZeroMQ (withZeroMQHydraNetwork)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec (Spec, describe, expectationFailure, it, pendingWith, shouldReturn)
import Test.QuickCheck (Arbitrary (..), Positive (getPositive), arbitrary, oneof, property)

type MockTx = ()

spec :: Spec
spec = describe "Networking layer" $ do
  let requestTx :: HydraMessage Integer
      requestTx = ReqTx 1

      lo = "127.0.0.1"

  describe "Ouroboros Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- newEmptyMVar
      failAfter 10 $ do
        withOuroborosHydraNetwork (lo, 45678) [(lo, 45679)] (const @_ @(HydraMessage Integer) $ pure ()) $ \hn1 ->
          withOuroborosHydraNetwork @(HydraMessage Integer) (lo, 45679) [(lo, 45678)] (putMVar received) $ \_ -> do
            broadcast hn1 requestTx
            takeMVar received `shouldReturn` requestTx

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyMVar
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 10 $ do
        withOuroborosHydraNetwork @(HydraMessage Integer) (lo, 45678) [(lo, 45679), (lo, 45680)] (putMVar node1received) $ \hn1 ->
          withOuroborosHydraNetwork (lo, 45679) [(lo, 45678), (lo, 45680)] (putMVar node2received) $ \hn2 -> do
            withOuroborosHydraNetwork (lo, 45680) [(lo, 45678), (lo, 45679)] (putMVar node3received) $ \hn3 -> do
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

    it "broadcasts messages to itself" $ do
      pendingWith "not tested yet"

  describe "0MQ Network" $
    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyMVar
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter 10 $ do
        withZeroMQHydraNetwork (lo, 55677) [(lo, 55678), (lo, 55679)] nullTracer (putMVar node1received) $ \hn1 ->
          withZeroMQHydraNetwork (lo, 55678) [(lo, 55677), (lo, 55679)] nullTracer (putMVar node2received) $ \hn2 ->
            withZeroMQHydraNetwork (lo, 55679) [(lo, 55677), (lo, 55678)] nullTracer (putMVar node3received) $ \hn3 -> do
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

  describe "Serialisation" $ do
    it "can roundtrip serialisation of HydraMessage" $ property $ prop_canRoundtripSerialise @(HydraMessage Integer)
    it "can roundtrip CBOR encoding/decoding of HydraMessage" $ property $ prop_canRoundtripCBOREncoding @(HydraMessage Integer)

assertBroadcastFrom :: Eq a => Show a => a -> HydraNetwork IO a -> [MVar a] -> IO ()
assertBroadcastFrom requestTx network receivers =
  tryBroadcast `catch` \(_ :: HUnitFailure) -> tryBroadcast
 where
  tryBroadcast = do
    broadcast network requestTx
    forM_ receivers $ \var -> failAfter 1 $ takeMVar var `shouldReturn` requestTx

instance Arbitrary (HydraMessage Integer) where
  arbitrary =
    oneof
      [ ReqTx <$> arbitrary
      , AckTx <$> arbitraryNatural <*> arbitrary
      , pure ConfTx
      , pure ReqSn
      , pure AckSn
      , pure ConfSn
      ]
   where
    arbitraryNatural = fromIntegral . getPositive <$> arbitrary @(Positive Integer)

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
