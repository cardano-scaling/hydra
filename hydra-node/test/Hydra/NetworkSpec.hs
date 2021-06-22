{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude

import Cardano.Binary (FromCBOR, ToCBOR, fromCBOR, toCBOR)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad.Class.MonadSTM (newEmptyTMVarIO, putTMVar, takeTMVar)
import Data.IP (toIPv4w)
import Hydra.HeadLogic (Host, HydraMessage (..), Snapshot (..))
import Hydra.Ledger (Party (..))
import Hydra.Ledger.Builder (utxoRef)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Ledger.SimpleSpec (genSimpleTx, genUtxo)
import Hydra.Network (Network)
import Hydra.Network.Ouroboros (broadcast, withOuroborosNetwork)
import Hydra.Network.ZeroMQ (withZeroMQNetwork)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec (Spec, describe, it, shouldReturn)
import Test.QuickCheck (
  Arbitrary (..),
  arbitrary,
  chooseBoundedIntegral,
  getPositive,
  oneof,
  property,
  vectorOf,
 )
import Test.QuickCheck.Gen (Gen)
import Test.Util (arbitraryNatural, failAfter, showLogsOnFailure)

spec :: Spec
spec = describe "Networking layer" $ do
  let requestTx :: HydraMessage SimpleTx
      requestTx = ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))

      lo = "127.0.0.1"

  describe "Ouroboros Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $ do
        withOuroborosNetwork tracer (lo, 45778) [(lo, 45779)] (const @_ @(HydraMessage SimpleTx) $ pure ()) $ \hn1 ->
          withOuroborosNetwork @(HydraMessage SimpleTx) tracer (lo, 45779) [(lo, 45778)] (atomically . putTMVar received) $ \_ -> do
            broadcast hn1 requestTx
            atomically (takeTMVar received) `shouldReturn` requestTx

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyTMVarIO
      node2received <- newEmptyTMVarIO
      node3received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $ do
        withOuroborosNetwork @(HydraMessage SimpleTx) tracer (lo, 45678) [(lo, 45679), (lo, 45680)] (atomically . putTMVar node1received) $ \hn1 ->
          withOuroborosNetwork tracer (lo, 45679) [(lo, 45678), (lo, 45680)] (atomically . putTMVar node2received) $ \hn2 -> do
            withOuroborosNetwork tracer (lo, 45680) [(lo, 45678), (lo, 45679)] (atomically . putTMVar node3received) $ \hn3 -> do
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

  describe "0MQ Network" $
    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyTMVarIO
      node2received <- newEmptyTMVarIO
      node3received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $ do
        withZeroMQNetwork tracer (lo, 55677) [(lo, 55678), (lo, 55679)] (atomically . putTMVar node1received) $ \hn1 ->
          withZeroMQNetwork tracer (lo, 55678) [(lo, 55677), (lo, 55679)] (atomically . putTMVar node2received) $ \hn2 ->
            withZeroMQNetwork tracer (lo, 55679) [(lo, 55677), (lo, 55678)] (atomically . putTMVar node3received) $ \hn3 -> do
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

  describe "Serialisation" $ do
    it "can roundtrip CBOR encoding/decoding of HydraMessage" $ property $ prop_canRoundtripCBOREncoding @(HydraMessage SimpleTx)

assertBroadcastFrom :: (Eq a, Show a) => a -> Network IO a -> [TMVar IO a] -> IO ()
assertBroadcastFrom requestTx network receivers =
  tryBroadcast `catch` \(_ :: HUnitFailure) -> tryBroadcast
 where
  tryBroadcast = do
    broadcast network requestTx
    forM_ receivers $ \var -> failAfter 1 $ atomically (takeTMVar var) `shouldReturn` requestTx

genParty :: Gen Party
genParty = UnsafeParty . fromInteger . getPositive <$> arbitrary

genHost :: Gen Host
genHost = do
  ip <- toIPv4w <$> arbitrary
  port <- fromIntegral <$> chooseBoundedIntegral (1, maxBound @Word16)
  pure (show ip, port)

instance Arbitrary (HydraMessage SimpleTx) where
  arbitrary =
    oneof
      [ ReqTx <$> genSimpleTx
      , AckTx <$> genParty <*> genSimpleTx
      , ReqSn <$> genParty <*> arbitraryNatural <*> vectorOf 10 genSimpleTx
      , AckSn <$> genParty <*> arbitraryNatural
      , Ping <$> genHost
      ]

instance Arbitrary (Snapshot SimpleTx) where
  arbitrary = Snapshot <$> arbitraryNatural <*> genUtxo <*> vectorOf 10 genSimpleTx

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a
