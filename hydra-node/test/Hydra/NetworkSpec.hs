{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Hydra.Prelude

import Cardano.Binary (FromCBOR, ToCBOR, fromCBOR, toCBOR)
import Cardano.Crypto.DSIGN (DSIGNAlgorithm (signDSIGN), genKeyDSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Control.Monad.Class.MonadSTM (newEmptyTMVarIO, putTMVar, takeTMVar)
import Data.IP (toIPv4w)
import Hydra.HeadLogic (HydraMessage (..), Snapshot (..))
import Hydra.Ledger (Party (..), Signed (UnsafeSigned))
import Hydra.Ledger.Builder (utxoRef)
import Hydra.Ledger.Simple (SimpleTx (..))
import Hydra.Ledger.SimpleSpec (genSimpleTx, genUtxo)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Network (Host (..), Network)
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
import Test.QuickCheck.Instances.ByteString ()
import Test.Util (arbitraryNatural, failAfter)

spec :: Spec
spec = describe "Networking layer" $ do
  let requestTx :: HydraMessage SimpleTx
      requestTx = ReqTx (SimpleTx 1 (utxoRef 1) (utxoRef 2))

      lo = "127.0.0.1"

  describe "Ouroboros Network" $ do
    it "broadcasts messages to single connected peer" $ do
      received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $
        withOuroborosNetwork tracer (Host lo 45778) [Host lo 45779] (const @_ @(HydraMessage SimpleTx) $ pure ()) $ \hn1 ->
          withOuroborosNetwork @(HydraMessage SimpleTx) tracer (Host lo 45779) [Host lo 45778] (atomically . putTMVar received) $ \_ -> do
            broadcast hn1 requestTx
            atomically (takeTMVar received) `shouldReturn` requestTx

    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyTMVarIO
      node2received <- newEmptyTMVarIO
      node3received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $
        withOuroborosNetwork @(HydraMessage SimpleTx) tracer (Host lo 45678) [Host lo 45679, Host lo 45680] (atomically . putTMVar node1received) $ \hn1 ->
          withOuroborosNetwork tracer (Host lo 45679) [Host lo 45678, Host lo 45680] (atomically . putTMVar node2received) $ \hn2 ->
            withOuroborosNetwork tracer (Host lo 45680) [Host lo 45678, Host lo 45679] (atomically . putTMVar node3received) $ \hn3 ->
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

  describe "0MQ Network" $
    it "broadcasts messages between 3 connected peers" $ do
      node1received <- newEmptyTMVarIO
      node2received <- newEmptyTMVarIO
      node3received <- newEmptyTMVarIO
      showLogsOnFailure $ \tracer -> failAfter 10 $
        withZeroMQNetwork tracer (Host lo 55677) [Host lo 55678, Host lo 55679] (atomically . putTMVar node1received) $ \hn1 ->
          withZeroMQNetwork tracer (Host lo 55678) [Host lo 55677, Host lo 55679] (atomically . putTMVar node2received) $ \hn2 ->
            withZeroMQNetwork tracer (Host lo 55679) [Host lo 55677, Host lo 55678] (atomically . putTMVar node3received) $ \hn3 ->
              concurrently_ (assertBroadcastFrom requestTx hn1 [node2received, node3received]) $
                concurrently_
                  (assertBroadcastFrom requestTx hn2 [node1received, node3received])
                  (assertBroadcastFrom requestTx hn3 [node2received, node1received])

  describe "Serialisation" $
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

-- | Some random signature, for any type 'a'.
genSignature :: Gen (Signed a)
genSignature = do
  key <- genKeyDSIGN . mkSeedFromBytes . fromList <$> vectorOf 8 arbitrary
  a <- arbitrary @ByteString
  pure . UnsafeSigned $ signDSIGN () a key

genHost :: Gen Host
genHost = do
  ip <- toIPv4w <$> arbitrary
  port <- fromIntegral <$> chooseBoundedIntegral (1, maxBound @Word16)
  pure $ Host (show ip) port

instance Arbitrary (HydraMessage SimpleTx) where
  arbitrary =
    oneof
      [ ReqTx <$> genSimpleTx
      , AckTx <$> genParty <*> genSimpleTx
      , ReqSn <$> genParty <*> arbitraryNatural <*> vectorOf 10 genSimpleTx
      , AckSn <$> genParty <*> genSignature <*> arbitraryNatural
      , Ping <$> genHost
      ]

instance Arbitrary (Snapshot SimpleTx) where
  arbitrary = Snapshot <$> arbitraryNatural <*> genUtxo <*> vectorOf 10 genSimpleTx

prop_canRoundtripCBOREncoding ::
  (ToCBOR a, FromCBOR a, Eq a) => a -> Bool
prop_canRoundtripCBOREncoding a =
  let encoded = toLazyByteString $ toCBOR a
   in (snd <$> deserialiseFromBytes fromCBOR encoded) == Right a
