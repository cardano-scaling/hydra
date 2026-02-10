-- | Tests for the UDP example event sink.
module Hydra.Events.UDPSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "bytestring" Data.ByteString.Char8 qualified as BS8
import "hydra-test-utils" Test.Network.Ports (withFreePort)
import "network-udp" Network.UDP (recvFrom, serverSocket, stop)
import "resourcet" Control.Monad.Trans.Resource (runResourceT)

import Hydra.Events (EventId, EventSink (..), putEvent)
import Hydra.Events.UDP (newUDPEventSink, withUDPEventSink)

spec :: Spec
spec = do
  describe "putEvent" $ do
    it "sends datagram" $ do
      withFreePort $ \port -> do
        bracket (serverSocket ("0.0.0.0", port)) stop $ \socket -> do
          withUDPEventSink "0.0.0.0" (show port) $ \sink -> do
            let event = 123 :: EventId
            putEvent sink event
            (received, _) <- recvFrom socket
            BS8.unpack received `shouldContain` "123"

    it "allows concurrent usage" $ do
      withFreePort $ \port -> do
        withUDPEventSink @EventId "0.0.0.0" (show port) $ \EventSink{putEvent} -> do
          concurrentlyLabelled_ ("put-event-123", putEvent 123) ("put-event-456", putEvent 456)

  it "supports multiple instances" $ do
    withFreePort $ \port -> do
      withUDPEventSink @EventId "0.0.0.0" (show port) $ \s1 -> do
        withUDPEventSink @EventId "0.0.0.0" (show port) $ \s2 -> do
          putEvent s1 123
          putEvent s2 456

      runResourceT $ do
        s1 <- newUDPEventSink @EventId "0.0.0.0" (show port)
        s2 <- newUDPEventSink @EventId "0.0.0.0" (show port)
        lift $ putEvent s1 123
        lift $ putEvent s2 456
