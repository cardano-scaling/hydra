-- | Tests for the UDP example event sink.
module Hydra.Events.UDPSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.ByteString.Char8 qualified as BS8
import Hydra.Events (EventId, EventSink (..), putEvent)
import Hydra.Events.UDP (newUDPEventSink)
import Network.UDP (recvFrom, serverSocket, stop)
import Test.Network.Ports (withFreePort)

spec :: Spec
spec = do
  describe "putEvent" $ do
    it "sends datagram" $ do
      withFreePort $ \port -> do
        bracket (serverSocket ("0.0.0.0", port)) stop $ \socket -> do
          sink <- newUDPEventSink "0.0.0.0" (show port)
          let event = 123 :: EventId
          putEvent sink event
          (received, _) <- recvFrom socket
          BS8.unpack received `shouldContain` "123"

    it "allows concurrent usage" $ do
      withFreePort $ \port -> do
        EventSink{putEvent} <- newUDPEventSink @EventId "localhost" (show port)
        concurrently_ (putEvent 123) (putEvent 456)

  it "supports multiple instances" $ do
    withFreePort $ \port -> do
      s1 <- newUDPEventSink @EventId "localhost" (show port)
      putEvent s1 123
      s2 <- newUDPEventSink @EventId "localhost" (show port)
      putEvent s2 456
