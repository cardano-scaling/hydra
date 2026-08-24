-- | Tests for the UDP example event sink.
module Hydra.Events.UDPSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Char8 qualified as BS8
import Hydra.Events (EventId, EventSink (..), putEvent)
import Hydra.Events.UDP (newUDPEventSink, withUDPEventSink)
import Network.Socket (PortNumber, socketPort)
import Network.UDP (ListenSocket (..), recvFrom, serverSocket, stop)

spec :: Spec
spec = do
  describe "putEvent" $ do
    it "sends datagram" $ do
      withUDPServerSocket $ \socket port -> do
        withUDPEventSink "0.0.0.0" (show port) $ \sink -> do
          let event = 123 :: EventId
          putEvent sink event
          (received, _) <- recvFrom socket
          BS8.unpack received `shouldContain` "123"

    it "allows concurrent usage" $ do
      withUDPServerSocket $ \_ port -> do
        withUDPEventSink @EventId "0.0.0.0" (show port) $ \EventSink{putEvent} -> do
          concurrentlyLabelled_ ("put-event-123", putEvent 123) ("put-event-456", putEvent 456)

  it "supports multiple instances" $ do
    withUDPServerSocket $ \_ port -> do
      withUDPEventSink @EventId "0.0.0.0" (show port) $ \s1 -> do
        withUDPEventSink @EventId "0.0.0.0" (show port) $ \s2 -> do
          putEvent s1 123
          putEvent s2 456

      runResourceT $ do
        s1 <- newUDPEventSink @EventId "0.0.0.0" (show port)
        s2 <- newUDPEventSink @EventId "0.0.0.0" (show port)
        lift $ putEvent s1 123
        lift $ putEvent s2 456

-- | Bind a UDP server socket on any free port and hand the assigned port to
-- the action. Binding port 0 directly avoids probing for a free port with a
-- separate socket, which is racy (and the previous TCP probe said nothing
-- about UDP availability anyway).
withUDPServerSocket :: (ListenSocket -> PortNumber -> IO a) -> IO a
withUDPServerSocket action =
  bracket (serverSocket ("0.0.0.0", 0)) stop $ \socket -> do
    port <- socketPort (listenSocket socket)
    action socket port
