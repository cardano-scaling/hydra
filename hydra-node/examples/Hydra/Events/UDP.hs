-- | Event sink example that sends events as JSON via udp.
module Hydra.Events.UDP where

import "hydra-prelude" Hydra.Prelude
import "aeson" Data.Aeson (encode)
import "network" Network.Socket (HostName, ServiceName)
import "network-udp" Network.UDP (UDPSocket, clientSocket, close, send)
import "resourcet" Control.Monad.Trans.Resource (MonadResource, allocate)

import Hydra.Events (EventSink (..))

-- | Create a new event sink that sends events as JSON.
--
-- This is the so-called bracket or with-style pattern, which can ensure that
-- resources are free'd after the 'EventSink' is not needed anymore.
--
-- See 'newUDPEventSink' for a variant that uses 'MonadResource' to clean up.
withUDPEventSink ::
  ToJSON e =>
  -- | Destination host
  HostName ->
  -- | Destination port
  ServiceName ->
  -- | Continuation with EventSink
  (EventSink e IO -> IO a) ->
  IO a
withUDPEventSink host port action =
  -- Make sure to free resources (file descriptor)
  bracket (clientSocket host port False) close $ \socket ->
    action $ EventSink{putEvent = sendData socket}

-- | Create a new event sink that sends events as JSON.
--
-- This uses 'MonadResource' to register and clean up resources. See
-- 'withUDPEventSink' for a variant that uses 'bracket' and does not require
-- 'resourcet'.
newUDPEventSink ::
  (ToJSON e, MonadResource m) =>
  -- | Destination host
  HostName ->
  -- | Destination port
  ServiceName ->
  m (EventSink e IO)
newUDPEventSink host port = do
  -- Make sure to free resources (file descriptor)
  (_, socket) <- allocate (clientSocket host port False) close
  pure EventSink{putEvent = sendData socket}

sendData :: ToJSON e => UDPSocket -> e -> IO ()
sendData socket e =
  send socket (toStrict $ encode e <> "\n")
