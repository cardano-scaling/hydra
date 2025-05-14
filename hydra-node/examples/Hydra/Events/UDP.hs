-- | Event sink example that sends events as JSON via udp.
module Hydra.Events.UDP where

import Hydra.Prelude

import Data.Aeson (encode)
import Hydra.Events (EventSink (..))
import Network.Socket (HostName, ServiceName)
import Network.UDP (clientSocket, send)

-- | Create a new event sink that sends events as JSON.
newUDPEventSink ::
  ToJSON e =>
  -- | Destination host
  HostName ->
  -- | Destination port
  ServiceName ->
  IO (EventSink e IO)
newUDPEventSink host port = do
  socket <- clientSocket host port False
  pure $
    EventSink
      { putEvent = \e ->
          send socket (toStrict $ encode e <> "\n")
      }
