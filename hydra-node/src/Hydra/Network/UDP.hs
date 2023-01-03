{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | An experimental network component for Hydra node that uses UDP.
module Hydra.Network.UDP where

import Hydra.Prelude

import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (unpack)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network (Host (..), Network (..), NetworkCallback, NetworkComponent)
import Network.Socket (
  Socket,
  SocketType (Datagram),
  addrAddress,
  addrFamily,
  bind,
  close,
  connect,
  defaultProtocol,
  getAddrInfo,
  socket,
 )
import Network.Socket.ByteString (recv, sendAll)

type PeersResolver m a = (a -> m [Host])

data UDPLog msg
  = UDPLog
  | UDPReceived msg
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withUDPNetwork ::
  (FromCBOR msg, ToCBOR msg) =>
  Tracer IO (UDPLog msg) ->
  -- | The address to bind the server on.
  Host ->
  -- | An `IO` action to "resolve" the peers to send a message to.
  PeersResolver IO msg ->
  NetworkComponent IO msg a
withUDPNetwork tracer localhost peersResolver callback action =
  withAsync (udpServer tracer localhost callback) $ \_ ->
    action (udpClient peersResolver)

udpClient :: ToCBOR msg => PeersResolver IO msg -> Network IO msg
udpClient peersResolver =
  Network $ \msg ->
    let payload = toStrictByteString $ toCBOR msg
     in peersResolver msg >>= traverse_ (sendUDP payload)

sendUDP :: ByteString -> Host -> IO ()
sendUDP payload Host{hostname, port} = do
  addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
  case addrinfos of
    (serveraddr : _) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      connect sock (addrAddress serveraddr)
      -- TODO: chunk the payload if too large. In theory, UDP messages can be several
      -- 10s of KBs large but in practice, routers can drop messages larger than ~512 bytes
      -- so it's necessary to chunk and reassemble larger messages.
      sendAll sock payload
      close sock
    _ -> error "TODO: what if address is not resolved"

udpServer ::
  forall msg.
  FromCBOR msg =>
  Tracer IO (UDPLog msg) ->
  Host ->
  NetworkCallback msg IO ->
  IO ()
udpServer tracer Host{hostname, port} callback = do
  addrinfos <- getAddrInfo Nothing (Just $ unpack hostname) (Just $ show port)
  case addrinfos of
    (serveraddr : _) -> do
      sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
      bind sock (addrAddress serveraddr)
      forever (receiveMsg sock)
    _ -> error "TODO: what if address is not resolved"
 where
  receiveMsg sock = do
    -- TODO: reassemble large messages
    bytes <- recv sock 4096
    case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
      Left err -> error $ "TODO: handle error: " <> show err
      Right (_, msg) -> traceWith tracer (UDPReceived msg) >> callback msg
