{-# LANGUAGE TypeApplications #-}

module Hydra.Network.P2P.P2P where

import Hydra.Prelude hiding (Proxy, atomically, get, handle, put)

import Cardano.Binary (toStrictByteString)
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List
import Data.Text (unpack)
import Hydra.Logging (Tracer, traceWith)
import qualified Hydra.Network as HN
import Network.Socket (
  AddrInfo (addrFlags, addrSocketType),
  AddrInfoFlag (..),
  HostName,
  ServiceName,
  Socket,
  SocketOption (ReuseAddr),
  SocketType (Stream),
  accept,
  addrAddress,
  bind,
  close,
  connect,
  defaultHints,
  getAddrInfo,
  gracefulClose,
  listen,
  openSocket,
  setCloseOnExecIfNeeded,
  setSocketOption,
  withFdSocket,
  withSocketsDo,
 )
import Network.Socket.ByteString (recv, sendAll)
import Cardano.Prelude (async)

data P2PLog msg
  = P2PLog
  | P2PServerConnected String
  | P2PServerDisconnected String
  | P2PClientConnected String
  | P2PClientDisconnected String
  | P2PReceived msg
  | P2PDecodeFailure String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

runServer :: Tracer IO (P2PLog msg) -> Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runServer tracer mhost port server = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) onClose loop
 where
  onClose s = do
    traceWith tracer (P2PServerDisconnected $ show s)
    close s
  resolve = do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    serverAddresses <- getAddrInfo (Just hints) mhost (Just port)
    case serverAddresses of
      [addr] -> return addr
      _ -> error "Could not resolve server address"
  open addr = bracketOnError (openSocket addr) onClose $ \sock -> do
    setSocketOption sock ReuseAddr 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1024
    return sock
  loop sock = forever $
    bracketOnError (accept sock) (close . fst) $
      \(conn, _peer) ->
        do
          traceWith tracer (P2PServerConnected $ "Server connected: " <> show sock)
          server conn
          `finally` gracefulClose conn 5000

runClient :: Tracer IO (P2PLog msg) -> HostName -> ServiceName -> (Socket -> IO a) -> IO a
runClient tracer host port client = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) onClose client
 where
  onClose s = do
    traceWith tracer (P2PClientDisconnected $ "Closed client connection: " <> show s)
    close s
  resolve = do
    let hints = defaultHints{addrSocketType = Stream}
    Data.List.head <$> getAddrInfo (Just hints) (Just host) (Just port)
  open addr = bracketOnError (openSocket addr) onClose $ \sock -> do
    traceWith tracer (P2PClientConnected $ "Trying to open addr client: " <> show addr)
    connect sock $ addrAddress addr
    return sock

connectClient :: ToCBOR msg => HN.Host -> Tracer IO (P2PLog msg) -> IO (TChan msg) -> IO ()
connectClient host tracer newChannel = do
  traceWith tracer (P2PClientConnected $ "Client trying to connect: " <> show hostname)
  runClient tracer (unpack hostname) (show port) $ \s -> do
    traceWith tracer (P2PClientConnected $ "Connection established to " ++ (show hostname <> ":" <> show port))
    chan <- newChannel
    msg <- atomically (readTChan chan)
    sendAll s (toStrictByteString (toCBOR msg))
 where
  HN.Host{HN.hostname, HN.port} = host

spawnServer :: forall msg. FromCBOR msg => HN.Host -> Tracer IO (P2PLog msg) -> HN.NetworkCallback msg IO -> IO ()
spawnServer host tracer callback = do
  traceWith tracer (P2PServerConnected $ "Spawn server on: " <> show hostname)
  runServer tracer (Just $ unpack hostname) (show port) talk
 where
  HN.Host{HN.hostname, HN.port} = host
  talk connectionSocket = do
    traceWith tracer (P2PClientConnected $ "TCP connection established from " ++ (show hostname <> ":" <> show port))
    bytes <- recv connectionSocket 1024
    unless (BS.null bytes) $
      case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict bytes) of
        Left err -> traceWith tracer (P2PDecodeFailure $ show err)
        Right (_, msg) -> traceWith tracer (P2PReceived msg) >> callback msg

withP2PNetwork ::
  (ToCBOR msg, FromCBOR msg) =>
  Tracer IO (P2PLog msg) ->
  HN.Host ->
  [HN.Host] ->
  HN.NetworkCallback msg IO ->
  (HN.Network IO msg -> IO ()) ->
  IO ()
withP2PNetwork tracer localHost remoteHosts networkCallback between = withSocketsDo $ do
  doIt
 where
  doIt = do
    bchan <- newBroadcastTChanIO
    withAsync (spawnServer localHost tracer networkCallback) $ \_ahandle ->
      withAsync (connectAllClients bchan) $ \_cHandle ->
        void $ async (between $ HN.Network{HN.broadcast = atomically . writeTChan bchan})

  connectAllClients chan =
    forM_ remoteHosts $ \client -> do
      let newChannel = atomically $ dupTChan chan
      connectClient client tracer newChannel
