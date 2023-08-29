{-# LANGUAGE TypeApplications #-}

module Hydra.Network.P2P.P2P where

import Hydra.Prelude hiding (Proxy, atomically, get, handle, put)

import Cardano.Binary (toStrictByteString)
import Cardano.Prelude (MVar, async, decodeUtf8, newEmptyMVar, putMVar, readMVar)
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Concurrent.STM (TChan, atomically, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List
import Data.Map ((!))
import qualified Data.Map.Strict as Map
import Data.Text (unpack)
import Hydra.Logging (Tracer, traceWith)
import qualified Hydra.Network as HN
import Network.Socket (withSocketsDo)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPAddr, defaultTCPParameters)

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

remoteServerAddr :: HN.Host -> Tracer IO (P2PLog msg) -> IO EndPointAddress
remoteServerAddr host tracer = withSocketsDo $ do
  etransport <- createTransport (defaultTCPAddr (unpack hostname) (show port)) defaultTCPParameters
  case etransport of
    Left _ -> traceWith tracer (P2PServerDisconnected $ "Failed to create transport") >> error ""
    Right transport -> do
      eendpoint <- newEndPoint transport
      case eendpoint of
        Left _ -> traceWith tracer (P2PServerDisconnected $ "Failed to create endpoint") >> error ""
        Right endpoint -> pure $ address endpoint
 where
  HN.Host{HN.hostname, HN.port} = host

spawnServer :: forall msg. FromCBOR msg => HN.Host -> Tracer IO (P2PLog msg) -> HN.NetworkCallback msg IO -> IO ()
spawnServer host tracer callback = withSocketsDo $ do
  etransport <- createTransport (defaultTCPAddr (unpack hostname) (show port)) defaultTCPParameters
  case etransport of
    Left _ -> traceWith tracer (P2PServerDisconnected $ "Failed to create transport") >> error ""
    Right transport -> do
      eendpoint <- newEndPoint transport
      case eendpoint of
        Left _ -> traceWith tracer (P2PServerDisconnected $ "Failed to create endpoint") >> error ""
        Right endpoint -> do
          serverClosed <- newEmptyMVar
          void $ async $ networkServer endpoint serverClosed
          traceWith tracer (P2PServerConnected $ "Network server started at " ++ show (address endpoint))
 where
  networkServer endpoint serverDone =
    go Map.empty
   where
    go :: Map ConnectionId (MVar Connection) -> IO ()
    go cs = do
      traceWith tracer (P2PServerConnected "Staring server async")
      event <- receive endpoint
      traceWith tracer (P2PServerConnected "After receive event")
      case event of
        ConnectionOpened cid rel addr -> do
          traceWith tracer (P2PServerConnected "Connection opened")
          connMVar <- newEmptyMVar
          void $ async $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
          go (insert cid connMVar cs)
        Received _cid bytes -> do
          void $ async $ do
            case deserialiseFromBytes (fromCBOR @msg) (LBS.fromStrict $ Data.List.head bytes) of
              Left err -> traceWith tracer (P2PDecodeFailure $ show err)
              Right (_, msg) -> traceWith tracer (P2PReceived msg) >> callback msg >> go cs
        ConnectionClosed cid -> do
          traceWith tracer (P2PServerDisconnected "Connection closed")
          void $ async $ do
            conn <- readMVar (cs ! cid)
            close conn
          go (delete cid cs)
        EndPointClosed -> do
          traceWith tracer (P2PServerDisconnected "server exiting")
          putMVar serverDone ()
        ReceivedMulticast{} -> traceWith tracer (P2PServerDisconnected "Multicast")
        ErrorEvent{} -> traceWith tracer (P2PServerDisconnected "ErrorEvent")

  HN.Host{HN.hostname, HN.port} = host

tryConnect :: Int -> HN.Host -> Tracer IO (P2PLog msg) -> EndPointAddress -> IO (Either () Connection)
tryConnect n host tracer serverAddress =
  if n == 0
    then do
      traceWith tracer (P2PClientDisconnected "Client failed to connect!")
      pure $ Left ()
    else do
      Right transport <- createTransport (defaultTCPAddr (unpack hostname) (show port)) defaultTCPParameters
      Right endpoint <- newEndPoint transport
      econn <- connect endpoint serverAddress ReliableOrdered defaultConnectHints
      case econn of
        Left _ -> do
          traceWith tracer (P2PClientDisconnected "Client reconnecting")
          threadDelay 1000000
          tryConnect (n - 1) host tracer serverAddress
        Right conn -> pure $ Right conn
 where
  HN.Host{HN.hostname, HN.port} = host

connectClient :: ToCBOR msg => HN.Host -> Tracer IO (P2PLog msg) -> IO (TChan msg) -> EndPointAddress -> IO ()
connectClient host tracer newChannel serverAddress = withSocketsDo $ do
  econn <- tryConnect 5 host tracer serverAddress
  case econn of
    Left _ -> traceWith tracer (P2PClientConnected $ "Failed to connect to server" <> show serverAddress)
    Right conn -> do
      traceWith tracer (P2PClientConnected "Client connected")
      chan <- newChannel
      traceWith tracer (P2PClientConnected "Reading message from queue")
      readAndSend conn chan
 where
  readAndSend conn chan = do
    msg <- atomically (readTChan chan)
    traceWith tracer (P2PClientConnected $ "Read message " <> C8.unpack (toStrictByteString (toCBOR msg)))
    sendResult <- send conn [toStrictByteString (toCBOR msg)]
    case sendResult of
      Left e -> traceWith tracer (P2PClientConnected "Failed to send a message to the server") >> threadDelay 1000000 >> readAndSend conn chan
      Right _ -> traceWith tracer (P2PClientConnected "Sent message from client") >> threadDelay 1000000 >> readAndSend conn chan

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
    withAsync (spawnServer localHost tracer networkCallback) $ \_ ->
      withAsync (connectAllClients bchan) $ \_ ->
        between $ HN.Network{HN.broadcast = atomically . writeTChan bchan}

  connectAllClients chan =
    forM_ remoteHosts $ \server -> do
      let newChannel = atomically $ dupTChan chan
      serverAddress <- remoteServerAddr server tracer
      connectClient server tracer newChannel serverAddress
