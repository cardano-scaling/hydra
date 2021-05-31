{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ZeroMQ where

import Cardano.Prelude hiding (atomically, takeMVar)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Class.MonadSTM (atomically, modifyTVar', newEmptyTMVarIO, newTVarIO, putTMVar, readTVar, takeTMVar)
import qualified Data.ByteString.Lazy as LBS
import Data.String (String)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network
import System.ZMQ4.Monadic (EventMsg (Connected), EventType (ConnectedEvent), Pub (Pub), Sub (Sub), bind, connect, monitor, receive, runZMQ, send, socket, subscribe)

data NetworkLog
  = PublisherStarted Host
  | MessageSent LBS.ByteString
  | MessageReceived Text
  | SubscribedTo [String]
  deriving (Show)

withZeroMQHydraNetwork ::
  Show tx =>
  Serialise tx =>
  Host ->
  [Host] ->
  Tracer IO NetworkLog ->
  NetworkCallback tx IO ->
  (HydraNetwork tx IO -> IO ()) ->
  IO ()
withZeroMQHydraNetwork localHost remoteHosts tracer incomingCallback continuation = do
  mvar <- newEmptyTMVarIO
  networkStatus <- newTVarIO (length remoteHosts)
  race_ (runServer mvar) $
    race_ (runClients networkStatus incomingCallback) $ do
      continuation $
        HydraNetwork
          { broadcast = atomically . putTMVar mvar
          , isNetworkReady = readTVar networkStatus <&> (== 0)
          }
 where
  toZMQAddress (hostName, port) = "tcp://" <> hostName <> ":" <> show port
  peerAddresses = map toZMQAddress remoteHosts

  runServer queue = runZMQ $ do
    pub <- socket Pub
    bind pub $ toZMQAddress localHost
    liftIO $ traceWith tracer (PublisherStarted localHost)
    forever $ do
      hydraMessage <- liftIO $ atomically $ takeTMVar queue
      let encoded = serialise hydraMessage
      send pub [] $ LBS.toStrict encoded
      liftIO $ traceWith tracer (MessageSent encoded)

  runClients networkStatus callback = runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    forM_ peerAddresses (connect sub)

    fn <- monitor [ConnectedEvent] sub
    liftIO $ loop networkStatus fn >> void (fn False)

    liftIO $ traceWith tracer (SubscribedTo peerAddresses)
    forever $ do
      msg <- receive sub
      case deserialiseOrFail (LBS.fromStrict msg) of
        Left err -> panic $ "failed to decode msg " <> show msg <> " : " <> show err
        Right hydraMessage -> liftIO $ do
          traceWith tracer (MessageReceived $ show hydraMessage)
          callback hydraMessage

  loop networkStatus fn = do
    fn True >>= \case
      (Just (Connected _ _)) -> do
        peersToConnect <- atomically $ do
          modifyTVar' networkStatus pred
          readTVar networkStatus
        unless (peersToConnect == 0) $ loop networkStatus fn
      _ -> loop networkStatus fn
