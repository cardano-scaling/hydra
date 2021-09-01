{-# LANGUAGE TypeApplications #-}

module Hydra.Network.ZeroMQ where

import Hydra.Prelude

import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.Monad.Class.MonadSTM (newEmptyTMVarIO, putTMVar, takeTMVar)
import Hydra.Logging (Tracer, traceWith)
import Hydra.Network
import System.ZMQ4.Monadic (
  Pub (Pub),
  Sub (Sub),
  bind,
  connect,
  receive,
  runZMQ,
  send,
  socket,
  subscribe,
 )

data NetworkLog
  = PublisherStarted Host
  | MessageSent Host LByteString
  | LogMessageReceived Host Text
  | SubscribedTo Host [String]
  deriving (Show)

withZeroMQNetwork ::
  (Show msg, ToCBOR msg, FromCBOR msg) =>
  Tracer IO NetworkLog ->
  Host ->
  [Host] ->
  NetworkComponent IO msg ()
withZeroMQNetwork tracer localHost remoteHosts incomingCallback continuation = do
  mvar <- newEmptyTMVarIO
  race_ (runServer mvar) $
    race_ (runClients incomingCallback) $ do
      continuation $
        Network
          { broadcast = atomically . putTMVar mvar
          }
 where
  toZMQAddress Host{hostname, port} = "tcp://" <> toString hostname <> ":" <> show port
  peerAddresses = map toZMQAddress remoteHosts

  runServer queue = runZMQ $ do
    pub <- socket Pub
    bind pub $ toZMQAddress localHost
    liftIO $ traceWith tracer (PublisherStarted localHost)
    forever $ do
      hydraMessage <- liftIO $ atomically $ takeTMVar queue
      let encoded = CBOR.toLazyByteString $ toCBOR hydraMessage
      send pub [] $ toStrict encoded
      liftIO $ traceWith tracer (MessageSent localHost encoded)

  runClients callback = runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    forM_ peerAddresses (connect sub)
    liftIO $ traceWith tracer (SubscribedTo localHost peerAddresses)
    forever $ do
      msg <- receive sub
      case CBOR.deserialiseFromBytes fromCBOR (fromStrict msg) of
        Left err -> error $ "failed to decode msg " <> show msg <> " : " <> show err
        Right (_, hydraMessage) -> liftIO $ do
          traceWith tracer (LogMessageReceived localHost $ show hydraMessage)
          callback hydraMessage
