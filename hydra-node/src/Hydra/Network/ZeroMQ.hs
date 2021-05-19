{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.Network.ZeroMQ where

import Cardano.Prelude hiding (atomically, takeMVar)
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Class.MonadSTM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import qualified Data.ByteString.Lazy as LBS
import Hydra.Network
import System.ZMQ4.Monadic (Pub (Pub), Sub (Sub), bind, connect, receive, runZMQ, send, socket, subscribe)

withZeroMQHydraNetwork ::
  Show tx =>
  Serialise tx =>
  Host ->
  [Host] ->
  NetworkCallback tx IO ->
  (HydraNetwork tx IO -> IO ()) ->
  IO ()
withZeroMQHydraNetwork localHost remoteHosts incomingCallback continuation = do
  mvar <- newEmptyTMVarIO
  race_ (runServer mvar) $
    race_ (runClients incomingCallback) $ do
      continuation $ HydraNetwork (atomically . putTMVar mvar)
 where
  toZMQAddress (hostName, port) = "tcp://" <> hostName <> ":" <> port

  runServer queue = runZMQ $ do
    let peerAddresses = map toZMQAddress remoteHosts
    pub <- socket Pub
    forM_ peerAddresses (connect pub)
    forever $ do
      hydraMessage <- liftIO $ atomically $ takeTMVar queue
      let encoded = serialise hydraMessage
      -- TODO use proper tracers
      hPutStrLn @Text stdout $ show localHost <> ": sending " <> show hydraMessage <> ", encoded as: " <> show encoded
      send pub [] $ LBS.toStrict encoded

  runClients callback = runZMQ $ do
    sub <- socket Sub
    subscribe sub ""
    bind sub $ toZMQAddress localHost
    forever $ do
      msg <- receive sub
      hPutStrLn @Text stdout $ show localHost <> ": receiving " <> show msg
      case deserialiseOrFail (LBS.fromStrict msg) of
        Left err -> panic $ "failed to decode msg " <> show msg <> " : " <> show err
        Right hydraMessage -> liftIO $ do
          hPutStrLn @Text stdout $ show localHost <> ": received " <> show hydraMessage
          callback hydraMessage
