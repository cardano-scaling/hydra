{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.Network.ZeroMQ where

import Cardano.Prelude hiding (atomically, takeMVar)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad.Class.MonadSTM (atomically, newEmptyTMVarIO, putTMVar, takeTMVar)
import qualified Data.ByteString.Lazy as LBS
import Hydra.Network
import System.ZMQ4.Monadic (Pub (Pub), Sub (Sub), bind, connect, receive, runZMQ, send, socket, subscribe)

withZeroMQHydraNetwork ::
  Host ->
  [Host] ->
  NetworkCallback IO ->
  (HydraNetwork IO -> IO ()) ->
  IO ()
withZeroMQHydraNetwork localHost remoteHosts incomingCallback continuation = do
  mvar <- newEmptyTMVarIO
  race_ (runServer localHost mvar) $
    race_ (runClients remoteHosts incomingCallback) $ do
      continuation $ HydraNetwork (atomically . putTMVar mvar)
 where
  toZMQAddress (hostName, port) = "tcp://" <> hostName <> ":" <> port

  runServer host queue = runZMQ $ do
    let hostAddress = toZMQAddress host
    pub <- socket Pub
    bind pub hostAddress
    forever $ do
      hydraMessage <- liftIO $ atomically $ takeTMVar queue
      let encoded = serialise hydraMessage
      -- TODO use proper tracers
      hPutStrLn @Text stdout $ show host <> ": sending " <> show hydraMessage <> ", encoded as: " <> show encoded
      send pub [] $ LBS.toStrict encoded

  runClients peers callback = runZMQ $ do
    let peerAddresses = map toZMQAddress peers
    sub <- socket Sub
    subscribe sub ""
    forM_ peerAddresses (connect sub)
    forever $ do
      msg <- receive sub
      hPutStrLn @Text stdout $ show localHost <> ": receiving " <> show msg
      case deserialiseOrFail (LBS.fromStrict msg) of
        Left err -> panic $ "failed to decode msg " <> show msg <> " : " <> show err
        Right hydraMessage -> liftIO $ do
          hPutStrLn @Text stdout $ show localHost <> ": received " <> show hydraMessage
          callback hydraMessage
