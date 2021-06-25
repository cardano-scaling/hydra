{-# LANGUAGE TypeApplications #-}

-- | A naive implementation of an application-level Heartbeat
-- This module exposes a /Component/ 'withHeartbeat' than can be used to
-- wrap another 'NetworkComponent' and piggy-back on it to send and propagate
-- 'HeartbeatMessage's.
--
-- Its current behavior is very simple: When it starts, it sends a 'Heartbeat' message
-- with its own identifier every 500ms, until the wrapped component sends another message.
-- `Heartbeat` messages received from other components are simply propagated to the
-- wrapped component.
module Hydra.Network.Heartbeat where

import Hydra.Prelude

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (..))
import Control.Monad.Class.MonadSTM (newTVarIO, writeTVar)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Network (Host, Network (..), NetworkCallback, NetworkComponent)

data HeartbeatState
  = SendHeartbeat
  | StopHeartbeat
  deriving (Eq)

data Heartbeat msg
  = Message msg
  | Ping Host
  deriving (Eq, Show)

instance (ToCBOR msg) => ToCBOR (Heartbeat msg) where
  toCBOR = \case
    (Message hmsg) -> toCBOR (0 :: Int) <> toCBOR hmsg
    (Ping host) -> toCBOR (1 :: Int) <> toCBOR host

instance (FromCBOR msg) => FromCBOR (Heartbeat msg) where
  fromCBOR =
    fromCBOR >>= \case
      (0 :: Int) -> Message <$> fromCBOR
      1 -> Ping <$> fromCBOR
      other -> fail $ "Unknown tag " <> show other <> " trying to deserialise value to Heartbeat"

-- | Wrap a `NetworkComponent` and handle sending/receiving of heartbeats.
withHeartbeat ::
  ( MonadAsync m
  , MonadDelay m
  ) =>
  Host ->
  NetworkComponent m (Heartbeat (HydraMessage msg)) ->
  NetworkComponent m (HydraMessage msg)
withHeartbeat localhost withNetwork callback action = do
  heartbeat <- newTVarIO SendHeartbeat
  withNetwork (fromHeartbeat callback) $ \network ->
    withAsync (sendHeartbeatFor localhost heartbeat network) $ \_ ->
      action (checkMessages network heartbeat)

fromHeartbeat :: NetworkCallback (HydraMessage msg) m -> NetworkCallback (Heartbeat (HydraMessage msg)) m
fromHeartbeat callback = \case
  Message msg -> callback msg
  Ping host -> callback (Connected host)

checkMessages ::
  MonadSTM m =>
  Network m (Heartbeat (HydraMessage msg)) ->
  TVar m HeartbeatState ->
  Network m (HydraMessage msg)
checkMessages Network{broadcast} heartbeatState =
  Network $ \msg -> do
    atomically (writeTVar heartbeatState StopHeartbeat)
    broadcast (Message msg)

sendHeartbeatFor ::
  ( MonadDelay m
  , MonadSTM m
  ) =>
  Host ->
  TVar m HeartbeatState ->
  Network m (Heartbeat (HydraMessage msg)) ->
  m ()
sendHeartbeatFor localhost heartbeatState Network{broadcast} =
  forever $ do
    threadDelay 0.5
    st <- readTVarIO heartbeatState
    when (st == SendHeartbeat) $ broadcast (Ping localhost)
