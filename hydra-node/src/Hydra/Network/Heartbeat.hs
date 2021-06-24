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

import Control.Monad.Class.MonadSTM (newTVarIO, readTVar, writeTVar)
import Hydra.HeadLogic (HydraMessage (..))
import Hydra.Network (Host, Network (..), NetworkComponent)

data HeartbeatState
  = SendHeartbeat
  | StopHeartbeat
  deriving (Eq)

-- | Wrap a `NetworkComponent` and handle sending/receiving of heartbeats.
withHeartbeat ::
  ( MonadAsync m
  , MonadDelay m
  ) =>
  Host ->
  NetworkComponent m (HydraMessage tx) ->
  NetworkComponent m (HydraMessage tx)
withHeartbeat localhost withNetwork callback action = do
  heartbeat <- newTVarIO SendHeartbeat
  withNetwork callback $ \network ->
    withAsync (sendHeartbeatFor localhost heartbeat network) $ \_ ->
      action (checkMessages network heartbeat)

checkMessages ::
  MonadSTM m =>
  Network m (HydraMessage tx) ->
  TVar m HeartbeatState ->
  Network m (HydraMessage tx)
checkMessages Network{broadcast} heartbeatState =
  Network $ \msg -> do
    case msg of
      Ping _ -> pure ()
      _ -> atomically (writeTVar heartbeatState StopHeartbeat)
    broadcast msg

sendHeartbeatFor ::
  ( MonadDelay m
  , MonadSTM m
  ) =>
  Host ->
  TVar m HeartbeatState ->
  Network m (HydraMessage tx) ->
  m ()
sendHeartbeatFor localhost heartbeatState Network{broadcast} =
  forever $ do
    threadDelay 0.5
    st <- atomically $ readTVar heartbeatState
    when (st == SendHeartbeat) $ broadcast (Ping localhost)
