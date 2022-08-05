{-# LANGUAGE TypeApplications #-}

-- | An implementation of an application-level failure detector.
-- This module exposes a /Component/ 'withHeartbeat' than can be used to
-- wrap another 'NetworkComponent' and piggy-back on it to send and propagate
-- 'Heartbeat' messages and detect other parties' liveness.
--
-- It is inspired by the /Increasing timeout/ algorithms from the book <https://www.distributedprogramming.net/index.shtml Introduction to Reliable and Secure Distributed Programming>
-- by /Cachin et al./ which is an /Eventually Perfect Failure Detector/ suitable for
-- partially synchronous network settings. It has the following behaviour:
--
--  * It broadcasts a 'Ping' to other parties through the underlying 'Network' implementation
--    if the last message has been sent more than 3s ago
--  * When receiving messages from other parties, it records reception time and notifies underlying
--    node with a 'Connected' message
--  * If new messages are received from 'alive' parties before 3s timeout expires no new 'Connected'
--    message is sent
-- *  If main thread detects that a formerly 'alive' party has not been seen for more than 3s, it is
--    marked as 'suspected' and a 'Disconnected' message is sent to the node.
module Hydra.Network.Heartbeat where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.Network (Host, Network (..), NetworkCallback, NetworkComponent)
import Hydra.Network.Message (Message (Connected, Disconnected))

data HeartbeatState = HeartbeatState
  { -- | The map of known 'Connected' parties with the last time they've been "seen".
    -- This is updated when we see a message from another 'Host'
    alive :: Map Host Time
  , -- | The set of known parties which might be 'Disconnected'
    -- This is updated after some time no message has been received from a 'Host'.
    suspected :: Set Host
  , -- | The timestamp of the last sent message.
    lastSent :: Maybe Time
  }
  deriving (Eq)

initialHeartbeatState :: HeartbeatState
initialHeartbeatState = HeartbeatState{alive = mempty, suspected = mempty, lastSent = Nothing}

data Heartbeat msg
  = Data Host msg
  | Ping Host
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance (ToCBOR msg) => ToCBOR (Heartbeat msg) where
  toCBOR = \case
    (Data host hmsg) -> toCBOR (0 :: Int) <> toCBOR host <> toCBOR hmsg
    (Ping host) -> toCBOR (1 :: Int) <> toCBOR host

instance (FromCBOR msg) => FromCBOR (Heartbeat msg) where
  fromCBOR =
    fromCBOR >>= \case
      (0 :: Int) -> Data <$> fromCBOR <*> fromCBOR
      1 -> Ping <$> fromCBOR
      other -> fail $ "Unknown tag " <> show other <> " trying to deserialise value to Heartbeat"

-- | Delay between each heartbeat check.
heartbeatDelay :: DiffTime
heartbeatDelay = 0.5

-- | Maximal delay between expected and sent heartbeats.
livenessDelay :: DiffTime
livenessDelay = 3

-- | Wrap a `NetworkComponent` and handle sending/receiving of heartbeats.
withHeartbeat ::
  ( MonadAsync m
  , MonadDelay m
  , MonadMonotonicTime m
  ) =>
  Host ->
  NetworkComponent m (Heartbeat (Message tx)) a ->
  NetworkComponent m (Message tx) a
withHeartbeat peer withNetwork callback action = do
  heartbeat <- newTVarIO initialHeartbeatState
  withNetwork (updateStateFromIncomingMessages heartbeat callback) $ \network ->
    withAsync (checkRemoteParties heartbeat callback) $ \_ ->
      withAsync (checkHeartbeatState peer heartbeat network) $ \_ ->
        action (updateStateFromOutgoingMessages peer heartbeat network)

updateStateFromIncomingMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  NetworkCallback (Message tx) m ->
  NetworkCallback (Heartbeat (Message tx)) m
updateStateFromIncomingMessages heartbeatState callback = \case
  Data peer msg -> notifyAlive peer >> callback msg
  Ping peer -> notifyAlive peer
 where
  notifyAlive peer = do
    now <- getMonotonicTime
    aliveSet <- alive <$> readTVarIO heartbeatState
    unless (peer `Map.member` aliveSet) $ callback (Connected peer)
    atomically $
      modifyTVar' heartbeatState $ \s ->
        s
          { alive = Map.insert peer now (alive s)
          , suspected = peer `Set.delete` suspected s
          }

updateStateFromOutgoingMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  Host ->
  TVar m HeartbeatState ->
  Network m (Heartbeat (Message msg)) ->
  Network m (Message msg)
updateStateFromOutgoingMessages localhost heartbeatState Network{broadcast, peers} =
  Network bcast peers
 where
  bcast = \msg -> do
    now <- getMonotonicTime
    updateLastSent heartbeatState now
    broadcast (Data localhost msg)

updateLastSent :: MonadSTM m => TVar m HeartbeatState -> Time -> m ()
updateLastSent heartbeatState now = atomically (modifyTVar' heartbeatState $ \s -> s{lastSent = Just now})

checkHeartbeatState ::
  ( MonadDelay m
  , MonadSTM m
  , MonadMonotonicTime m
  ) =>
  Host ->
  TVar m HeartbeatState ->
  Network m (Heartbeat (Message msg)) ->
  m ()
checkHeartbeatState localhost heartbeatState Network{broadcast} =
  forever $ do
    threadDelay heartbeatDelay
    st <- readTVarIO heartbeatState
    now <- getMonotonicTime
    when (shouldSendHeartbeat now st) $ do
      updateLastSent heartbeatState now
      broadcast (Ping localhost)

shouldSendHeartbeat :: Time -> HeartbeatState -> Bool
shouldSendHeartbeat now HeartbeatState{lastSent} =
  maybe True (checkTimeout id now) lastSent

checkRemoteParties ::
  ( MonadDelay m
  , MonadSTM m
  , MonadMonotonicTime m
  ) =>
  TVar m HeartbeatState ->
  NetworkCallback (Message msg) m ->
  m ()
checkRemoteParties heartbeatState callback =
  forever $ do
    threadDelay (heartbeatDelay * 2)
    now <- getMonotonicTime
    updateSuspected heartbeatState now
      >>= mapM_ (callback . Disconnected)

updateSuspected :: MonadSTM m => TVar m HeartbeatState -> Time -> m (Set Host)
updateSuspected heartbeatState now =
  atomically $ do
    aliveParties <- alive <$> readTVar heartbeatState
    let timedOutParties = Map.filter (checkTimeout (2 *) now) aliveParties
    unless (Map.null timedOutParties) $
      modifyTVar' heartbeatState $ \s ->
        s
          { suspected = suspected s <> Map.keysSet timedOutParties
          , alive = aliveParties `Map.difference` timedOutParties
          }
    pure $ Map.keysSet timedOutParties

checkTimeout :: (DiffTime -> DiffTime) -> Time -> Time -> Bool
checkTimeout delayFn now seen = diffTime now seen > delayFn livenessDelay
