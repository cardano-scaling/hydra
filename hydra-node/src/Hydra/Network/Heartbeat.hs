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
--  * If main thread detects that a formerly 'alive' party has not been seen for more than 3s, it is
--    marked as 'suspected' and a 'Disconnected' message is sent to the node.
module Hydra.Network.Heartbeat where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Control.Concurrent.Class.MonadSTM (modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Hydra.Network (Connectivity (..), Host, Network (..), NetworkCallback (..), NetworkComponent)

data HeartbeatState = HeartbeatState
  { alive :: Map Host Time
  -- ^ The map of known 'Connected' parties with the last time they've been "seen".
  -- This is updated when we see a message from another node
  , suspected :: Set Host
  -- ^ The set of known parties which might be 'Disconnected'
  -- This is updated after some time no message has been received from a node.
  }
  deriving stock (Eq)

initialHeartbeatState :: HeartbeatState
initialHeartbeatState = HeartbeatState{alive = mempty, suspected = mempty}

data Heartbeat msg
  = Data Host msg
  | Ping Host
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR msg => ToCBOR (Heartbeat msg) where
  toCBOR = \case
    (Data host hmsg) -> toCBOR (0 :: Int) <> toCBOR host <> toCBOR hmsg
    (Ping host) -> toCBOR (1 :: Int) <> toCBOR host

instance FromCBOR msg => FromCBOR (Heartbeat msg) where
  fromCBOR =
    fromCBOR >>= \case
      (0 :: Int) -> Data <$> fromCBOR <*> fromCBOR
      1 -> Ping <$> fromCBOR
      other -> fail $ "Unknown tag " <> show other <> " trying to deserialise value to Heartbeat"

instance ToCBOR msg => SignableRepresentation (Heartbeat msg) where
  getSignableRepresentation = serialize'

isPing :: Heartbeat msg -> Bool
isPing = \case
  Ping{} -> True
  _ -> False

-- | Delay between each heartbeat check.
--
-- NOTE: This could be made configurable.
heartbeatDelay :: DiffTime
heartbeatDelay = 0.5

-- | Maximal delay between expected and sent heartbeats.
--
-- NOTE: This could be made configurable.
livenessDelay :: DiffTime
livenessDelay = 3

-- | Wrap a lower-level `NetworkComponent` and handle sending/receiving of heartbeats.
--
-- Note that the type of consumed and sent messages can be different.
withHeartbeat ::
  ( MonadAsync m
  , MonadDelay m
  ) =>
  -- | This node's id, used to identify `Heartbeat` messages broadcast to peers.
  Host ->
  -- | Underlying `NetworkComponent` for sending and consuming `Heartbeat` messages.
  NetworkComponent m (Heartbeat inbound) (Heartbeat outbound) a ->
  -- | Returns a network component that can be used to send and consume arbitrary messages.
  -- This layer will take care of peeling out/wrapping messages into `Heartbeat`s.
  NetworkComponent m inbound outbound a
withHeartbeat nodeId withNetwork callback action = do
  heartbeat <- newTVarIO initialHeartbeatState
  lastSent <- newTVarIO Nothing
  withNetwork (updateStateFromIncomingMessages heartbeat callback) $ \network ->
    withAsync (checkRemoteParties heartbeat onConnectivity) $ \_ ->
      withAsync (checkHeartbeatState nodeId lastSent network) $ \_ ->
        action (updateStateFromOutgoingMessages nodeId lastSent network)
 where
  NetworkCallback{onConnectivity} = callback

updateStateFromIncomingMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  NetworkCallback inbound m ->
  NetworkCallback (Heartbeat inbound) m
updateStateFromIncomingMessages heartbeatState callback =
  NetworkCallback
    { deliver = \case
        Data nodeId msg -> notifyAlive nodeId >> deliver msg
        Ping nodeId -> notifyAlive nodeId
    , -- NOTE: We deliberately ignore connnectivity events from the underlying
      -- NetworkComponent here as the heartbeat provides a "better" detection of
      -- connectivity.
      onConnectivity = const $ pure ()
    }
 where
  NetworkCallback{deliver, onConnectivity} = callback

  notifyAlive peer = do
    now <- getMonotonicTime
    aliveSet <- alive <$> readTVarIO heartbeatState
    unless (peer `Map.member` aliveSet) $ onConnectivity (Connected peer)
    atomically $
      modifyTVar' heartbeatState $ \s ->
        s
          { alive = Map.insert peer now (alive s)
          , suspected = peer `Set.delete` suspected s
          }

updateStateFromOutgoingMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  Host ->
  TVar m (Maybe Time) ->
  Network m (Heartbeat outbound) ->
  Network m outbound
updateStateFromOutgoingMessages nodeId lastSent Network{broadcast} =
  Network $ \msg -> do
    now <- getMonotonicTime
    updateLastSent lastSent now
    broadcast (Data nodeId msg)

updateLastSent :: MonadSTM m => TVar m (Maybe Time) -> Time -> m ()
updateLastSent lastSent now = atomically (writeTVar lastSent (Just now))

checkHeartbeatState ::
  ( MonadDelay m
  , MonadSTM m
  ) =>
  Host ->
  TVar m (Maybe Time) ->
  Network m (Heartbeat outbound) ->
  m ()
checkHeartbeatState nodeId lastSent Network{broadcast} =
  forever $ do
    threadDelay heartbeatDelay
    st <- readTVarIO lastSent
    now <- getMonotonicTime
    when (shouldSendHeartbeat now st) $ do
      updateLastSent lastSent now
      broadcast (Ping nodeId)

shouldSendHeartbeat :: Time -> Maybe Time -> Bool
shouldSendHeartbeat now = maybe True (checkTimeout id now)

checkRemoteParties ::
  ( MonadDelay m
  , MonadSTM m
  ) =>
  TVar m HeartbeatState ->
  (Connectivity -> m ()) ->
  m ()
checkRemoteParties heartbeatState onConnectivity =
  forever $ do
    threadDelay (heartbeatDelay * 2)
    now <- getMonotonicTime
    updateSuspected heartbeatState now
      >>= mapM_ (onConnectivity . Disconnected)

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
