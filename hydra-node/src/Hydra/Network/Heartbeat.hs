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

import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (..))
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.HeadLogic (HydraMessage (..), getParty)
import Hydra.Ledger (Party)
import Hydra.Network (Network (..), NetworkCallback, NetworkComponent)

data HeartbeatState = HeartbeatState
  { -- | The map of known 'Connected' parties with the last time they've been "seen".
    -- This is updated when we see a message from another 'Party'
    alive :: Map Party Time
  , -- | The set of known parties which might be 'Disconnected'
    -- This is updated after some time no message has been received from a 'Party'.
    suspected :: Set Party
  , -- | The timestamp of the last sent message.
    lastSent :: Maybe Time
  }
  deriving (Eq)

initialHeartbeatState :: HeartbeatState
initialHeartbeatState = HeartbeatState{alive = mempty, suspected = mempty, lastSent = Nothing}

data Heartbeat msg
  = Message msg
  | Ping Party
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
  Party ->
  NetworkComponent m (Heartbeat (HydraMessage msg)) ->
  NetworkComponent m (HydraMessage msg)
withHeartbeat party withNetwork callback action = do
  heartbeat <- newTVarIO initialHeartbeatState
  withNetwork (updateStateFromIncomingMessages heartbeat callback) $ \network ->
    withAsync (checkHeartbeatState party heartbeat callback network) $ \_ ->
      action (updateStateFromOutboundMessages heartbeat network)

updateStateFromIncomingMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  NetworkCallback (HydraMessage msg) m ->
  NetworkCallback (Heartbeat (HydraMessage msg)) m
updateStateFromIncomingMessages heartbeatState callback = \case
  Message msg -> notifyAlive (getParty msg) >> callback msg
  Ping party -> notifyAlive party
 where
  notifyAlive party = do
    now <- getMonotonicTime
    aliveSet <- alive <$> readTVarIO heartbeatState
    unless (party `Map.member` aliveSet) $ callback (Connected party)
    atomically $
      modifyTVar' heartbeatState $ \s ->
        s
          { alive = Map.insert party now (alive s)
          , suspected = party `Set.delete` suspected s
          }

updateStateFromOutboundMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  Network m (Heartbeat (HydraMessage msg)) ->
  Network m (HydraMessage msg)
updateStateFromOutboundMessages heartbeatState Network{broadcast} =
  Network $ \msg -> do
    now <- getMonotonicTime
    updateLastSent heartbeatState now
    broadcast (Message msg)

updateLastSent :: MonadSTM m => TVar m HeartbeatState -> Time -> m ()
updateLastSent heartbeatState now = atomically (modifyTVar' heartbeatState $ \s -> s{lastSent = Just now})

checkHeartbeatState ::
  ( MonadDelay m
  , MonadSTM m
  , MonadMonotonicTime m
  ) =>
  Party ->
  TVar m HeartbeatState ->
  NetworkCallback (HydraMessage msg) m ->
  Network m (Heartbeat (HydraMessage msg)) ->
  m ()
checkHeartbeatState localhost heartbeatState callback Network{broadcast} =
  forever $ do
    threadDelay heartbeatDelay
    st <- readTVarIO heartbeatState
    now <- getMonotonicTime
    when (shouldSendHeartbeat now st) $ do
      updateLastSent heartbeatState now
      broadcast (Ping localhost)
    suspectedParties <- updateSuspected heartbeatState now
    forM_ suspectedParties $ callback . Disconnected

updateSuspected :: MonadSTM m => TVar m HeartbeatState -> Time -> m (Set Party)
updateSuspected heartbeatState now =
  atomically $ do
    aliveParties <- alive <$> readTVar heartbeatState
    let timedOutParties = Map.filter (livenessTimeout now) aliveParties
    unless (Map.null timedOutParties) $
      modifyTVar' heartbeatState $ \s ->
        s
          { suspected = suspected s <> Map.keysSet timedOutParties
          , alive = aliveParties `Map.difference` timedOutParties
          }
    pure $ Map.keysSet timedOutParties

shouldSendHeartbeat :: Time -> HeartbeatState -> Bool
shouldSendHeartbeat now HeartbeatState{lastSent} =
  maybe True (livenessTimeout now) lastSent

livenessTimeout :: Time -> Time -> Bool
livenessTimeout now seen = diffTime now seen > livenessDelay
