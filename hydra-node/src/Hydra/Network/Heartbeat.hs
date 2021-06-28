{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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
import Control.Monad.Class.MonadSTM (modifyTVar', newTVarIO)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Hydra.HeadLogic (HydraMessage (..))
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
  withNetwork (fromHeartbeat heartbeat callback) $ \network ->
    withAsync (sendHeartbeatFor party heartbeat callback network) $ \_ ->
      action (checkMessages heartbeat network)

fromHeartbeat ::
  (Monad m, MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  NetworkCallback (HydraMessage msg) m ->
  NetworkCallback (Heartbeat (HydraMessage msg)) m
fromHeartbeat heartbeatState callback = \case
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

getParty :: HydraMessage msg -> Party
getParty =
  \case
    (ReqTx p _) -> p
    (ReqSn p _ _) -> p
    (AckSn p _ _) -> p
    (Connected p) -> p
    (Disconnected p) -> p

checkMessages ::
  (MonadSTM m, MonadMonotonicTime m) =>
  TVar m HeartbeatState ->
  Network m (Heartbeat (HydraMessage msg)) ->
  Network m (HydraMessage msg)
checkMessages heartbeatState Network{broadcast} =
  Network $ \msg -> do
    now <- getMonotonicTime
    atomically (modifyTVar' heartbeatState $ \s -> s{lastSent = Just now})
    broadcast (Message msg)

sendHeartbeatFor ::
  ( MonadDelay m
  , MonadSTM m
  , MonadMonotonicTime m
  ) =>
  Party ->
  TVar m HeartbeatState ->
  NetworkCallback (HydraMessage msg) m ->
  Network m (Heartbeat (HydraMessage msg)) ->
  m ()
sendHeartbeatFor localhost heartbeatState callback Network{broadcast} =
  forever $ do
    threadDelay 0.5
    st <- readTVarIO heartbeatState
    now <- getMonotonicTime
    when (shouldSendHeartbeat now st) $ broadcast (Ping localhost)
    suspectedParties <- updateSuspected heartbeatState now
    forM_ suspectedParties $ callback . Disconnected

updateSuspected :: MonadSTM m => TVar m HeartbeatState -> Time -> m (Set Party)
updateSuspected heartbeatState now =
  atomically $ do
    aliveParties <- alive <$> readTVar heartbeatState
    let timedOutParties = Map.filter (\seen -> diffTime now seen > 3) aliveParties
    unless (Map.null timedOutParties) $
      modifyTVar' heartbeatState $ \s ->
        s
          { suspected = suspected s <> Map.keysSet timedOutParties
          , alive = aliveParties `Map.difference` timedOutParties
          }
    pure $ Map.keysSet timedOutParties

shouldSendHeartbeat :: Time -> HeartbeatState -> Bool
shouldSendHeartbeat now HeartbeatState{lastSent} =
  maybe True (\seen -> diffTime now seen > 3) lastSent
