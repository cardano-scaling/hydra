{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude hiding (id)

import qualified Data.Map as Map
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.HeadLogic (Effect (..), Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Network.Message (Message (..))
import Hydra.Node (HydraNodeLog (..))
import Hydra.Snapshot (Snapshot (..))

-- | A trace of an event or effect for a specific transaction.
data Trace tx
  = TraceEvent
      { timestamp :: UTCTime
      -- ^ The starting point in time for this event.
      , txid :: TxIdType tx
      -- ^ The transaction id this event applies to.
      , us :: NominalDiffTime
      -- ^ The duration of the event, expressed as a number of
      -- seconds with a $10^12$ precision.
      , event :: Text
      -- ^ A string identifying this event.
      }
  | TraceEffect
      { timestamp :: UTCTime
      -- ^ The starting point in time for this effect.
      , txid :: TxIdType tx
      -- ^ The transaction id this effect applies to.
      , us :: NominalDiffTime
      -- ^ The duration of the effect, expressed as a number of
      -- seconds with a $10^12$ precision.
      , effect :: Text
      -- ^ A string identifying this effect.
      }
  deriving stock (Generic)

deriving instance (IsTx tx) => Eq (Trace tx)
deriving instance (IsTx tx) => Show (Trace tx)
deriving instance (IsTx tx) => ToJSON (Trace tx)

data TraceKey
  = EventKey Word64
  | EffectKey Word64 Word32
  deriving stock (Eq, Show, Ord)

-- | Compute duration of some `Event`s and `Effect`s from logs.
--
-- This function is meant to be used with a `sequence` in order to traverse a stream of
-- log entries and output list of `Trace` as begin/end pairs are found and identified.
-- Each `Trace` emitted is tied to a specific transaction id which provides an easy way to
-- identify in which part of their journey through Hydra transactions are spending time.
--
-- It currently compute duration of:
--  * `NewTx`, `ReqTx`, `ReqSn` events,
--  * `ReqTx`,  `TxValid` and `SnapshotConfirmed` effects.
--
-- NOTE: Some potential improvements
--  * Move this function to `Monitoring` and expose an histogram kind of metric for each type of event / effect
--  * Handle more events, in particular the `AckSn` which is slightly problematic as it does not contain
--    a direct reference to a transaction id so we would need to carry around a secondary map to keep
--    track of this link.
tracePerformance :: IsTx tx => Envelope (HydraLog tx (Message tx)) -> State (Map TraceKey [Trace tx]) [Trace tx]
tracePerformance envelope = do
  pending <- get
  case envelope of
    Envelope{timestamp, message = Node BeginEvent{eventId, event = ClientEvent (NewTx tx)}} -> do
      put (Map.insert (EventKey eventId) [TraceEvent{event = "NewTx", timestamp, txid = txId tx, us = 0}] pending)
      pure []
    Envelope{timestamp, message = Node BeginEvent{eventId, event = NetworkEvent{message = ReqTx{transaction}}}} -> do
      put (Map.insert (EventKey eventId) [TraceEvent{event = "ReqTx", timestamp, txid = txId transaction, us = 0}] pending)
      pure []
    Envelope{timestamp, message = Node BeginEvent{eventId, event = NetworkEvent{message = ReqSn{transactionIds}}}} -> do
      put (Map.insert (EventKey eventId) (map (\txid -> TraceEvent{event = "ReqSn", timestamp, txid, us = 0}) transactionIds) pending)
      pure []
    Envelope{timestamp, message = Node EndEvent{eventId}} ->
      case Map.lookup (EventKey eventId) pending of
        Just es -> do
          put $ Map.delete (EventKey eventId) pending
          pure $ map (computeDuration timestamp) es
        Nothing -> pure []
    Envelope{timestamp, message = Node BeginEffect{eventId, effectId, effect = NetworkEffect ReqTx{transaction}}} -> do
      put (Map.insert (EffectKey eventId effectId) [TraceEffect{effect = "ReqTx", timestamp, txid = txId transaction, us = 0}] pending)
      pure []
    Envelope{timestamp, message = Node BeginEffect{eventId, effectId, effect = ClientEffect TxValid{transaction}}} -> do
      put (Map.insert (EffectKey eventId effectId) [TraceEffect{effect = "TxValid", timestamp, txid = txId transaction, us = 0}] pending)
      pure []
    Envelope{timestamp, message = Node BeginEffect{eventId, effectId, effect = ClientEffect SnapshotConfirmed{snapshot = Snapshot{confirmed}}}} -> do
      put (Map.insert (EffectKey eventId effectId) (map (\txid -> TraceEffect{effect = "SnapshotConfirmed", timestamp, txid, us = 0}) confirmed) pending)
      pure []
    Envelope{timestamp, message = Node EndEffect{eventId, effectId}} ->
      case Map.lookup (EffectKey eventId effectId) pending of
        Just es -> do
          put $ Map.delete (EffectKey eventId effectId) pending
          pure $ fmap (computeDuration timestamp) es
        Nothing -> pure []
    _ -> pure []
 where
  computeDuration :: UTCTime -> Trace tx -> Trace tx
  computeDuration endTime = \case
    e@TraceEvent{timestamp = startTime} -> e{us = 1_000_000 * diffUTCTime endTime startTime}
    e@TraceEffect{timestamp = startTime} -> e{us = 1_000_000 * diffUTCTime endTime startTime}
