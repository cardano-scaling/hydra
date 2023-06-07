{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude hiding (id)

import qualified Data.Map as Map
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.API.ServerOutput (ServerOutput (SnapshotConfirmed, TxValid))
import Hydra.Cardano.Api (Tx)
import Hydra.HeadLogic (Effect (ClientEffect, NetworkEffect), Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (Envelope))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
import Hydra.Node (HydraNodeLog (BeginEffect, BeginEvent, EndEffect, EndEvent))
import Hydra.Snapshot (Snapshot (Snapshot))

data Trace
  = TraceEvent
      { timestamp :: UTCTime
      , id :: TxIdType Tx
      , us :: NominalDiffTime
      , event :: Text
      }
  | TraceEffect
      { timestamp :: UTCTime
      , id :: TxIdType Tx
      , us :: NominalDiffTime
      , effect :: Text
      }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data TraceKey
  = EventKey Word64
  | EffectKey Word64 Word32
  deriving stock (Eq, Show, Ord)

tracePerformance :: Envelope (HydraLog Tx (Message Tx)) -> State (Map TraceKey [Trace]) [Trace]
tracePerformance envelope = do
  pending <- get
  case envelope of
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (ClientEvent (NewTx tx))))) -> do
      put (Map.insert (EventKey eventID) [TraceEvent{event = "NewTx", timestamp, id = txId tx, us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _ (ReqTx _p tx))))) -> do
      put (Map.insert (EventKey eventID) [TraceEvent{event = "ReqTx", timestamp, id = txId tx, us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _na (ReqSn _p sn txs))))) -> do
      put (Map.insert (EventKey eventID) (map (\tx -> TraceEvent{event = "ReqSn", timestamp, id = txId tx, us = 0}) txs) pending)
      pure []
    -- (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _na (AckSn _p _ms sn))))) -> do
    --   put (Map.insert eventID [TraceEvent{event = "AckSn", timestamp, id = show (toInteger sn), us = 0}] pending)
    --   pure []
    (Envelope ut _n _txt (Node (EndEvent _pa eventID))) ->
      case Map.lookup (EventKey eventID) pending of
        Just es -> do
          put $ Map.delete (EventKey eventID) pending
          pure $ map (\e -> e{us = 1_000_000 * diffUTCTime ut (timestamp e)}) es
        Nothing -> pure []
    (Envelope timestamp _n _txt (Node (BeginEffect pa eventId effectId (NetworkEffect (ReqTx pa' tx))))) -> do
      put (Map.insert (EffectKey eventId effectId) [TraceEffect{effect = "ReqTx", timestamp, id = txId tx, us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEffect pa eventId effectId (ClientEffect (TxValid hi tx))))) -> do
      put (Map.insert (EffectKey eventId effectId) [TraceEffect{effect = "TxValid", timestamp, id = txId tx, us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEffect pa eventId effectId (ClientEffect (SnapshotConfirmed hi (Snapshot sn utot txs) ms))))) -> do
      put (Map.insert (EffectKey eventId effectId) (map (\tx -> TraceEffect{effect = "SnapshotConfirmed", timestamp, id = txId tx, us = 0}) txs) pending)
      pure []
    (Envelope ut _n _txt (Node (EndEffect _pa eventId effectId))) ->
      case Map.lookup (EffectKey eventId effectId) pending of
        Just es -> do
          put $ Map.delete (EffectKey eventId effectId) pending
          pure $ map (\e -> e{us = 1_000_000 * diffUTCTime ut (timestamp e)}) es
        Nothing -> pure []
    --      (ReqSn pa' sn txs) -> _wj
    _ -> pure []
