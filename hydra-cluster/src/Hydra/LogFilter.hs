{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude hiding (id)

import qualified Data.Map as Map
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Cardano.Api (Tx)
import Hydra.HeadLogic (Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (Envelope))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Network.Message (Message (AckSn, ReqSn, ReqTx))
import Hydra.Node (HydraNodeLog (BeginEvent, EndEvent))

data TraceEvent = TraceEvent
  { timestamp :: UTCTime
  , id :: Text
  , us :: NominalDiffTime
  , event :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

tracePerformance :: Envelope (HydraLog Tx (Message Tx)) -> State (Map Word64 [TraceEvent]) [TraceEvent]
tracePerformance envelope = do
  pending <- get
  case envelope of
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (ClientEvent (NewTx tx))))) -> do
      put (Map.insert eventID [TraceEvent{event = "NewTx", timestamp, id = show (txId tx), us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _ (ReqTx _p tx))))) -> do
      put (Map.insert eventID [TraceEvent{event = "ReqTx", timestamp, id = show (txId tx), us = 0}] pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _na (ReqSn _p sn txs))))) -> do
      put (Map.insert eventID (map (\tx -> TraceEvent{event = "ReqSn", timestamp, id = show (toInteger sn) <> ":" <> show (txId tx), us = 0}) txs) pending)
      pure []
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (NetworkEvent _na (AckSn _p _ms sn))))) -> do
      put (Map.insert eventID [TraceEvent{event = "AckSn", timestamp, id = show (toInteger sn), us = 0}] pending)
      pure []
    (Envelope ut _n _txt (Node (EndEvent _pa eventID))) ->
      case Map.lookup eventID pending of
        Just es -> do
          put $ Map.delete eventID pending
          pure $ map (\e -> e{us = 1_000_000 * diffUTCTime ut (timestamp e)}) es
        Nothing -> pure []
    _ -> pure []
