{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude hiding (id)

import Data.Aeson (Value, object, (.=))
import qualified Data.Map as Map
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Cardano.Api (Tx)
import Hydra.HeadLogic (Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (Envelope))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Node (HydraNodeLog (BeginEvent, EndEvent))

data TraceEvent = TraceEvent
  { timestamp :: UTCTime
  , id :: TxIdType Tx
  , us :: NominalDiffTime
  , event :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

tracePerformance :: Envelope (HydraLog Tx (Message Tx)) -> State (Map Word64 TraceEvent) (Maybe TraceEvent)
tracePerformance envelope = do
  pending <- get
  case envelope of
    (Envelope timestamp _n _txt (Node (BeginEvent _pa eventID (ClientEvent (NewTx tx))))) -> do
      put (Map.insert eventID (TraceEvent{event = "NewTx", timestamp, id = txId tx, us = 0}) pending)
      pure Nothing
    (Envelope ut _n _txt (Node (EndEvent _pa eventID))) ->
      case Map.lookup eventID pending of
        Just e -> do
          put $ Map.delete eventID pending
          pure $ Just $ e{us = 1_000_000 * diffUTCTime ut (timestamp e)}
        Nothing -> pure Nothing
    _ -> pure Nothing
