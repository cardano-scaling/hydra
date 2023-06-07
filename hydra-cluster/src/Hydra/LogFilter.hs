-- | Utility functions to filter and simplify raw logs.
module Hydra.LogFilter where

import Hydra.Prelude

import Data.Aeson (Value, object, (.=))
import qualified Data.Map as Map
import Hydra.API.ClientInput (ClientInput (NewTx))
import Hydra.Cardano.Api (Tx)
import Hydra.HeadLogic (Event (..))
import Hydra.Ledger (IsTx (..))
import Hydra.Logging (Envelope (..))
import Hydra.Logging.Messages (HydraLog (Node))
import Hydra.Network.Message (Message)
import Hydra.Node (HydraNodeLog (BeginEvent, EndEvent))

tracePerformance :: Envelope (HydraLog Tx (Message Tx)) -> State (Map Word64 (TxIdType Tx, UTCTime)) (Maybe Value)
tracePerformance envelope = do
  pending <- get
  case envelope of
    (Envelope ut _n _txt (Node (BeginEvent _pa eventID (ClientEvent (NewTx tx))))) -> do
      put (Map.insert eventID (txId tx, ut) pending)
      pure Nothing
    (Envelope ut _n _txt (Node (EndEvent _pa eventID))) ->
      case Map.lookup eventID pending of
        Just (txid, begin) -> do
          put $ Map.delete eventID pending
          pure $
            Just $
              object
                [ "timestamp" .= begin
                , "id" .= txid
                , "event" .= ("NewTx" :: Text)
                , "us" .= (1_000_000 * diffUTCTime ut begin)
                ]
        Nothing -> pure Nothing
    _ -> pure Nothing
