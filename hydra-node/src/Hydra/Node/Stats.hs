-- | Compact measurements of current state. Sampling these instead of counting
-- transitions keeps gauges correct after replay, side-loading and rollbacks.
module Hydra.Node.Stats where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Hydra.HeadLogic.State
import Hydra.Node.State
import Hydra.Tx (HeadParameters (..), IsTx (..), Snapshot (..), getSnapshot)

data NodeStats = NodeStats
  { synchronized :: Bool
  , phase :: Text
  , queuedInputs :: Natural
  , parkedInputs :: Int
  , confirmedNumber :: Natural
  , snapshotPending :: Bool
  , missingSignatures :: Int
  , pendingTransactions :: Int
  , inactiveDeposits :: Int
  , activeDeposits :: Int
  , expiredDeposits :: Int
  , decommitState :: Text
  , fanoutRemainingOutputs :: Int
  , fanoutMode :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

nodeStats :: IsTx tx => Natural -> Int -> NodeState tx -> NodeStats
nodeStats queuedInputs parkedInputs ns =
  case headState ns of
    Idle{} -> base
    Open OpenState{parameters = HeadParameters{parties}, coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot, seenSnapshot, allTxs, decommitTx}} ->
      let snap = getSnapshot confirmedSnapshot
          missing = case seenSnapshot of
            SeenSnapshot{signatories} -> max 0 (length parties - Map.size signatories)
            RequestedSnapshot{} -> length parties
            _ -> 0
          dc
            | isJust (utxoToDecommit snap) = "approved"
            | isJust decommitTx = "requested"
            | otherwise = "none"
       in (withSnapshot snap base)
            { phase = "open"
            , snapshotPending = snapshotInFlight seenSnapshot
            , missingSignatures = missing
            , pendingTransactions = Map.size allTxs
            , decommitState = dc
            }
    Closed ClosedState{confirmedSnapshot} ->
      (withSnapshot (getSnapshot confirmedSnapshot) base)
        { phase = "closed"
        }
    FanoutProgress PartialFanoutState{confirmedSnapshot, remainingOutputs, mode} ->
      (withSnapshot (getSnapshot confirmedSnapshot) base)
        { phase = "fanout"
        , fanoutRemainingOutputs = length $ outputsOfUTxO remainingOutputs
        , fanoutMode = case mode of
            AutoDrain -> "automatic"
            DistributingSelection{} -> "selected"
            AwaitingSelection -> "deferred"
        }
 where
  pending = Map.elems $ pendingDeposits ns
  count status' = length $ filter (\Deposit{status} -> status == status') pending
  withSnapshot :: Snapshot tx -> NodeStats -> NodeStats
  withSnapshot Snapshot{number} st = st{confirmedNumber = fromIntegral number}
  base =
    NodeStats
      { synchronized = case ns of NodeInSync{} -> True; NodeCatchingUp{} -> False
      , phase = "idle"
      , queuedInputs
      , parkedInputs
      , confirmedNumber = 0
      , snapshotPending = False
      , missingSignatures = 0
      , pendingTransactions = 0
      , inactiveDeposits = count Inactive
      , activeDeposits = count Active
      , expiredDeposits = count Expired
      , decommitState = "none"
      , fanoutRemainingOutputs = 0
      , fanoutMode = "none"
      }
