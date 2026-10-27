-- | Compact measurements of current state. Sampling these instead of counting
-- transitions keeps gauges correct after replay, side-loading and rollbacks.
module Hydra.Node.Stats where

import Hydra.Prelude

import Data.Foldable (minimum)
import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.HeadLogic.State
import Hydra.Node.State
import Hydra.Tx (HeadParameters (..), IsTx (..), Snapshot (..), getSnapshot)

data NodeStats = NodeStats
  { synchronized :: Bool
  , phase :: Text
  , processedSlot :: Double
  , queuedInputs :: Natural
  , parkedInputs :: Int
  , delayedInputs :: Integer
  , confirmedNumber :: Natural
  , confirmedVersion :: Natural
  , snapshotPending :: Bool
  , missingSignatures :: Int
  , pendingTransactions :: Int
  , inactiveDeposits :: Int
  , activeDeposits :: Int
  , expiredDeposits :: Int
  , oldestDepositTimestamp :: Double
  , nextDepositDeadline :: Double
  , decommitState :: Text
  , fanoutRemainingOutputs :: Int
  , fanoutMode :: Text
  , contestationDeadlineTimestamp :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

nodeStats :: IsTx tx => Natural -> Int -> Integer -> NodeState tx -> NodeStats
nodeStats queuedInputs parkedInputs delayedInputs ns =
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
    Closed ClosedState{confirmedSnapshot, contestationDeadline} ->
      (withSnapshot (getSnapshot confirmedSnapshot) base)
        { phase = "closed"
        , contestationDeadlineTimestamp = timestamp contestationDeadline
        }
    FanoutProgress PartialFanoutState{confirmedSnapshot, contestationDeadline, remainingOutputs, mode} ->
      (withSnapshot (getSnapshot confirmedSnapshot) base)
        { phase = "fanout"
        , contestationDeadlineTimestamp = timestamp contestationDeadline
        , fanoutRemainingOutputs = length $ outputsOfUTxO remainingOutputs
        , fanoutMode = case mode of
            AutoDrain -> "automatic"
            DistributingSelection{} -> "selected"
            AwaitingSelection -> "deferred"
        }
 where
  pending = Map.elems $ pendingDeposits ns
  count status' = length $ filter (\Deposit{status} -> status == status') pending
  earliest field = maybe 0 timestamp $ viaNonEmpty minimum (map field pending)
  timestamp = realToFrac . utcTimeToPOSIXSeconds
  ChainPointTime{currentSlot = ChainSlot slot} = chainPointTime ns
  withSnapshot :: Snapshot tx -> NodeStats -> NodeStats
  withSnapshot Snapshot{number, version} st = st{confirmedNumber = fromIntegral number, confirmedVersion = fromIntegral version}
  base =
    NodeStats
      { synchronized = case ns of NodeInSync{} -> True; NodeCatchingUp{} -> False
      , phase = "idle"
      , processedSlot = fromIntegral slot
      , queuedInputs
      , parkedInputs
      , delayedInputs
      , confirmedNumber = 0
      , confirmedVersion = 0
      , snapshotPending = False
      , missingSignatures = 0
      , pendingTransactions = 0
      , inactiveDeposits = count Inactive
      , activeDeposits = count Active
      , expiredDeposits = count Expired
      , oldestDepositTimestamp = earliest created
      , nextDepositDeadline = earliest deadline
      , decommitState = "none"
      , fanoutRemainingOutputs = 0
      , fanoutMode = "none"
      , contestationDeadlineTimestamp = 0
      }
