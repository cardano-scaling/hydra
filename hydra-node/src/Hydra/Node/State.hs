{-# LANGUAGE UndecidableInstances #-}

module Hydra.Node.State (module Hydra.Node.State, module Deposit) where

import Hydra.Prelude

import Data.Map.Strict qualified as Map
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Records (HasField (..))
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState (..), chainStateSlot)
import Hydra.HeadLogic.Deposit as Deposit (Deposit (..), DepositStatus (..), TrackedDeposit (..))
import Hydra.HeadLogic.Deposit qualified as DepositEntry
import Hydra.HeadLogic.Snapshot.Settlement (finalizedDepositTxId)
import Hydra.HeadLogic.State (
  CoordinatedHeadState (CoordinatedHeadState, finalizedCommit),
  HeadState (Idle),
  IdleState (..),
  OpenState (OpenState, coordinatedHeadState, headId),
 )
import Hydra.Tx (
  HeadId,
  IsTx (..),
 )

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

type TrackedDeposits tx = Map (TxIdType tx) (TrackedDeposit tx)

-- | Track the given deposits with a fresh lifecycle: recorded at slot 0
-- and unconsumed.
trackedFromPending :: PendingDeposits tx -> TrackedDeposits tx
trackedFromPending = fmap (\deposit -> TrackedDeposit{deposit, recordedAt = ChainSlot 0, consumedAt = Nothing})

-- | Deposits pending as observed on chain: the tracked deposits not consumed
-- yet.
pendingDeposits :: NodeState tx -> PendingDeposits tx
pendingDeposits =
  Map.mapMaybe (\TrackedDeposit{deposit, consumedAt} -> deposit <$ guard (isNothing consumedAt)) . deposits

-- | Derived view, so record-dot access keeps working across the tracked
-- representation.
instance view ~ PendingDeposits tx => HasField "pendingDeposits" (NodeState tx) view where
  getField = pendingDeposits

-- | Keep pending deposits belonging to this Head, including any restored by
-- rollback. Inactive and expired deposits remain in the view.
depositsForHead :: HeadId -> PendingDeposits tx -> PendingDeposits tx
depositsForHead targetHeadId =
  Map.filter (\Deposit{headId} -> headId == targetHeadId)

-- | 'depositsForHead', excluding the deposit claimed by the retained finalized
-- increment. Rollback can restore that deposit, but new snapshots must not
-- claim it again. Activation, expiry and capacity checks happen separately.
depositsForHeadWithoutFinalizedCommit :: IsTx tx => OpenState tx -> PendingDeposits tx -> PendingDeposits tx
depositsForHeadWithoutFinalizedCommit OpenState{headId, coordinatedHeadState = CoordinatedHeadState{finalizedCommit}} =
  maybe id Map.delete (finalizedDepositTxId finalizedCommit) . depositsForHead headId

-- | Record a newly observed deposit at the given slot. Re-recording an id (its
-- deposit transaction re-landed after a rollback) starts a fresh lifecycle.
recordDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> Deposit tx -> NodeState tx -> NodeState tx
recordDeposit slot depositTxId deposit nodeState =
  nodeState
    { deposits =
        Map.alter (DepositEntry.record slot deposit) depositTxId $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Mark a deposit consumed at the given slot: its increment or recover was
-- observed on chain. Re-consuming (the consuming transaction re-landed after a
-- rollback) re-stamps the slot, so a rollback of the re-landed transaction
-- still resurfaces the deposit.
consumeDeposit :: IsTx tx => ChainSlot -> TxIdType tx -> NodeState tx -> NodeState tx
consumeDeposit slot depositTxId nodeState =
  nodeState
    { deposits =
        Map.alter (DepositEntry.consume slot) depositTxId $
          pruneConsumedDeposits slot (deposits nodeState)
    }

-- | Rewind the deposit view to the given (rolled back) slot, see
-- 'TrackedDeposit'.
rollbackDeposits :: ChainSlot -> NodeState tx -> NodeState tx
rollbackDeposits slot nodeState =
  nodeState{deposits = Map.mapMaybe (DepositEntry.rollback slot . Just) (deposits nodeState)}

-- | Drop consumed deposits beyond 'depositRetentionHorizon': no rollback can
-- resurface them anymore, so retaining them would only grow persisted state
-- with every deposit ever settled. Called on the deposit write paths, which is
-- enough because only deposit churn creates consumed entries. Unconsumed
-- deposits are never pruned — an expired deposit stays recoverable
-- indefinitely.
pruneConsumedDeposits :: ChainSlot -> TrackedDeposits tx -> TrackedDeposits tx
pruneConsumedDeposits (ChainSlot slot) =
  Map.filter (\TrackedDeposit{consumedAt} -> maybe True (> cutoff) consumedAt)
 where
  cutoff =
    case depositRetentionHorizon of
      ChainSlot horizon -> ChainSlot (if slot > horizon then slot - horizon else 0)

-- | How long consumed deposits are retained for rollbacks: sized to cover the
-- deepest rollback Cardano can produce (the security parameter k = 2160
-- blocks, roughly 12 hours at one block per 20 slots) with a three-fold
-- margin.
depositRetentionHorizon :: ChainSlot
depositRetentionHorizon = ChainSlot 129600

data ChainPointTime = ChainPointTime
  { currentSlot :: ChainSlot
  -- ^ Latest chain slot as observed on chain.
  , currentChainTime :: UTCTime
  -- ^ Time corresponding to `currentSlot`.
  , drift :: NominalDiffTime
  -- ^ Time difference with current system wall-clock measured in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR ChainPointTime where
  toCBOR = genericToCBOR

instance FromCBOR ChainPointTime where
  fromCBOR = genericFromCBOR

data NodeState tx
  = -- | Normal operation of the node where it is connected and has a recent
    -- view of the chain.
    NodeInSync
      { headState :: HeadState tx
      , deposits :: TrackedDeposits tx
      -- ^ Deposits as observed on chain, with their L1 lifecycle (see
      -- 'TrackedDeposit'); read the pending view via 'pendingDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  | -- | Node is catching up on its view of the chain and should behave
    -- differently.
    NodeCatchingUp
      { headState :: HeadState tx
      , deposits :: TrackedDeposits tx
      -- ^ Deposits as observed on chain, with their L1 lifecycle (see
      -- 'TrackedDeposit'); read the pending view via 'pendingDeposits'.
      -- TODO: could even move the chain state here (also see todo below)
      -- , chainState :: ChainStateType tx
      , chainPointTime :: ChainPointTime
      }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (NodeState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (NodeState tx)

instance IsChainState tx => ToCBOR (NodeState tx) where
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (NodeState tx) where
  fromCBOR = genericFromCBOR

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeCatchingUp
    { headState = Idle IdleState{chainState}
    , deposits = mempty
    , chainPointTime = initialChainPointTime chainState
    }

initialChainPointTime :: IsChainState tx => ChainStateType tx -> ChainPointTime
initialChainPointTime chainState =
  ChainPointTime
    { currentSlot = chainStateSlot chainState
    , currentChainTime = initialChainTime
    , drift = 0
    }

initialChainTime :: UTCTime
initialChainTime = posixSecondsToUTCTime 0

data SyncedStatus = InSync | CatchingUp
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance ToCBOR SyncedStatus where
  toCBOR = genericToCBOR

instance FromCBOR SyncedStatus where
  fromCBOR = genericFromCBOR

syncedStatus :: NodeState tx -> SyncedStatus
syncedStatus NodeInSync{} = InSync
syncedStatus NodeCatchingUp{} = CatchingUp
