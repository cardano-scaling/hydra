{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State where

import Hydra.Prelude

import Data.Map qualified as Map
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.Tx (
  HeadId,
  HeadParameters,
  HeadSeed,
  IsTx (..),
  Party,
 )
import Hydra.Tx.Crypto (Signature)
import Hydra.Tx.IsTx (ArbitraryIsTx)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot,
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
 )
import Test.QuickCheck (recursivelyShrink)

type PendingDeposits tx = Map (TxIdType tx) (Deposit tx)

-- FIXME: move to a dedicated module (maybe with deposits too?)
data NodeState tx = NodeState
  { headState :: HeadState tx
  , pendingDeposits :: PendingDeposits tx
  -- ^ Pending deposits as observed on chain.
  -- TODO: could even move the chain state here (also see todo below)
  -- , chainState :: ChainStateType tx
  , currentPoint :: ChainPointType tx
  }
  deriving stock (Generic)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx), Arbitrary (ChainPointType tx)) => Arbitrary (NodeState tx) where
  arbitrary = genericArbitrary

deriving stock instance (IsTx tx, Eq (ChainStateType tx), Eq (ChainPointType tx)) => Eq (NodeState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx), Show (ChainPointType tx)) => Show (NodeState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx), ToJSON (ChainPointType tx)) => ToJSON (NodeState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx), FromJSON (ChainPointType tx)) => FromJSON (NodeState tx)

initNodeState :: IsChainState tx => ChainStateType tx -> NodeState tx
initNodeState chainState =
  NodeState
    { headState = Idle IdleState{chainState}
    , pendingDeposits = mempty
    , currentPoint = chainStatePoint chainState
    }

-- | The main state of the Hydra protocol state machine. It holds both, the
-- overall protocol state, but also the off-chain 'CoordinatedHeadState'.
--
-- Each of the sub-types (InitialState, OpenState, etc.) contain a black-box
-- 'chainState' corresponding to the 'ChainEvent' that has been observed leading
-- to the state.
--
-- Note that rollbacks are currently not fully handled in the head logic and
-- only this internal chain state gets replaced with the "rolled back to"
-- version.
--
-- TODO: chainState would actually not be needed in the HeadState anymore as we
-- do not persist the 'HeadState' and not access it in the HeadLogic either.
data HeadState tx
  = Idle (IdleState tx)
  | Initial (InitialState tx)
  | Open (OpenState tx)
  | Closed (ClosedState tx)
  deriving stock (Generic)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (HeadState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (HeadState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (HeadState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (HeadState tx)

-- | Update the chain state in any 'HeadState'.
setChainState :: ChainStateType tx -> HeadState tx -> HeadState tx
setChainState chainState = \case
  Idle st -> Idle st{chainState}
  Initial st -> Initial st{chainState}
  Open st -> Open st{chainState}
  Closed st -> Closed st{chainState}

-- | Get the chain state in any 'HeadState'.
getChainState :: HeadState tx -> ChainStateType tx
getChainState = \case
  Idle IdleState{chainState} -> chainState
  Initial InitialState{chainState} -> chainState
  Open OpenState{chainState} -> chainState
  Closed ClosedState{chainState} -> chainState

-- | Get the head parameters in any 'HeadState'.
getHeadParameters :: HeadState tx -> Maybe HeadParameters
getHeadParameters = \case
  Idle _ -> Nothing
  Initial InitialState{parameters} -> Just parameters
  Open OpenState{parameters} -> Just parameters
  Closed ClosedState{parameters} -> Just parameters

-- ** Idle

-- | An 'Idle' head only having a chain state with things seen on chain so far.
newtype IdleState tx = IdleState {chainState :: ChainStateType tx}
  deriving stock (Generic)

deriving stock instance Eq (ChainStateType tx) => Eq (IdleState tx)
deriving stock instance Show (ChainStateType tx) => Show (IdleState tx)
deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (IdleState tx)
deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (IdleState tx)

instance Arbitrary (ChainStateType tx) => Arbitrary (IdleState tx) where
  arbitrary = genericArbitrary

-- ** Initial

-- | An 'Initial' head which already has an identity and is collecting commits.
data InitialState tx = InitialState
  { parameters :: HeadParameters
  , pendingCommits :: PendingCommits
  , committed :: Committed tx
  , chainState :: ChainStateType tx
  , headId :: HeadId
  , headSeed :: HeadSeed
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (InitialState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (InitialState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (InitialState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (InitialState tx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (InitialState tx) where
  arbitrary = do
    InitialState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

type PendingCommits = Set Party

type Committed tx = Map Party (UTxOType tx)

-- ** Open

-- | An 'Open' head with a 'CoordinatedHeadState' tracking off-chain
-- transactions.
data OpenState tx = OpenState
  { parameters :: HeadParameters
  , coordinatedHeadState :: CoordinatedHeadState tx
  , chainState :: ChainStateType tx
  , headId :: HeadId
  , headSeed :: HeadSeed
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (OpenState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (OpenState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (OpenState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (OpenState tx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (OpenState tx) where
  arbitrary =
    OpenState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Off-chain state of the Coordinated Head protocol.
data CoordinatedHeadState tx = CoordinatedHeadState
  { localUTxO :: UTxOType tx
  -- ^ The latest UTxO resulting from applying 'localTxs' to
  -- 'confirmedSnapshot'. Spec: L̂
  , localTxs :: [tx]
  -- ^ List of transactions applied locally and pending inclusion in a snapshot.
  -- Ordering in this list is important as transactions are added in order of
  -- application. Spec: T̂
  , allTxs :: !(Map.Map (TxIdType tx) tx)
  -- ^ Map containing all the transactions ever seen by this node and not yet
  -- included in a snapshot. Spec: Tall
  , confirmedSnapshot :: ConfirmedSnapshot tx
  -- ^ The latest confirmed snapshot. Spec: S̅
  , seenSnapshot :: SeenSnapshot tx
  -- ^ Last seen snapshot and signatures accumulator. Spec: Û, ŝ and Σ̂
  , currentDepositTxId :: Maybe (TxIdType tx)
  -- ^ Current/next deposit to incrementally commit. Spec: Uα
  -- TODO: update in spec: Uα -> tx^#α
  , decommitTx :: Maybe tx
  -- ^ Pending decommit transaction. Spec: txω
  , version :: SnapshotVersion
  -- ^ Last open state version as observed on chain. Spec: ̂v
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (CoordinatedHeadState tx)
deriving stock instance IsTx tx => Show (CoordinatedHeadState tx)
deriving anyclass instance IsTx tx => ToJSON (CoordinatedHeadState tx)
deriving anyclass instance IsTx tx => FromJSON (CoordinatedHeadState tx)

instance ArbitraryIsTx tx => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

-- | Data structure to help in tracking whether we have seen or requested a
-- ReqSn already and if seen, the signatures we collected already.
data SeenSnapshot tx
  = -- | Never saw a ReqSn.
    NoSeenSnapshot
  | -- | No snapshot in flight with last seen snapshot number as given.
    LastSeenSnapshot {lastSeen :: SnapshotNumber}
  | -- | ReqSn was sent out and it should be considered already in flight.
    RequestedSnapshot
      { lastSeen :: SnapshotNumber
      , requested :: SnapshotNumber
      }
  | -- | ReqSn for given snapshot was received.
    SeenSnapshot
      { snapshot :: Snapshot tx
      , signatories :: Map Party (Signature (Snapshot tx))
      -- ^ Collected signatures and so far.
      }
  deriving stock (Generic)

instance ArbitraryIsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

deriving stock instance IsTx tx => Eq (SeenSnapshot tx)
deriving stock instance IsTx tx => Show (SeenSnapshot tx)
deriving anyclass instance IsTx tx => ToJSON (SeenSnapshot tx)
deriving anyclass instance IsTx tx => FromJSON (SeenSnapshot tx)

-- | Get the last seen snapshot number given a 'SeenSnapshot'.
seenSnapshotNumber :: SeenSnapshot tx -> SnapshotNumber
seenSnapshotNumber = \case
  NoSeenSnapshot -> 0
  LastSeenSnapshot{lastSeen} -> lastSeen
  RequestedSnapshot{lastSeen} -> lastSeen
  SeenSnapshot{snapshot = Snapshot{number}} -> number

-- | A deposit tracked by the protocol. The 'DepositStatus' determines whether
-- it may be used for an incremental commit or not.
data Deposit tx = Deposit
  { headId :: HeadId
  , deposited :: UTxOType tx
  , created :: UTCTime
  , deadline :: UTCTime
  , status :: DepositStatus
  }
  deriving (Generic)

deriving stock instance IsTx tx => Eq (Deposit tx)
deriving stock instance IsTx tx => Show (Deposit tx)
deriving anyclass instance IsTx tx => ToJSON (Deposit tx)
deriving anyclass instance IsTx tx => FromJSON (Deposit tx)

instance ArbitraryIsTx tx => Arbitrary (Deposit tx) where
  arbitrary = genericArbitrary
  shrink = recursivelyShrink

data DepositStatus = Inactive | Active | Expired
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

instance Arbitrary DepositStatus where
  arbitrary = genericArbitrary
  shrink = genericShrink

-- ** Closed

-- | An 'Closed' head with an current candidate 'ConfirmedSnapshot', which may
-- be contested before the 'contestationDeadline'.
data ClosedState tx = ClosedState
  { parameters :: HeadParameters
  , confirmedSnapshot :: ConfirmedSnapshot tx
  , contestationDeadline :: UTCTime
  , readyToFanoutSent :: Bool
  -- ^ Tracks whether we have informed clients already about being
  -- 'ReadyToFanout'.
  , chainState :: ChainStateType tx
  , headId :: HeadId
  , headSeed :: HeadSeed
  , version :: SnapshotVersion
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ClosedState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (ClosedState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ClosedState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ClosedState tx)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ClosedState tx) where
  arbitrary =
    ClosedState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
