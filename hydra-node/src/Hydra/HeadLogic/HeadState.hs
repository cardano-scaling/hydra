{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The main state of the Hydra protocol state machine. It holds both, the
-- overall protocol state, but also the off-chain 'CoordinatedHeadState'.
--
-- Each of the sub-types (InitialState, OpenState, etc.) contain black-box
-- 'chainState' corresponding to 'OnChainEvent' that has been observed leading
-- to the state.
--
-- Note that rollbacks are currently not fully handled in the head logic and
-- only this internal chain state gets replaced with the "rolled back to"
-- version.
module Hydra.HeadLogic.HeadState where

import Hydra.Prelude

import Hydra.Chain (
  ChainStateType,
  HeadId,
  HeadParameters (..),
  IsChainState,
 )
import Hydra.Crypto (
  Signature,
 )
import Hydra.Ledger (
  ChainSlot,
  IsTx,
  UTxOType,
 )
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber)

-- | Top-level state of Hydra Head.
data HeadState tx
  = Idle (IdleState tx)
  | Initial (InitialState tx)
  | Open (OpenState tx)
  | Closed (ClosedState tx)
  deriving stock (Generic)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (HeadState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (HeadState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (HeadState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (HeadState tx)


-- | Get the chain state in any 'HeadState'.
getChainState :: HeadState tx -> ChainStateType tx
getChainState = \case
  Idle IdleState{chainState} -> chainState
  Initial InitialState{chainState} -> chainState
  Open OpenState{chainState} -> chainState
  Closed ClosedState{chainState} -> chainState

-- | Update the chain state in any 'HeadState'.
setChainState :: ChainStateType tx -> HeadState tx -> HeadState tx
setChainState chainState = \case
  Idle st -> Idle st{chainState}
  Initial st -> Initial st{chainState}
  Open st -> Open st{chainState}
  Closed st -> Closed st{chainState}

-- | An event denoting that some change happened to the `HeadState`.
newtype StateChanged tx =
  -- TODO: This is just a wrapping of previous `NewState` outcome, need to refine to
  -- represent the various changes in the state
  StateChanged {headState :: HeadState tx}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (StateChanged tx)
deriving instance (IsTx tx, IsChainState tx) => Show (StateChanged tx)
deriving newtype instance (IsTx tx, IsChainState tx) => ToJSON (StateChanged tx)
deriving newtype instance (IsTx tx, IsChainState tx) => FromJSON (StateChanged tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (StateChanged tx) where
  arbitrary = genericArbitrary

-- | Applies a `StateChanged` event to a given `HeadState` yielding a new,
-- updated `HeadState`.
updateState :: StateChanged tx -> HeadState tx -> HeadState tx
updateState StateChanged {headState} _ = headState

-- ** Idle

-- | An 'Idle' head only having a chain state with things seen on chain so far.
newtype IdleState tx = IdleState {chainState :: ChainStateType tx}
  deriving (Generic)

deriving instance Eq (ChainStateType tx) => Eq (IdleState tx)
deriving instance Show (ChainStateType tx) => Show (IdleState tx)
deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (IdleState tx)
deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (IdleState tx)

instance (Arbitrary (ChainStateType tx)) => Arbitrary (IdleState tx) where
  arbitrary = genericArbitrary

-- ** Initial

-- | An 'Initial' head which already has an identity and is collecting commits.
data InitialState tx = InitialState
  { parameters :: HeadParameters
  , pendingCommits :: PendingCommits
  , committed :: Committed tx
  , chainState :: ChainStateType tx
  , headId :: HeadId
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (InitialState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (InitialState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (InitialState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (InitialState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (InitialState tx) where
  arbitrary = do
    InitialState
      <$> arbitrary
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
  , currentSlot :: ChainSlot
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (OpenState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (OpenState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (OpenState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (OpenState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (OpenState tx) where
  arbitrary =
    OpenState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Off-chain state of the Coordinated Head protocol.
data CoordinatedHeadState tx = CoordinatedHeadState
  { seenUTxO :: UTxOType tx
  -- ^ The latest UTxO resulting from applying 'seenTxs' to
  -- 'confirmedSnapshot'. Spec: L̂
  , seenTxs :: [tx]
  -- ^ List of seen transactions pending inclusion in a snapshot. Spec: T̂
  , confirmedSnapshot :: ConfirmedSnapshot tx
  -- ^ The latest confirmed snapshot. Spec: U̅, s̅ and σ̅
  , seenSnapshot :: SeenSnapshot tx
  -- ^ Last seen snapshot and signatures accumulator. Spec: Û, ŝ and Σ̂
  }
  deriving stock (Generic)

deriving instance IsTx tx => Eq (CoordinatedHeadState tx)
deriving instance IsTx tx => Show (CoordinatedHeadState tx)
deriving instance IsTx tx => ToJSON (CoordinatedHeadState tx)
deriving instance IsTx tx => FromJSON (CoordinatedHeadState tx)

instance IsTx tx => Arbitrary (CoordinatedHeadState tx) where
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

instance IsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary = genericArbitrary

deriving instance IsTx tx => Eq (SeenSnapshot tx)
deriving instance IsTx tx => Show (SeenSnapshot tx)
deriving instance IsTx tx => ToJSON (SeenSnapshot tx)
deriving instance IsTx tx => FromJSON (SeenSnapshot tx)

-- | Get the last seen snapshot number given a 'SeenSnapshot'.
seenSnapshotNumber :: SeenSnapshot tx -> SnapshotNumber
seenSnapshotNumber = \case
  NoSeenSnapshot -> 0
  LastSeenSnapshot{lastSeen} -> lastSeen
  RequestedSnapshot{lastSeen} -> lastSeen
  SeenSnapshot{snapshot = Snapshot{number}} -> number

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
  }
  deriving (Generic)

deriving instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ClosedState tx)
deriving instance (IsTx tx, Show (ChainStateType tx)) => Show (ClosedState tx)
deriving instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ClosedState tx)
deriving instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ClosedState tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ClosedState tx) where
  arbitrary =
    ClosedState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
