{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.HeadLogicTypes where

import Hydra.Prelude

import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (
  ChainEvent (..),
  ChainStateType,
  HeadId,
  HeadParameters (..),
  IsChainState,
  PostChainTx (..),
  PostTxError,
 )
import Hydra.ContestationPeriod
import Hydra.Crypto (
  HydraKey,
  Signature,
  SigningKey,
  VerificationKey,
 )
import Hydra.Ledger (
  ChainSlot,
  IsTx,
  UTxOType,
  ValidationError,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party)
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber)

-- * Types

-- TODO: Move logic up and types down or re-organize using explicit exports

-- | The different events which are processed by the head logic (the "core").
-- Corresponding to each of the "shell" layers, we distinguish between events
-- from the client, the network and the chain.
data Event tx
  = -- | Event received from clients via the "Hydra.API".
    ClientEvent {clientInput :: ClientInput tx}
  | -- | Event received from peers via a "Hydra.Network".
    --
    --  * `ttl` is a simple counter that's decreased every time the event is
    --    reenqueued due to a wait. It's default value is `defaultTTL`
    NetworkEvent {ttl :: TTL, message :: Message tx}
  | -- | Event received from the chain via a "Hydra.Chain".
    OnChainEvent {chainEvent :: ChainEvent tx}
  | -- | Event to re-ingest errors from 'postTx' for further processing.
    PostTxError {postChainTx :: PostChainTx tx, postTxError :: PostTxError tx}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (Event tx)
deriving instance (IsTx tx, IsChainState tx) => Show (Event tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (Event tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (Event tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Event tx)
  where
  arbitrary = genericArbitrary

-- | Analogous to events, the pure head logic "core" can have effects emited to
-- the "shell" layers and we distinguish the same: effects onto the client, the
-- network and the chain.
data Effect tx
  = -- | Effect to be handled by the "Hydra.API", results in sending this 'ServerOutput'.
    ClientEffect {serverOutput :: ServerOutput tx}
  | -- | Effect to be handled by a "Hydra.Network", results in a 'Hydra.Network.broadcast'.
    NetworkEffect {message :: Message tx}
  | -- | Effect to be handled by a "Hydra.Chain", results in a 'Hydra.Chain.postTx'.
    OnChainEffect {postChainTx :: PostChainTx tx}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (Effect tx)
deriving instance (IsTx tx, IsChainState tx) => Show (Effect tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (Effect tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (Effect tx)

instance
  ( IsTx tx
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (Effect tx)
  where
  arbitrary = genericArbitrary

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

-- ** Other types

type TTL = Natural

defaultTTL :: TTL
defaultTTL = 5

-- | Preliminary type for collecting errors occurring during 'update'.
-- TODO: Try to merge this (back) into 'Outcome'.
data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | InvalidSnapshot {expected :: SnapshotNumber, actual :: SnapshotNumber}
  | LedgerError ValidationError
  | RequireFailed RequirementFailure
  | NotOurHead {ourHeadId :: HeadId, otherHeadId :: HeadId}
  deriving stock (Generic)

instance (Typeable tx, Show (Event tx), Show (HeadState tx)) => Exception (LogicError tx)

instance (Arbitrary (Event tx), Arbitrary (HeadState tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)
deriving instance (ToJSON (Event tx), ToJSON (HeadState tx)) => ToJSON (LogicError tx)
deriving instance (FromJSON (Event tx), FromJSON (HeadState tx)) => FromJSON (LogicError tx)

data RequirementFailure
  = ReqSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  | ReqSnNotLeader {requestedSn :: SnapshotNumber, leader :: Party}
  | InvalidMultisignature {multisig :: Text, vkeys :: [VerificationKey HydraKey]}
  | SnapshotAlreadySigned {knownSignatures :: [Party], receivedSignature :: Party}
  | AckSnNumberInvalid {requestedSn :: SnapshotNumber, lastSeenSn :: SnapshotNumber}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary RequirementFailure where
  arbitrary = genericArbitrary

data Outcome tx
  = OnlyEffects {effects :: [Effect tx]}
  | NewState {headState :: HeadState tx, effects :: [Effect tx]}
  | Wait {reason :: WaitReason}
  | Error {error :: LogicError tx}
  deriving stock (Generic)

deriving instance (IsTx tx, IsChainState tx) => Eq (Outcome tx)
deriving instance (IsTx tx, IsChainState tx) => Show (Outcome tx)
deriving instance (IsTx tx, IsChainState tx) => ToJSON (Outcome tx)
deriving instance (IsTx tx, IsChainState tx) => FromJSON (Outcome tx)

instance (IsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (Outcome tx) where
  arbitrary = genericArbitrary

data WaitReason
  = WaitOnNotApplicableTx {validationError :: ValidationError}
  | WaitOnSnapshotNumber {waitingFor :: SnapshotNumber}
  | WaitOnSeenSnapshot
  | WaitOnContestationDeadline
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary WaitReason where
  arbitrary = genericArbitrary

data Environment = Environment
  { party :: Party
  -- ^ This is the p_i from the paper
  , -- NOTE(MB): In the long run we would not want to keep the signing key in
    -- memory, i.e. have an 'Effect' for signing or so.
    signingKey :: SigningKey HydraKey
  , otherParties :: [Party]
  , contestationPeriod :: ContestationPeriod
  }

-- * Snapshot

data SnapshotOutcome tx
  = ShouldSnapshot SnapshotNumber [tx] -- TODO(AB) : should really be a Set (TxId tx)
  | ShouldNotSnapshot NoSnapshotReason
  deriving (Eq, Show, Generic)

data NoSnapshotReason
  = NotLeader SnapshotNumber
  | SnapshotInFlight SnapshotNumber
  | NoTransactionsToSnapshot
  deriving (Eq, Show, Generic)
