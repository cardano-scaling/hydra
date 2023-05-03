{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implements the Head Protocol's /state machine/ as a /pure function/.
--
-- The protocol is described in two parts in the [Hydra paper](https://iohk.io/en/research/library/papers/hydrafast-isomorphic-state-channels/)
--
--     * One part detailing how the Head deals with /client input/.
--     * Another part describing how the Head reacts to /network messages/ from peers.
--     * A third part detailing the /On-Chain Verification (OCV)/ protocol, i.e. the abstract "smart contracts" that are need to provide on-chain security.
--
-- This module is about the first two parts, while the "Hydra.Contract.Head" module in 'hydra-plutus' covers the third part.
module Hydra.HeadLogic where

import Hydra.Prelude

import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (getField)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Chain (
  ChainEvent (..),
  ChainSlot,
  ChainStateType,
  HeadId,
  HeadParameters (..),
  IsChainState (chainStateSlot),
  OnChainTx (..),
  PostChainTx (..),
  PostTxError,
 )
import Hydra.ContestationPeriod
import Hydra.Crypto (
  HydraKey,
  Signature,
  SigningKey,
  aggregateInOrder,
  sign,
  verifyMultiSignature,
 )
import Hydra.Ledger (
  IsTx,
  Ledger (..),
  UTxOType,
  ValidationError,
  applyTransactions,
 )
import Hydra.Network.Message (Message (..))
import Hydra.Party (Party (vkey))
import Hydra.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber, getSnapshot)
import Test.QuickCheck (oneof)

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
    OnChainEffect {chainState :: ChainStateType tx, postChainTx :: PostChainTx tx}
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
-- It is a recursive data structure, where 'previousRecoverableState' fields
-- record the state before the latest 'OnChainEvent' that has been observed.
-- On-Chain events are indeed only __eventually__ immutable and the application
-- state may be rolled back at any time (with a decreasing probability as the
-- time pass).
--
-- Thus, leverage functional immutable data-structure, we build a recursive
-- structure of states which we can easily navigate backwards when needed (see
-- 'Rollback' and 'rollback').
--
-- Note that currently, rolling back to a previous recoverable state eliminates
-- any off-chain events (e.g. transactions) that happened after that state. This
-- is particularly important for anything following the transition to
-- 'OpenState' since this is where clients may start submitting transactions. In
-- practice, clients should not send transactions right way but wait for a
-- certain grace period to minimize the risk.
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
  , previousRecoverableState :: HeadState tx
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
      <*> oneof
        [ Idle <$> arbitrary
        , Initial <$> arbitrary
        ]

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
  , previousRecoverableState :: HeadState tx
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
      <*> (Initial <$> arbitrary)

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
  , previousRecoverableState :: HeadState tx
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
      <*> oneof
        [ Open <$> arbitrary
        , Closed <$> arbitrary
        ]

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
  | RequireFailed Text
  deriving stock (Generic)

instance (Typeable tx, Show (Event tx), Show (HeadState tx)) => Exception (LogicError tx)

instance (Arbitrary (Event tx), Arbitrary (HeadState tx)) => Arbitrary (LogicError tx) where
  arbitrary = genericArbitrary

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)
deriving instance (ToJSON (Event tx), ToJSON (HeadState tx)) => ToJSON (LogicError tx)
deriving instance (FromJSON (Event tx), FromJSON (HeadState tx)) => FromJSON (LogicError tx)

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

-- * The Coordinated Head protocol

-- ** Opening the Head

-- | Client request to init the head. This leads to an init transaction on chain,
-- containing the head parameters.
--
-- __Transition__: 'IdleState' → 'IdleState'
onIdleClientInit ::
  Environment ->
  IdleState tx ->
  Outcome tx
onIdleClientInit env st =
  OnlyEffects [OnChainEffect{chainState, postChainTx = InitTx parameters}]
 where
  parameters =
    HeadParameters
      { contestationPeriod
      , parties = party : otherParties
      }

  Environment{party, otherParties, contestationPeriod} = env

  IdleState{chainState} = st

-- | Observe an init transaction, initialize parameters in an 'InitialState' and
-- notify clients that they can now commit.
--
-- __Transition__: 'IdleState' → 'InitialState'
onIdleChainInitTx ::
  IdleState tx ->
  -- | New chain state.
  ChainStateType tx ->
  [Party] ->
  ContestationPeriod ->
  HeadId ->
  Outcome tx
onIdleChainInitTx idleState newChainState parties contestationPeriod headId =
  NewState
    ( Initial
        InitialState
          { parameters = HeadParameters{contestationPeriod, parties}
          , pendingCommits = Set.fromList parties
          , committed = mempty
          , previousRecoverableState = Idle idleState
          , chainState = newChainState
          , headId
          }
    )
    [ClientEffect $ HeadIsInitializing headId (fromList parties)]

-- | Client request to commit a UTxO entry to the head. Provided the client
-- hasn't committed yet, this leads to a commit transaction on-chain containing
-- that UTxO entry.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientCommit ::
  Environment ->
  InitialState tx ->
  ClientInput tx ->
  Outcome tx
onInitialClientCommit env st clientInput =
  case clientInput of
    (Commit utxo)
      -- REVIEW: Is 'canCommit' something we want to handle here or have the OCV
      -- deal with it?
      | canCommit -> OnlyEffects [OnChainEffect{chainState, postChainTx = CommitTx party utxo}]
    _ -> OnlyEffects [ClientEffect $ CommandFailed clientInput]
 where
  canCommit = party `Set.member` pendingCommits

  InitialState{pendingCommits, chainState} = st

  Environment{party} = env

-- | Observe a commit transaction and record the committed UTxO in the state.
-- Also, if this is the last commit to be observed, post a collect-com
-- transaction on-chain.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialChainCommitTx ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  -- | Comitting party
  Party ->
  -- | Committed UTxO
  UTxOType tx ->
  Outcome tx
onInitialChainCommitTx st newChainState pt utxo =
  NewState newState $
    notifyClient
      : [postCollectCom | canCollectCom]
 where
  newState =
    Initial
      InitialState
        { parameters
        , pendingCommits = remainingParties
        , committed = newCommitted
        , previousRecoverableState = Initial st
        , chainState = newChainState
        , headId
        }

  newCommitted = Map.insert pt utxo committed

  notifyClient = ClientEffect $ Committed headId pt utxo

  postCollectCom =
    OnChainEffect
      { chainState = newChainState
      , postChainTx = CollectComTx $ fold newCommitted
      }

  canCollectCom = null remainingParties

  remainingParties = Set.delete pt pendingCommits

  InitialState{parameters, pendingCommits, committed, headId} = st

-- | Client request to abort the head. This leads to an abort transaction on
-- chain, reimbursing already committed UTxOs.
--
-- __Transition__: 'InitialState' → 'InitialState'
onInitialClientAbort ::
  Monoid (UTxOType tx) =>
  InitialState tx ->
  Outcome tx
onInitialClientAbort st =
  OnlyEffects [OnChainEffect{chainState, postChainTx = AbortTx{utxo = fold committed}}]
 where
  InitialState{chainState, committed} = st

-- | Observe an abort transaction by switching the state and notifying clients
-- about it.
--
-- __Transition__: 'InitialState' → 'IdleState'
onInitialChainAbortTx ::
  Monoid (UTxOType tx) =>
  -- | New chain state
  ChainStateType tx ->
  Committed tx ->
  HeadId ->
  Outcome tx
onInitialChainAbortTx newChainState committed headId =
  NewState
    (Idle IdleState{chainState = newChainState})
    [ClientEffect $ HeadIsAborted{headId, utxo = fold committed}]

-- | Observe a collectCom transaction. We initialize the 'OpenState' using the
-- head parameters from 'IdleState' and construct an 'InitialSnapshot' holding
-- @u0@ from the committed UTxOs.
--
-- __Transition__: 'InitialState' → 'OpenState'
onInitialChainCollectTx ::
  (Monoid (UTxOType tx)) =>
  InitialState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onInitialChainCollectTx st newChainState =
  NewState
    ( Open
        OpenState
          { parameters
          , coordinatedHeadState =
              CoordinatedHeadState u0 mempty initialSnapshot NoSeenSnapshot
          , previousRecoverableState = Initial st
          , chainState = newChainState
          , headId
          }
    )
    [ClientEffect $ HeadIsOpen{headId, utxo = u0}]
 where
  u0 = fold committed

  initialSnapshot = InitialSnapshot u0

  -- TODO: Do we want to check whether this even matches our local state? For
  -- example, we do expect `null remainingParties` but what happens if it's
  -- untrue?
  InitialState{parameters, committed, headId} = st

-- ** Off-chain protocol

-- | Client request to ingest a new transaction into the head.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientNewTx ::
  Environment ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenClientNewTx env tx =
  OnlyEffects [NetworkEffect $ ReqTx party tx]
 where
  Environment{party} = env

-- | Process a transaction request ('ReqTx') from a party.
--
-- We apply this transaction to the seen utxo (ledger state). If not applicable,
-- we wait and retry later. If it applies, this yields an updated seen ledger
-- state. Then, we check whether we are the leader for the next snapshot and
-- emit a snapshot request 'ReqSn' including this transaction if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqTx ::
  Environment ->
  Ledger tx ->
  OpenState tx ->
  TTL ->
  -- | The transaction to be submitted to the head.
  tx ->
  Outcome tx
onOpenNetworkReqTx env ledger st ttl tx =
  -- Spec: wait L̂ ◦ tx ̸= ⊥ combined with L̂ ← L̂ ◦ tx
  case applyTransactions seenUTxO [tx] of
    Left (_, err)
      | ttl <= 0 ->
          OnlyEffects [ClientEffect $ TxInvalid headId seenUTxO tx err]
      | otherwise -> Wait $ WaitOnNotApplicableTx err
    Right utxo' ->
      NewState
        ( Open
            st
              { coordinatedHeadState =
                  coordinatedHeadState
                    { seenTxs = seenTxs <> [tx]
                    , seenUTxO = utxo'
                    }
              }
        )
        [ClientEffect $ TxValid headId tx]
        & emitSnapshot env
 where
  Ledger{applyTransactions} = ledger

  CoordinatedHeadState{seenTxs, seenUTxO} = coordinatedHeadState

  OpenState{coordinatedHeadState, headId} = st

-- | Process a snapshot request ('ReqSn') from party.
--
-- This checks that s is the next snapshot number and that the party is
-- responsible for leading that snapshot. Then, we potentially wait until the
-- previous snapshot is confirmed (no snapshot is in flight), before we apply
-- (or wait until applicable) the requested transactions to the last confirmed
-- snapshot. Only then, we start tracking this new "seen" snapshot, compute a
-- signature of it and send the corresponding 'AckSn' to all parties. Finally,
-- the pending transaction set gets pruned to only contain still applicable
-- transactions.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkReqSn ::
  IsTx tx =>
  Environment ->
  Ledger tx ->
  OpenState tx ->
  -- | Party which sent the ReqSn.
  Party ->
  -- | Requested snapshot number.
  SnapshotNumber ->
  -- | List of transactions to snapshot.
  [tx] ->
  Outcome tx
onOpenNetworkReqSn env ledger st otherParty sn requestedTxs =
  -- TODO: Verify the request is signed by (?) / comes from the leader
  -- (Can we prove a message comes from a given peer, without signature?)

  -- Spec: require s = ŝ + 1 and leader(s) = j
  requireReqSn $
    -- Spec: wait s̅ = ŝ
    waitNoSnapshotInFlight $
      -- Spec: wait U̅ ◦ T /= ⊥ combined with Û ← Ū̅ ◦ T
      waitApplyTxs $ \u -> do
        -- NOTE: confSn == seenSn == sn here
        let nextSnapshot = Snapshot (confSn + 1) u requestedTxs
        -- Spec: σᵢ
        let snapshotSignature = sign signingKey nextSnapshot
        -- Spec: T̂ ← {tx | ∀tx ∈ T̂ , Û ◦ tx ≠ ⊥} and L̂ ← Û ◦ T̂
        let (seenTxs', seenUTxO') = pruneTransactions u
        NewState
          ( Open
              st
                { coordinatedHeadState =
                    coordinatedHeadState
                      { seenSnapshot = SeenSnapshot nextSnapshot mempty
                      , seenTxs = seenTxs'
                      , seenUTxO = seenUTxO'
                      }
                }
          )
          [NetworkEffect $ AckSn party snapshotSignature sn]
 where
  requireReqSn continue =
    if sn == seenSn + 1 && isLeader parameters otherParty sn
      then continue
      else Error $ RequireFailed "requireReqSn"

  waitNoSnapshotInFlight continue =
    if confSn == seenSn
      then continue
      else Wait $ WaitOnSnapshotNumber seenSn

  -- XXX: Wait for these transactions to apply is actually not needed. They must
  -- be applicable already. This is a bit of a precursor for only submitting
  -- transaction ids/hashes .. which we really should do.
  waitApplyTxs cont =
    case applyTransactions ledger confirmedUTxO requestedTxs of
      Left (_, err) ->
        Wait $ WaitOnNotApplicableTx err
      Right u -> cont u

  pruneTransactions utxo = do
    foldr go ([], utxo) seenTxs
   where
    go tx (txs, u) =
      case applyTransactions ledger u [tx] of
        Left (_, _) -> (txs, u)
        Right u' -> (txs <> [tx], u')

  confSn = case confirmedSnapshot of
    InitialSnapshot{} -> 0
    ConfirmedSnapshot{snapshot = Snapshot{number}} -> number

  seenSn = seenSnapshotNumber seenSnapshot

  confirmedUTxO = case confirmedSnapshot of
    InitialSnapshot{initialUTxO} -> initialUTxO
    ConfirmedSnapshot{snapshot = Snapshot{utxo}} -> utxo

  CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} = coordinatedHeadState

  OpenState{parameters, coordinatedHeadState} = st

  Environment{party, signingKey} = env

-- | Process a snapshot acknowledgement ('AckSn') from a party.
--
-- We do require that the is from the last seen or next expected snapshot, and
-- potentially wait wait for the corresponding 'ReqSn' before proceeding. If the
-- party hasn't sent us a signature yet, we store it. Once a signature from each
-- party has been collected, we aggregate a multi-signature and verify it is
-- correct. If everything is fine, the snapshot can be considered as the latest
-- confirmed one. Similar to processing a 'ReqTx', we check whether we are
-- leading the next snapshot and craft a corresponding 'ReqSn' if needed.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenNetworkAckSn ::
  IsTx tx =>
  Environment ->
  OpenState tx ->
  -- | Party which sent the AckSn.
  Party ->
  -- | Signature from other party.
  Signature (Snapshot tx) ->
  -- | Snapshot number of this AckSn.
  SnapshotNumber ->
  Outcome tx
onOpenNetworkAckSn env openState otherParty snapshotSignature sn =
  -- TODO: verify authenticity of message and whether otherParty is part of the head
  -- Spec: require s ∈ {ŝ, ŝ + 1}
  requireValidAckSn $ do
    -- Spec: wait ŝ = s
    waitOnSeenSnapshot $ \snapshot sigs -> do
      -- Spec: (j,.) ∉ ̂Σ
      requireNotSignedYet sigs $ do
        let sigs' = Map.insert otherParty snapshotSignature sigs
        ifAllMembersHaveSigned snapshot sigs' $ do
          -- Spec: σ̃ ← MS-ASig(k_H, ̂Σ̂)
          let multisig = aggregateInOrder sigs' parties
          requireVerifiedMultisignature multisig snapshot $ do
            NewState
              ( onlyUpdateCoordinatedHeadState $
                  coordinatedHeadState
                    { confirmedSnapshot =
                        ConfirmedSnapshot
                          { snapshot
                          , signatures = multisig
                          }
                    , seenSnapshot = LastSeenSnapshot (number snapshot)
                    }
              )
              [ClientEffect $ SnapshotConfirmed headId snapshot multisig]
              & emitSnapshot env
 where
  seenSn = seenSnapshotNumber seenSnapshot

  requireValidAckSn continue =
    if sn `elem` [seenSn, seenSn + 1]
      then continue
      else Error $ RequireFailed "requireValidAckSn"

  waitOnSeenSnapshot continue =
    case seenSnapshot of
      SeenSnapshot snapshot sigs
        | seenSn == sn -> continue snapshot sigs
      _ -> Wait WaitOnSeenSnapshot

  requireNotSignedYet sigs continue =
    if not (Map.member otherParty sigs)
      then continue
      else Error $ RequireFailed "requireNotSignedYet"

  ifAllMembersHaveSigned snapshot sigs' cont =
    if Map.keysSet sigs' == Set.fromList parties
      then cont
      else
        NewState
          ( onlyUpdateCoordinatedHeadState $
              coordinatedHeadState
                { seenSnapshot = SeenSnapshot snapshot sigs'
                }
          )
          []

  requireVerifiedMultisignature multisig msg cont =
    if verifyMultiSignature (vkey <$> parties) multisig msg
      then cont
      else Error $ RequireFailed "requireVerifiedMultisignature"

  -- XXX: Data structures become unwieldy -> helper functions or lenses
  onlyUpdateCoordinatedHeadState chs' =
    Open openState{coordinatedHeadState = chs'}

  CoordinatedHeadState{seenSnapshot} = coordinatedHeadState

  OpenState
    { parameters = HeadParameters{parties}
    , coordinatedHeadState
    , headId
    } = openState

-- ** Closing the Head

-- | Client request to close the head. This leads to a close transaction on
-- chain using the latest confirmed snaphshot of the 'OpenState'.
--
-- __Transition__: 'OpenState' → 'OpenState'
onOpenClientClose ::
  OpenState tx ->
  Outcome tx
onOpenClientClose st =
  OnlyEffects [OnChainEffect{chainState, postChainTx = CloseTx confirmedSnapshot}]
 where
  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{chainState, coordinatedHeadState} = st

-- | Observe a close transaction. If the closed snapshot number is smaller than
-- our last confirmed, we post a contest transaction. Also, we do schedule a
-- notification for clients to fanout at the deadline.
--
-- __Transition__: 'OpenState' → 'ClosedState'
onOpenChainCloseTx ::
  OpenState tx ->
  -- | New chain state.
  ChainStateType tx ->
  -- | Closed snapshot number.
  SnapshotNumber ->
  -- | Contestation deadline.
  UTCTime ->
  Outcome tx
onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline =
  NewState closedState $
    notifyClient
      : [ OnChainEffect
          { -- REVIEW: Was using "old" chainState before
            chainState = newChainState
          , postChainTx = ContestTx{confirmedSnapshot}
          }
        | doContest
        ]
 where
  doContest =
    number (getSnapshot confirmedSnapshot) > closedSnapshotNumber

  closedState =
    Closed
      ClosedState
        { parameters
        , confirmedSnapshot
        , contestationDeadline
        , readyToFanoutSent = False
        , previousRecoverableState = Open openState
        , chainState = newChainState
        , headId
        }

  notifyClient =
    ClientEffect $
      HeadIsClosed
        { headId
        , snapshotNumber = closedSnapshotNumber
        , contestationDeadline
        }

  CoordinatedHeadState{confirmedSnapshot} = coordinatedHeadState

  OpenState{parameters, headId, coordinatedHeadState} = openState

-- | Observe a contest transaction. If the contested snapshot number is smaller
-- than our last confirmed snapshot, we post a contest transaction.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedChainContestTx ::
  ClosedState tx ->
  SnapshotNumber ->
  Outcome tx
onClosedChainContestTx closedState snapshotNumber
  | snapshotNumber < number (getSnapshot confirmedSnapshot) =
      OnlyEffects
        [ ClientEffect HeadIsContested{snapshotNumber, headId}
        , OnChainEffect{chainState, postChainTx = ContestTx{confirmedSnapshot}}
        ]
  | snapshotNumber > number (getSnapshot confirmedSnapshot) =
      -- TODO: A more recent snapshot number was succesfully contested, we will
      -- not be able to fanout! We might want to communicate that to the client!
      OnlyEffects [ClientEffect HeadIsContested{snapshotNumber, headId}]
  | otherwise =
      OnlyEffects [ClientEffect HeadIsContested{snapshotNumber, headId}]
 where
  ClosedState{chainState, confirmedSnapshot, headId} = closedState

-- | Client request to fanout leads to a fanout transaction on chain using the
-- latest confirmed snapshot from 'ClosedState'.
--
-- __Transition__: 'ClosedState' → 'ClosedState'
onClosedClientFanout ::
  ClosedState tx ->
  Outcome tx
onClosedClientFanout closedState =
  OnlyEffects
    [ OnChainEffect
        { chainState
        , postChainTx =
            FanoutTx{utxo, contestationDeadline}
        }
    ]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{chainState, confirmedSnapshot, contestationDeadline} = closedState

-- | Observe a fanout transaction by finalize the head state and notifying
-- clients about it.
--
-- __Transition__: 'ClosedState' → 'IdleState'
onClosedChainFanoutTx ::
  ClosedState tx ->
  -- | New chain state
  ChainStateType tx ->
  Outcome tx
onClosedChainFanoutTx closedState newChainState =
  NewState
    (Idle IdleState{chainState = newChainState})
    [ ClientEffect $ HeadIsFinalized{headId, utxo}
    ]
 where
  Snapshot{utxo} = getSnapshot confirmedSnapshot

  ClosedState{confirmedSnapshot, headId} = closedState

-- | Observe a chain rollback and transition to corresponding previous
-- recoverable state.
--
-- __Transition__: 'OpenState' → 'HeadState'
onCurrentChainRollback ::
  (IsChainState tx) =>
  HeadState tx ->
  ChainSlot ->
  Outcome tx
onCurrentChainRollback currentState slot =
  NewState (rollback slot currentState) [ClientEffect RolledBack]
 where
  rollback rollbackSlot hs
    | chainStateSlot (getChainState hs) <= rollbackSlot = hs
    | otherwise =
        case hs of
          Idle{} -> hs
          Initial InitialState{previousRecoverableState} ->
            rollback rollbackSlot previousRecoverableState
          Open OpenState{previousRecoverableState} ->
            rollback rollbackSlot previousRecoverableState
          Closed ClosedState{previousRecoverableState} ->
            rollback rollbackSlot previousRecoverableState

-- | The "pure core" of the Hydra node, which handles the 'Event' against a
-- current 'HeadState'. Resulting new 'HeadState's are retained and 'Effect'
-- outcomes handled by the "Hydra.Node".
update ::
  (IsTx tx, IsChainState tx) =>
  Environment ->
  Ledger tx ->
  HeadState tx ->
  Event tx ->
  Outcome tx
update env ledger st ev = case (st, ev) of
  (Idle idleState, ClientEvent Init) ->
    onIdleClientInit env idleState
  (Idle idleState, OnChainEvent Observation{observedTx = OnInitTx{headId, contestationPeriod, parties}, newChainState}) ->
    onIdleChainInitTx idleState newChainState parties contestationPeriod headId
  -- Initial
  (Initial idleState, ClientEvent clientInput@(Commit _)) ->
    onInitialClientCommit env idleState clientInput
  (Initial initialState, OnChainEvent Observation{observedTx = OnCommitTx{party = pt, committed = utxo}, newChainState}) ->
    onInitialChainCommitTx initialState newChainState pt utxo
  (Initial initialState, ClientEvent Abort) ->
    onInitialClientAbort initialState
  (Initial initialState, OnChainEvent Observation{observedTx = OnCollectComTx{}, newChainState}) ->
    onInitialChainCollectTx initialState newChainState
  (Initial InitialState{headId, committed}, OnChainEvent Observation{observedTx = OnAbortTx{}, newChainState}) ->
    onInitialChainAbortTx newChainState committed headId
  (Initial InitialState{committed, headId}, ClientEvent GetUTxO) ->
    OnlyEffects [ClientEffect . GetUTxOResponse headId $ fold committed]
  -- Open
  (Open openState, ClientEvent Close) ->
    onOpenClientClose openState
  (Open{}, ClientEvent (NewTx tx)) ->
    onOpenClientNewTx env tx
  (Open openState, NetworkEvent ttl (ReqTx _ tx)) ->
    onOpenNetworkReqTx env ledger openState ttl tx
  (Open openState, NetworkEvent _ (ReqSn otherParty sn txs)) ->
    -- XXX: ttl == 0 not handled for ReqSn
    onOpenNetworkReqSn env ledger openState otherParty sn txs
  (Open openState, NetworkEvent _ (AckSn otherParty snapshotSignature sn)) ->
    -- XXX: ttl == 0 not handled for AckSn
    onOpenNetworkAckSn env openState otherParty snapshotSignature sn
  ( Open openState
    , OnChainEvent Observation{observedTx = OnCloseTx{snapshotNumber = closedSnapshotNumber, contestationDeadline}, newChainState}
    ) ->
      onOpenChainCloseTx openState newChainState closedSnapshotNumber contestationDeadline
  (Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}, headId}, ClientEvent GetUTxO) ->
    -- TODO: Is it really intuitive that we respond from the confirmed ledger if
    -- transactions are validated against the seen ledger?
    OnlyEffects [ClientEffect . GetUTxOResponse headId $ getField @"utxo" $ getSnapshot confirmedSnapshot]
  -- Closed
  (Closed closedState, OnChainEvent Observation{observedTx = OnContestTx{snapshotNumber}}) ->
    onClosedChainContestTx closedState snapshotNumber
  (Closed cst@ClosedState{contestationDeadline, readyToFanoutSent, headId}, OnChainEvent (Tick chainTime))
    | chainTime > contestationDeadline && not readyToFanoutSent ->
        NewState
          (Closed cst{readyToFanoutSent = True})
          [ClientEffect $ ReadyToFanout headId]
  (Closed closedState, ClientEvent Fanout) ->
    onClosedClientFanout closedState
  (Closed closedState, OnChainEvent Observation{observedTx = OnFanoutTx{}, newChainState}) ->
    onClosedChainFanoutTx closedState newChainState
  -- General
  (currentState, OnChainEvent (Rollback slot)) ->
    onCurrentChainRollback currentState slot
  (_, OnChainEvent Tick{}) ->
    OnlyEffects []
  (_, NetworkEvent _ (Connected nodeId)) ->
    OnlyEffects [ClientEffect $ PeerConnected{peer = nodeId}]
  (_, NetworkEvent _ (Disconnected nodeId)) ->
    OnlyEffects [ClientEffect $ PeerDisconnected{peer = nodeId}]
  (_, PostTxError{postChainTx, postTxError}) ->
    OnlyEffects [ClientEffect $ PostTxOnChainFailed{postChainTx, postTxError}]
  (_, ClientEvent{clientInput}) ->
    OnlyEffects [ClientEffect $ CommandFailed clientInput]
  _ ->
    Error $ InvalidEvent ev st

-- * Snapshot helper functions

data SnapshotOutcome tx
  = ShouldSnapshot SnapshotNumber [tx] -- TODO(AB) : should really be a Set (TxId tx)
  | ShouldNotSnapshot NoSnapshotReason
  deriving (Eq, Show, Generic)

data NoSnapshotReason
  = NotLeader SnapshotNumber
  | SnapshotInFlight SnapshotNumber
  | NoTransactionsToSnapshot
  deriving (Eq, Show, Generic)

isLeader :: HeadParameters -> Party -> SnapshotNumber -> Bool
isLeader HeadParameters{parties} p sn =
  case p `elemIndex` parties of
    Just i -> ((fromIntegral sn - 1) `mod` length parties) == i
    _ -> False

-- | Snapshot emission decider
newSn :: Environment -> HeadParameters -> CoordinatedHeadState tx -> SnapshotOutcome tx
newSn Environment{party} parameters CoordinatedHeadState{confirmedSnapshot, seenSnapshot, seenTxs} =
  if
      | not (isLeader parameters party nextSn) ->
          ShouldNotSnapshot $ NotLeader nextSn
      | -- NOTE: This is different than in the spec. If we use seenSn /=
        -- confirmedSn here, we implicitly require confirmedSn <= seenSn. Which
        -- may be an acceptable invariant, but we have property tests which are
        -- more strict right now. Anyhow, we can be more expressive.
        snapshotInFlight ->
          ShouldNotSnapshot $ SnapshotInFlight nextSn
      | null seenTxs ->
          ShouldNotSnapshot NoTransactionsToSnapshot
      | otherwise ->
          ShouldSnapshot nextSn seenTxs
 where
  nextSn = confirmedSn + 1

  snapshotInFlight = case seenSnapshot of
    NoSeenSnapshot -> False
    LastSeenSnapshot{} -> False
    RequestedSnapshot{} -> True
    SeenSnapshot{} -> True

  Snapshot{number = confirmedSn} = getSnapshot confirmedSnapshot

-- | Emit a snapshot if we are the next snapshot leader. 'Outcome' modifying
-- signature so it can be chained with other 'update' functions.
emitSnapshot :: Environment -> Outcome tx -> Outcome tx
emitSnapshot env@Environment{party} outcome =
  case outcome of
    NewState (Open OpenState{parameters, coordinatedHeadState, previousRecoverableState, chainState, headId}) effects ->
      case newSn env parameters coordinatedHeadState of
        ShouldSnapshot sn txs -> do
          let CoordinatedHeadState{seenSnapshot} = coordinatedHeadState
          NewState
            ( Open
                OpenState
                  { parameters
                  , coordinatedHeadState =
                      coordinatedHeadState
                        { seenSnapshot =
                            RequestedSnapshot
                              { lastSeen = seenSnapshotNumber $ seenSnapshot
                              , requested = sn
                              }
                        }
                  , previousRecoverableState
                  , chainState
                  , headId
                  }
            )
            $ NetworkEffect (ReqSn party sn txs) : effects
        _ -> outcome
    _ -> outcome
