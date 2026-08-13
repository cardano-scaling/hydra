{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State where

import Hydra.Prelude

import Data.Aeson (object, withObject, (.:), (.=))
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.Tx (
  HeadId,
  HeadParameters,
  HeadSeed,
  IsTx (..),
  Party,
 )
import Hydra.Tx.Crypto (Signature, getSignableRepresentation)
import Hydra.Tx.Snapshot (
  ConfirmedSnapshot,
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
 )

-- | The main state of the Hydra protocol state machine. It holds both, the
-- overall protocol state, but also the off-chain 'CoordinatedHeadState'.
--
-- Each of the sub-types (OpenState, etc.) contain a black-box
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
  | Open (OpenState tx)
  | Closed (ClosedState tx)
  | -- | A closed head whose UTxO is being fanned out across multiple
    -- transactions (on-chain in the @FanoutProgress@ state). Reached from
    -- 'Closed' once the first partial fanout is observed.
    FanoutProgress (PartialFanoutState tx)
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (HeadState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (HeadState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (HeadState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (HeadState tx)

instance IsChainState tx => ToCBOR (HeadState tx) where
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (HeadState tx) where
  fromCBOR = genericFromCBOR

-- | Update the chain state in any 'HeadState'.
setChainState :: ChainStateType tx -> HeadState tx -> HeadState tx
setChainState chainState = \case
  Idle st -> Idle st{chainState}
  Open st -> Open st{chainState}
  Closed st -> Closed st{chainState}
  FanoutProgress st -> FanoutProgress st{chainState}

-- | Get the chain state in any 'HeadState'.
getChainState :: HeadState tx -> ChainStateType tx
getChainState = \case
  Idle IdleState{chainState} -> chainState
  Open OpenState{chainState} -> chainState
  Closed ClosedState{chainState} -> chainState
  FanoutProgress PartialFanoutState{chainState} -> chainState

-- | Get the head parameters in any 'HeadState'.
getHeadParameters :: HeadState tx -> Maybe HeadParameters
getHeadParameters = \case
  Idle _ -> Nothing
  Open OpenState{parameters} -> Just parameters
  Closed ClosedState{parameters} -> Just parameters
  FanoutProgress PartialFanoutState{parameters} -> Just parameters

-- | Get the head parameters in any 'HeadState'.
getOpenStateConfirmedSnapshot :: HeadState tx -> Maybe (ConfirmedSnapshot tx)
getOpenStateConfirmedSnapshot = \case
  Idle _ -> Nothing
  Open OpenState{coordinatedHeadState = CoordinatedHeadState{confirmedSnapshot}} -> Just confirmedSnapshot
  Closed ClosedState{} -> Nothing
  FanoutProgress PartialFanoutState{} -> Nothing

-- ** Idle

-- | An 'Idle' head only having a chain state with things seen on chain so far.
newtype IdleState tx = IdleState {chainState :: ChainStateType tx}
  deriving stock (Generic)

deriving stock instance Eq (ChainStateType tx) => Eq (IdleState tx)
deriving stock instance Show (ChainStateType tx) => Show (IdleState tx)
deriving anyclass instance ToJSON (ChainStateType tx) => ToJSON (IdleState tx)
deriving anyclass instance FromJSON (ChainStateType tx) => FromJSON (IdleState tx)

instance IsChainState tx => ToCBOR (IdleState tx) where
  toCBOR IdleState{chainState} = toCBOR chainState

instance IsChainState tx => FromCBOR (IdleState tx) where
  fromCBOR = IdleState <$> fromCBOR

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

instance IsChainState tx => ToCBOR (OpenState tx) where
  toCBOR OpenState{parameters, coordinatedHeadState, chainState, headId, headSeed} =
    toCBOR parameters
      <> toCBOR coordinatedHeadState
      <> toCBOR chainState
      <> toCBOR headId
      <> toCBOR headSeed

instance IsChainState tx => FromCBOR (OpenState tx) where
  fromCBOR = OpenState <$> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Off-chain state of the Coordinated Head protocol.
data CoordinatedHeadState tx = CoordinatedHeadState
  { localUTxO :: UTxOType tx
  -- ^ The latest UTxO resulting from applying 'localTxs' to
  -- 'confirmedSnapshot'. Spec: L̂
  , localTxs :: Seq tx
  -- ^ Sequence of transactions applied locally and pending inclusion in a
  -- snapshot. Ordering is important as transactions are added in order of
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

instance IsTx tx => ToCBOR (CoordinatedHeadState tx) where
  toCBOR CoordinatedHeadState{localUTxO, localTxs, allTxs, confirmedSnapshot, seenSnapshot, currentDepositTxId, decommitTx, version} =
    toCBOR localUTxO
      <> toCBOR localTxs
      <> toCBOR allTxs
      <> toCBOR confirmedSnapshot
      <> toCBOR seenSnapshot
      <> toCBOR currentDepositTxId
      <> toCBOR decommitTx
      <> toCBOR version

instance IsTx tx => FromCBOR (CoordinatedHeadState tx) where
  fromCBOR =
    CoordinatedHeadState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

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
      -- ^ Collected signatures so far.
      , signableBytes :: ~ByteString
      -- ^ Pre-computed result of 'getSignableRepresentation snapshot', cached
      -- to avoid recomputing the expensive UTxO hash on every AckSn
      -- verification. Explicitly lazy under StrictData: state hydration folds
      -- every historical 'NodeState' to WHNF, and a strict field here would
      -- force one full accumulator commitment per replayed SnapshotRequested
      -- event. At runtime it is forced once, on the first AckSn.
      }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (SeenSnapshot tx)
deriving stock instance IsTx tx => Show (SeenSnapshot tx)

-- Manual instances that exclude 'signableBytes' from JSON (it is derived from
-- 'snapshot' and recomputed on deserialisation).
instance IsTx tx => ToJSON (SeenSnapshot tx) where
  toJSON = \case
    NoSeenSnapshot ->
      object ["tag" .= ("NoSeenSnapshot" :: Text)]
    LastSeenSnapshot{lastSeen} ->
      object ["tag" .= ("LastSeenSnapshot" :: Text), "lastSeen" .= lastSeen]
    RequestedSnapshot{lastSeen, requested} ->
      object
        [ "tag" .= ("RequestedSnapshot" :: Text)
        , "lastSeen" .= lastSeen
        , "requested" .= requested
        ]
    SeenSnapshot{snapshot, signatories} ->
      object
        [ "tag" .= ("SeenSnapshot" :: Text)
        , "snapshot" .= snapshot
        , "signatories" .= signatories
        ]

instance IsTx tx => FromJSON (SeenSnapshot tx) where
  parseJSON = withObject "SeenSnapshot" $ \obj -> do
    tag :: Text <- obj .: "tag"
    case tag of
      "NoSeenSnapshot" -> pure NoSeenSnapshot
      "LastSeenSnapshot" -> LastSeenSnapshot <$> obj .: "lastSeen"
      "RequestedSnapshot" ->
        RequestedSnapshot
          <$> obj .: "lastSeen"
          <*> obj .: "requested"
      "SeenSnapshot" -> do
        snapshot <- obj .: "snapshot"
        signatories <- obj .: "signatories"
        pure $ mkSeenSnapshot snapshot signatories
      other -> fail $ "unknown SeenSnapshot tag: " <> toString other

-- Manual instances that exclude 'signableBytes' from CBOR (it is derived from
-- 'snapshot' and recomputed on deserialisation), like the JSON instances above.
instance IsTx tx => ToCBOR (SeenSnapshot tx) where
  toCBOR = \case
    NoSeenSnapshot ->
      toCBOR ("NoSeenSnapshot" :: Text)
    LastSeenSnapshot{lastSeen} ->
      toCBOR ("LastSeenSnapshot" :: Text) <> toCBOR lastSeen
    RequestedSnapshot{lastSeen, requested} ->
      toCBOR ("RequestedSnapshot" :: Text) <> toCBOR lastSeen <> toCBOR requested
    SeenSnapshot{snapshot, signatories} ->
      toCBOR ("SeenSnapshot" :: Text) <> toCBOR snapshot <> toCBOR signatories

instance IsTx tx => FromCBOR (SeenSnapshot tx) where
  fromCBOR =
    fromCBOR >>= \case
      ("NoSeenSnapshot" :: Text) -> pure NoSeenSnapshot
      "LastSeenSnapshot" -> LastSeenSnapshot <$> fromCBOR
      "RequestedSnapshot" -> RequestedSnapshot <$> fromCBOR <*> fromCBOR
      "SeenSnapshot" -> mkSeenSnapshot <$> fromCBOR <*> fromCBOR
      tag -> fail $ show tag <> " is not a proper CBOR-encoded SeenSnapshot"

-- | Smart constructor for 'SeenSnapshot' that computes and caches
-- 'signableBytes' from 'snapshot', enforcing the invariant that they stay in sync.
mkSeenSnapshot ::
  IsTx tx =>
  Snapshot tx ->
  Map Party (Signature (Snapshot tx)) ->
  SeenSnapshot tx
mkSeenSnapshot snapshot signatories =
  SeenSnapshot{snapshot, signatories, signableBytes = getSignableRepresentation snapshot}

-- | Get the last seen snapshot number given a 'SeenSnapshot'.
seenSnapshotNumber :: SeenSnapshot tx -> SnapshotNumber
seenSnapshotNumber = \case
  NoSeenSnapshot -> 0
  LastSeenSnapshot{lastSeen} -> lastSeen
  RequestedSnapshot{lastSeen} -> lastSeen
  SeenSnapshot{snapshot = Snapshot{number}} -> number

-- | Whether a snapshot is currently in-flight (requested or being signed).
snapshotInFlight :: SeenSnapshot tx -> Bool
snapshotInFlight = \case
  NoSeenSnapshot -> False
  LastSeenSnapshot{} -> False
  RequestedSnapshot{} -> True
  SeenSnapshot{} -> True

-- | Whether AckSns are currently being collected for a snapshot.
-- Unlike 'snapshotInFlight', returns False for 'RequestedSnapshot' — a
-- snapshot sent but not yet echoed is stale once the version bumps and should
-- not block a fresh request with the new version.
isCollectingAcks :: SeenSnapshot tx -> Bool
isCollectingAcks = \case
  SeenSnapshot{} -> True
  _ -> False

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

instance IsChainState tx => ToCBOR (ClosedState tx) where
  toCBOR ClosedState{parameters, confirmedSnapshot, contestationDeadline, readyToFanoutSent, chainState, headId, headSeed, version} =
    toCBOR parameters
      <> toCBOR confirmedSnapshot
      <> toCBOR contestationDeadline
      <> toCBOR readyToFanoutSent
      <> toCBOR chainState
      <> toCBOR headId
      <> toCBOR headSeed
      <> toCBOR version

instance IsChainState tx => FromCBOR (ClosedState tx) where
  fromCBOR =
    ClosedState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

-- ** PartialFanout

-- Terminology note: the selective-fanout feature spans several near-synonymous
-- names; they map as follows:
--
--   * PartialFanout          — the client input (a user-selected subset to fan out)
--   * HeadPartiallyFannedOut — the server output reporting one observed step
--   * FanningOut             — the client-visible HeadStatus
--   * FanoutProgress         — this off-chain HeadState constructor (and the
--                              matching on-chain head datum), holding a PartialFanoutState
--   * FanoutMode             — how the next step is chosen while in FanoutProgress

-- | How the node decides which UTxOs to distribute in the next fanout step
-- while the head is in 'FanoutProgress'.
data FanoutMode tx
  = -- | Entered only via the 'Fanout' client command: drain the whole remaining
    -- set automatically, dynamically chunked, ending in the final fanout.
    AutoDrain
  | -- | Manual mode: keep distributing this (content-tracked) user selection,
    -- dynamically chunked, until it is exhausted.
    DistributingSelection (UTxOType tx)
  | -- | Manual mode: the previous selection has been fully distributed. Wait for
    -- the next 'PartialFanout' command; do not auto-drain.
    AwaitingSelection
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (FanoutMode tx)
deriving stock instance IsTx tx => Show (FanoutMode tx)
deriving anyclass instance IsTx tx => ToJSON (FanoutMode tx)
deriving anyclass instance IsTx tx => FromJSON (FanoutMode tx)

instance IsTx tx => ToCBOR (FanoutMode tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (FanoutMode tx) where
  fromCBOR = genericFromCBOR

-- | A closed head whose UTxO is being distributed across multiple fanout
-- transactions (on-chain @FanoutProgress@). Holds the partial-fanout bookkeeping
-- that used to live in 'ClosedState'.
data PartialFanoutState tx = PartialFanoutState
  { parameters :: HeadParameters
  , confirmedSnapshot :: ConfirmedSnapshot tx
  , contestationDeadline :: UTCTime
  , chainState :: ChainStateType tx
  , headId :: HeadId
  , headSeed :: HeadSeed
  , version :: SnapshotVersion
  , remainingOutputs :: UTxOType tx
  -- ^ UTxO still to be fanned out (tracked by content, via set difference).
  , distributedOutputs :: UTxOType tx
  -- ^ Accumulates UTxO distributed so far; used to reconstruct the full set in
  --   'HeadFannedOut' once the head is finalized.
  , mode :: FanoutMode tx
  -- ^ Drives the chunk source for the next step (see 'FanoutMode').
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (PartialFanoutState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (PartialFanoutState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (PartialFanoutState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (PartialFanoutState tx)

instance IsChainState tx => ToCBOR (PartialFanoutState tx) where
  toCBOR PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version, remainingOutputs, distributedOutputs, mode} =
    toCBOR parameters
      <> toCBOR confirmedSnapshot
      <> toCBOR contestationDeadline
      <> toCBOR chainState
      <> toCBOR headId
      <> toCBOR headSeed
      <> toCBOR version
      <> toCBOR remainingOutputs
      <> toCBOR distributedOutputs
      <> toCBOR mode

instance IsChainState tx => FromCBOR (PartialFanoutState tx) where
  fromCBOR =
    PartialFanoutState
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
