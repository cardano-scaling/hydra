{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Hydra.HeadLogic.State (
  module Hydra.HeadLogic.State,
  module Hydra.HeadLogic.Settlements,
) where

import Hydra.Prelude

import Cardano.Binary (Decoder)
import Data.Aeson (Value (Object), defaultOptions, genericParseJSON, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Hydra.Chain.ChainState (ChainSlot, IsChainState (..))
import Hydra.HeadLogic.Settlements
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
  getSnapshot,
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
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (IdleState tx) where
  fromCBOR = genericFromCBOR

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
  toCBOR = genericToCBOR

instance IsChainState tx => FromCBOR (OpenState tx) where
  fromCBOR = genericFromCBOR

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
  , settlements :: !(Settlements tx)
  -- ^ Snapshots whose increment or decrement settled on chain and may still be
  -- erased by a rollback, see 'Settlements'.
  , unretained :: !UnretainedSettlements
  -- ^ Version bumps observed on chain whose snapshot this node has not
  -- confirmed yet, see 'UnretainedSettlements'.
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (CoordinatedHeadState tx)
deriving stock instance IsTx tx => Show (CoordinatedHeadState tx)
deriving anyclass instance IsTx tx => ToJSON (CoordinatedHeadState tx)

-- JSON checkpoints predate the CBOR event store and are still migrated at startup.
instance IsTx tx => FromJSON (CoordinatedHeadState tx) where
  parseJSON = withObject "CoordinatedHeadState" $ \fields -> do
    settlements <-
      if KeyMap.member "settlements" fields
        then fields .: "settlements"
        else do
          commit <- fields .:? "finalizedCommit"
          decommit <- fields .:? "finalizedDecommit"
          migrateFinalizedSnapshots (catMaybes [commit, decommit])
    unretained <- fields .:? "unretained" .!= mempty
    genericParseJSON defaultOptions $
      Object $
        KeyMap.insert "settlements" (toJSON (settlements :: Settlements tx)) $
          KeyMap.insert "unretained" (toJSON (unretained :: UnretainedSettlements)) fields

-- | Tag of the layout carrying retained settlements and observation notes. Fields are a bare
-- concatenation, so each layout needs its own tag. Older layouts remain readable.
coordinatedHeadStateCBORTag :: Text
coordinatedHeadStateCBORTag = "CoordinatedHeadState4"

-- | Tag of the layout without retained settlements. Decoded, never written.
coordinatedHeadStateCBORTagV1 :: Text
coordinatedHeadStateCBORTagV1 = "CoordinatedHeadState"

-- | Tag of the layout with optional finalized commit and decommit records.
-- Decoded, never written.
coordinatedHeadStateCBORTagV2 :: Text
coordinatedHeadStateCBORTagV2 = "CoordinatedHeadState2"

instance IsTx tx => ToCBOR (CoordinatedHeadState tx) where
  toCBOR CoordinatedHeadState{localUTxO, localTxs, allTxs, confirmedSnapshot, seenSnapshot, currentDepositTxId, decommitTx, version, settlements, unretained} =
    toCBOR coordinatedHeadStateCBORTag
      <> toCBOR localUTxO
      <> toCBOR localTxs
      <> toCBOR allTxs
      <> toCBOR confirmedSnapshot
      <> toCBOR seenSnapshot
      <> toCBOR currentDepositTxId
      <> toCBOR decommitTx
      <> toCBOR version
      <> toCBOR settlements
      <> toCBOR unretained

instance IsTx tx => FromCBOR (CoordinatedHeadState tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == coordinatedHeadStateCBORTag -> decode fromCBOR fromCBOR
        | tag == "CoordinatedHeadState3" -> decode fromCBOR (pure mempty)
        | tag == coordinatedHeadStateCBORTagV2 -> decode decodeV2Settlements (pure mempty)
        | tag == coordinatedHeadStateCBORTagV1 -> decode (pure emptySettlements) (pure mempty)
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded CoordinatedHeadState"
   where
    decode :: Decoder s (Settlements tx) -> Decoder s UnretainedSettlements -> Decoder s (CoordinatedHeadState tx)
    decode decodeSettlements decodeNotes = do
      localUTxO <- fromCBOR
      localTxs <- fromCBOR
      allTxs <- fromCBOR
      confirmedSnapshot <- fromCBOR
      seenSnapshot <- fromCBOR
      currentDepositTxId <- fromCBOR
      decommitTx <- fromCBOR
      version <- fromCBOR
      settlements <- decodeSettlements
      unretained <- decodeNotes
      pure CoordinatedHeadState{localUTxO, localTxs, allTxs, confirmedSnapshot, seenSnapshot, currentDepositTxId, decommitTx, version, settlements, unretained}

    decodeV2Settlements :: Decoder s (Settlements tx)
    decodeV2Settlements = do
      finalizedCommit <- fromCBOR
      finalizedDecommit <- fromCBOR
      migrateFinalizedSnapshots (catMaybes [finalizedCommit, finalizedDecommit])

-- | Share the lossless legacy conversion between JSON and CBOR migrations.
migrateFinalizedSnapshots :: (IsTx tx, MonadFail m) => [FinalizedSnapshot tx] -> m (Settlements tx)
migrateFinalizedSnapshots = foldlM retain emptySettlements
 where
  retain :: (IsTx tx, MonadFail m) => Settlements tx -> FinalizedSnapshot tx -> m (Settlements tx)
  retain retained (FinalizedSnapshot snapshot observedAtSlot) = do
    let Snapshot{version = snapshotVersion} = getSnapshot snapshot
        status = Landed observedAtSlot
    case Map.lookup snapshotVersion (settlementEntries retained) of
      Just Settlement{snapshot = previousSnapshot, status = previousStatus}
        | previousSnapshot /= snapshot -> fail "Conflicting V2 finalized snapshots at the same version"
        | Landed previousSlot <- previousStatus ->
            pure $ adjustSettlementStatus snapshotVersion (Landed $ max previousSlot observedAtSlot) retained
        | otherwise -> pure retained
      Nothing -> either (fail . show) pure $ tryInsertSettlement Settlement{snapshot, status} retained

-- | Decode-only record for the V2 layout. Its constructor name and field order
-- are part of the legacy CBOR format.
data FinalizedSnapshot tx = FinalizedSnapshot (ConfirmedSnapshot tx) ChainSlot
  deriving stock (Generic)

instance IsTx tx => FromJSON (FinalizedSnapshot tx) where
  parseJSON = withObject "FinalizedSnapshot" $ \fields ->
    FinalizedSnapshot <$> fields .: "snapshot" <*> fields .: "observedAtSlot"

instance IsTx tx => FromCBOR (FinalizedSnapshot tx) where
  fromCBOR = genericFromCBOR

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
  , unconfirmedSnapshot :: Maybe (Snapshot tx)
  -- ^ The snapshot this node signed but had not seen confirmed when the head
  -- closed. A close or contest carrying its full multisignature confirms it,
  -- see 'Hydra.HeadLogic.onClosedChainContestTx'.
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (ClosedState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (ClosedState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (ClosedState tx)
deriving anyclass instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (ClosedState tx)

-- | Tag of the current on-disk\/wire layout, which carries
-- 'unconfirmedSnapshot'. Like 'coordinatedHeadStateCBORTag', the fields are a
-- bare concatenation, so 'closedStateCBORTagV1' names the layout written before
-- that field existed and is still accepted.
closedStateCBORTag :: Text
closedStateCBORTag = "ClosedState2"

-- | Tag of the layout without 'unconfirmedSnapshot', as written by the generic
-- encoding. Decoded, never written.
closedStateCBORTagV1 :: Text
closedStateCBORTagV1 = "ClosedState"

instance IsChainState tx => ToCBOR (ClosedState tx) where
  toCBOR ClosedState{parameters, confirmedSnapshot, contestationDeadline, readyToFanoutSent, chainState, headId, headSeed, version, unconfirmedSnapshot} =
    toCBOR closedStateCBORTag
      <> toCBOR parameters
      <> toCBOR confirmedSnapshot
      <> toCBOR contestationDeadline
      <> toCBOR readyToFanoutSent
      <> toCBOR chainState
      <> toCBOR headId
      <> toCBOR headSeed
      <> toCBOR version
      <> toCBOR unconfirmedSnapshot

instance IsChainState tx => FromCBOR (ClosedState tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == closedStateCBORTag -> decode True
        | tag == closedStateCBORTagV1 -> decode False
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded ClosedState"
   where
    decode :: Bool -> Decoder s (ClosedState tx)
    decode hasUnconfirmed = do
      parameters <- fromCBOR
      confirmedSnapshot <- fromCBOR
      contestationDeadline <- fromCBOR
      readyToFanoutSent <- fromCBOR
      chainState <- fromCBOR
      headId <- fromCBOR
      headSeed <- fromCBOR
      version <- fromCBOR
      -- A state from before this field existed has no unconfirmed snapshot to
      -- adopt, like it was at the time.
      unconfirmedSnapshot <- if hasUnconfirmed then fromCBOR else pure Nothing
      pure ClosedState{parameters, confirmedSnapshot, contestationDeadline, readyToFanoutSent, chainState, headId, headSeed, version, unconfirmedSnapshot}

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

-- | One observed step and the part of this node's selection it consumed.
-- Storing only that delta keeps rollback history linear in distributed outputs
-- and prevents a different driver's disjoint step from expanding our selection.
data FanoutStepLanded tx = FanoutStepLanded
  { landedAt :: ChainSlot
  , stepOutputs :: UTxOType tx
  , selectedOutputs :: UTxOType tx
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (FanoutStepLanded tx)
deriving stock instance IsTx tx => Show (FanoutStepLanded tx)
deriving anyclass instance IsTx tx => ToJSON (FanoutStepLanded tx)

instance IsTx tx => FromJSON (FanoutStepLanded tx) where
  parseJSON = withObject "FanoutStepLanded" $ \fields -> do
    landedAt <- fields .: "landedAt"
    stepOutputs <- fields .: "stepOutputs"
    selectedOutputs <-
      if KeyMap.member "selectedOutputs" fields
        then fields .: "selectedOutputs"
        else selectedOutputsInStep stepOutputs <$> fields .: "modeBefore"
    pure FanoutStepLanded{landedAt, stepOutputs, selectedOutputs}

instance IsTx tx => ToCBOR (FanoutStepLanded tx) where
  toCBOR FanoutStepLanded{landedAt, stepOutputs, selectedOutputs} =
    toCBOR ("FanoutStepLanded2" :: Text) <> toCBOR landedAt <> toCBOR stepOutputs <> toCBOR selectedOutputs

instance IsTx tx => FromCBOR (FanoutStepLanded tx) where
  fromCBOR = do
    tag <- fromCBOR
    case (tag :: Text) of
      "FanoutStepLanded2" -> FanoutStepLanded <$> fromCBOR <*> fromCBOR <*> fromCBOR
      "FanoutStepLanded" -> do
        landedAt <- fromCBOR
        stepOutputs <- fromCBOR
        selectedOutputs <- selectedOutputsInStep stepOutputs <$> fromCBOR
        pure FanoutStepLanded{landedAt, stepOutputs, selectedOutputs}
      _ -> fail "Invalid FanoutStepLanded tag"

-- | Match by output content and multiplicity, retaining the selection's own
-- references. A step on L1 names its outputs differently from the L2 selection.
selectedOutputsInStep :: IsTx tx => UTxOType tx -> FanoutMode tx -> UTxOType tx
selectedOutputsInStep stepOutputs = \case
  DistributingSelection selection ->
    let remaining = foldl' (flip removeOneOutputFromUTxO) selection (outputsOfUTxO stepOutputs)
     in selection `withoutUTxO` remaining
  _ -> mempty

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
  , stepsLanded :: [FanoutStepLanded tx]
  -- ^ The steps observed so far, oldest first. This is the record, derived
  --   from the chain, that the two sets above and 'mode' are rewound from on a
  --   rollback.
  , everLanded :: Bool
  -- ^ Whether a step of this fanout was ever observed on chain, on any fork.
  --   A rollback that rewinds the progress to nothing distributed does not
  --   reset it. A re-post that fails after such a rewind is racing the erased
  --   step landing again; that is not a failed start, and it must not cost
  --   this node the driver role (see the 'PostTxError' arm of
  --   'Hydra.HeadLogic.update').
  }
  deriving stock (Generic)

deriving stock instance (IsTx tx, Eq (ChainStateType tx)) => Eq (PartialFanoutState tx)
deriving stock instance (IsTx tx, Show (ChainStateType tx)) => Show (PartialFanoutState tx)
deriving anyclass instance (IsTx tx, ToJSON (ChainStateType tx)) => ToJSON (PartialFanoutState tx)
instance (IsTx tx, FromJSON (ChainStateType tx)) => FromJSON (PartialFanoutState tx) where
  parseJSON = withObject "PartialFanoutState" $ \fields -> do
    stepsLanded <- fields .:? "stepsLanded" .!= []
    distributedOutputs <- fields .: "distributedOutputs"
    everLanded <- fields .:? "everLanded" .!= (not (null (stepsLanded :: [FanoutStepLanded tx])) || distributedOutputs /= (mempty :: UTxOType tx))
    genericParseJSON defaultOptions $
      Object $
        KeyMap.insert "stepsLanded" (toJSON stepsLanded) $
          KeyMap.insert "everLanded" (toJSON everLanded) fields

-- | CBOR tag of the current 'PartialFanoutState' layout.
partialFanoutStateCBORTag :: Text
partialFanoutStateCBORTag = "PartialFanoutState3"

-- | CBOR tag of the layout written before 'stepsLanded' existed: the
-- constructor-name tag 'genericToCBOR' wrote, followed by the other fields in
-- declaration order.
partialFanoutStateCBORTagV1 :: Text
partialFanoutStateCBORTagV1 = "PartialFanoutState"

instance IsChainState tx => ToCBOR (PartialFanoutState tx) where
  toCBOR PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version, remainingOutputs, distributedOutputs, mode, stepsLanded, everLanded} =
    toCBOR partialFanoutStateCBORTag
      <> toCBOR parameters
      <> toCBOR confirmedSnapshot
      <> toCBOR contestationDeadline
      <> toCBOR chainState
      <> toCBOR headId
      <> toCBOR headSeed
      <> toCBOR version
      <> toCBOR remainingOutputs
      <> toCBOR distributedOutputs
      <> toCBOR mode
      <> toCBOR stepsLanded
      <> toCBOR everLanded

instance IsChainState tx => FromCBOR (PartialFanoutState tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == partialFanoutStateCBORTag -> decode True True
        | tag == "PartialFanoutState2" -> decode True False
        | tag == partialFanoutStateCBORTagV1 -> decode False False
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded PartialFanoutState"
   where
    decode :: Bool -> Bool -> Decoder s (PartialFanoutState tx)
    decode hasSteps hasEverLanded = do
      parameters <- fromCBOR
      confirmedSnapshot <- fromCBOR
      contestationDeadline <- fromCBOR
      chainState <- fromCBOR
      headId <- fromCBOR
      headSeed <- fromCBOR
      version <- fromCBOR
      remainingOutputs <- fromCBOR
      distributedOutputs <- fromCBOR
      mode <- fromCBOR
      -- A state written before steps were tracked cannot rewind them on a
      -- rollback, exactly as before the upgrade; steps landing from now on can.
      stepsLanded <- if hasSteps then fromCBOR else pure []
      -- Such a state can only have distributed something by seeing a step land.
      everLanded <- if hasEverLanded then fromCBOR else pure (not (null stepsLanded) || distributedOutputs /= mempty)
      pure PartialFanoutState{parameters, confirmedSnapshot, contestationDeadline, chainState, headId, headSeed, version, remainingOutputs, distributedOutputs, mode, stepsLanded, everLanded}
