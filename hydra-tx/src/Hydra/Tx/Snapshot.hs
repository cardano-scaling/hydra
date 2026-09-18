{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Tx.Snapshot where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (serialise)
import Data.Aeson (Value (String), object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (SerialiseAsRawBytes (..))
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (MultiSignature)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (IsTx (..), combinedUTxO)
import PlutusLedgerApi.V3 (fromBuiltin, toBuiltin, toData)
import PlutusTx.Builtins (sha2_256)

-- * SnapshotNumber and SnapshotVersion

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, Real, Num, Enum, Integral)

instance ToCBOR SnapshotNumber where
  toCBOR = genericToCBOR

instance FromCBOR SnapshotNumber where
  fromCBOR = genericFromCBOR

-- NOTE: On-chain scripts ensure snapshot number does not become negative.
fromChainSnapshotNumber :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshotNumber =
  UnsafeSnapshotNumber . fromMaybe 0 . integerToNatural

newtype SnapshotVersion
  = UnsafeSnapshotVersion Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, Real, Num, Enum, Integral)

instance ToCBOR SnapshotVersion where
  toCBOR = genericToCBOR

instance FromCBOR SnapshotVersion where
  fromCBOR = genericFromCBOR

-- NOTE: On-chain scripts ensure snapshot version does not become negative.
fromChainSnapshotVersion :: Onchain.SnapshotVersion -> SnapshotVersion
fromChainSnapshotVersion =
  UnsafeSnapshotVersion . fromMaybe 0 . integerToNatural

-- * Snapshot

data Snapshot tx = Snapshot
  { headId :: HeadId
  , version :: SnapshotVersion
  -- ^ Open state version this snapshot is based on. Spec: v
  , number :: SnapshotNumber
  -- ^ Monotonically increasing snapshot number. Spec: s
  , confirmed :: [tx]
  -- ^ The set of transactions that lead to 'utxo'. Spec: T
  , utxo :: UTxOType tx
  -- ^ Snaspshotted UTxO set. Spec: U
  , utxoToCommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be committed. Spec: Uα
  , depositTxId :: Maybe (TxIdType tx)
  -- ^ Transaction which deposited 'utxoToCommit' on L1, i.e. the deposit an
  -- increment of this snapshot is allowed to claim. Bound into the signature
  -- so a deposit cannot be swapped for a look-alike one, see
  -- 'getSignableRepresentation'.
  --
  -- A transaction id identifies a deposit because a deposit is always the first
  -- output of its transaction; 'Hydra.Tx.Deposit.observeDepositTx' enforces that
  -- and 'Hydra.Contract.Head.checkIncrement' requires it on-chain. Callers set
  -- this exactly when 'utxoToCommit' is set.
  , utxoToDecommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be decommitted. Spec: Uω
  , accumulator :: Accumulator.HydraAccumulator
  -- ^ What the head holds if the pending L1 tx (increment or decrement) has
  -- not happened:
  --
  -- > utxo <> utxoToDecommit
  --
  -- A decommit is still in the head until its DecrementTx lands, and a deposit
  -- is not in the head until its IncrementTx lands. Close/Contest store this one
  -- when the head is still at this snapshot's 'version' (redeemers Any/Unused).
  -- Spec: A
  , appliedAccumulator :: Accumulator.HydraAccumulator
  -- ^ What the head holds after the pending L1 tx happened:
  --
  -- > utxo <> utxoToCommit
  --
  -- The decommit was paid out, the deposit was absorbed. Close/Contest store
  -- this one when the head moved past this snapshot's 'version' (redeemer Used).
  --
  -- Both are signed, because when the snapshot is signed nobody knows yet which
  -- of the two will be true at close time, and the closed head must commit to
  -- exactly what it holds so that fanout cannot pay out a UTxO twice. With
  -- nothing pending both fields are the same value. Never transmitted, always
  -- rebuilt from the UTxO sets, see 'Hydra.Tx.Accumulator.buildFromSnapshotUTxOs'.
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

-- | Binary representation of snapshot signatures. That is, concatenated CBOR for
-- 'headId', 'version', 'number', 'accumulatorHash', 'appliedAccumulatorHash',
-- 'decommitOutputsHash', and 'commitOutputsHash' according to CDDL schemata:
--
-- headId = bytes .size 28
-- version = uint
-- number = uint
-- accumulatorHash = bytes .size 32  ; blake2b-256 of the compressed G1 commitment of 'accumulator'
-- appliedAccumulatorHash = bytes .size 32  ; blake2b-256 of the compressed G1 commitment of 'appliedAccumulator'
-- decommitOutputsHash = bytes .size 32  ; sha2-256 of the ordered decommit outputs (Uω)
-- commitOutputsHash = bytes .size 32  ; sha2-256 of the ordered commit outputs (Uα)
--                                     ; and of the deposit transaction id
--
-- The two accumulator hashes let Close/Contest store whichever of the two UTxO
-- sets the head actually holds at that time (see the field docs above).
-- 'decommitOutputsHash' and 'commitOutputsHash' additionally bind the exact
-- ordered sets of decommit (Uω) and commit (Uα) outputs, so the on-chain
-- decrement and increment validators can recompute them from the materialized L1
-- decommit outputs / claimed deposit and reject any redirected/altered output.
--
-- 'commitOutputsHash' further binds 'depositTxId'. Committed content on its own
-- does not identify a deposit: a deposit datum is unauthenticated data anyone can
-- copy into a look-alike deposit holding less value, which would otherwise hash
-- the same and accept this snapshot's signatures. See the matching computation in
-- 'Hydra.Contract.Head.checkIncrement'.
instance IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation snapshot@Snapshot{headId, version, number, accumulator, appliedAccumulator, utxoToDecommit} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger version)
        <> serialise (toData . toBuiltin $ toInteger number)
        <> serialise (toData $ toBuiltin accumulatorBytes)
        <> serialise (toData $ toBuiltin appliedAccumulatorBytes)
        <> serialise (toData $ toBuiltin decommitOutputsHash)
        <> serialise (toData $ toBuiltin (commitOutputsHash snapshot))
   where
    accumulatorBytes = Accumulator.getAccumulatorHash accumulator
    appliedAccumulatorBytes = Accumulator.getAccumulatorHash appliedAccumulator
    -- Matches on-chain 'Hydra.Contract.Util.hashTxOuts' over the same outputs in
    -- the same (TxIn-sorted) order; empty-list hash when there is nothing pending.
    decommitOutputsHash = hashUTxO @tx (fromMaybe mempty utxoToDecommit)

-- | Digest of a snapshot's pending commit (Uα) as bound into its signature: the
-- ordered commit outputs together with the id of the deposit transaction they
-- come from.
--
-- Both halves are required. The outputs alone do not identify a deposit, since a
-- deposit datum is unauthenticated data anyone can copy into a look-alike
-- deposit holding less value; binding the deposit's transaction id makes the
-- signature usable for that one deposit only. The increment validator recomputes
-- this from the deposit input it claims, see 'Hydra.Contract.Head.checkIncrement'.
-- Close, contest and decrement transactions cannot recompute it (they spend no
-- deposit) and carry it in their redeemer instead, where it only feeds signature
-- verification.
commitOutputsHash :: forall tx. IsTx tx => Snapshot tx -> ByteString
commitOutputsHash Snapshot{utxoToCommit, depositTxId} =
  fromBuiltin . sha2_256 . toBuiltin $
    hashUTxO @tx (fromMaybe mempty utxoToCommit)
      <> foldMap (txIdBytes @tx) depositTxId

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON Snapshot{headId, number, utxo, confirmed, utxoToCommit, utxoToDecommit, version, accumulator, appliedAccumulator, depositTxId} =
    object
      [ "headId" .= headId
      , "version" .= version
      , "number" .= number
      , "confirmed" .= confirmed
      , "utxo" .= utxo
      , "utxoToCommit" .= utxoToCommit
      , "utxoToDecommit" .= utxoToDecommit
      , "depositTxId" .= depositTxId
      , "accumulator" .= String (decodeUtf8 $ Base16.encode $ Accumulator.getAccumulatorHash accumulator)
      , "appliedAccumulator" .= String (decodeUtf8 $ Base16.encode $ Accumulator.getAccumulatorHash appliedAccumulator)
      ]

instance IsTx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj -> do
    headId <- obj .: "headId"
    version <- obj .: "version"
    number <- obj .: "number"
    confirmed <- obj .: "confirmed"
    utxo <- obj .: "utxo"
    utxoToCommit <-
      obj .:? "utxoToCommit" >>= \case
        Nothing -> pure mempty
        (Just utxoC) -> pure utxoC
    utxoToDecommit <-
      obj .:? "utxoToDecommit" >>= \case
        Nothing -> pure mempty
        (Just utxoD) -> pure utxoD
    depositTxId <- obj .:? "depositTxId"
    -- The "accumulator" and "appliedAccumulator" JSON fields carry only the
    -- hashes for display; both accumulators are always rebuilt from the UTxO sets.
    -- SECURITY: never trust a hash from the JSON instead of rebuilding. This
    -- instance is reachable from untrusted client input (SideLoadSnapshot),
    -- and the accumulator hashes are what multisignatures verify against, so
    -- they must always be derived from the UTxO content.
    --
    -- SECURITY: the rebuilt accumulators are bottom when the UTxO set is larger
    -- than the trusted setup can commit to ('computeG1CommitmentBytes' errors
    -- above 'Accumulator.maxAccumulatorSize'), and they are lazy, so this decode
    -- succeeds for a set of any size and the failure surfaces wherever the
    -- accumulator is first forced. The size is deliberately not bounded here:
    -- this codec is also the persistence format, replayed from the event store
    -- at startup, where a rejected decode would stop the node from starting.
    -- Every client API entry point bounds it instead, before the value can be
    -- queued, logged or echoed -- see 'Hydra.API.ClientInput.validateClientInput'
    -- and 'Hydra.API.HTTPServer.handleSideLoadSnapshot'.
    let (accumulator, appliedAccumulator) = Accumulator.buildFromSnapshotUTxOs utxo utxoToCommit utxoToDecommit
    pure $ Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit, depositTxId, accumulator, appliedAccumulator}

-- | Tag of the current on-disk\/wire layout, which carries 'depositTxId'.
--
-- The fields are a bare concatenation with no length prefix, so a layout change
-- is only decodable when the tag distinguishes it: 'snapshotCBORTagV1' names the
-- one written before 'depositTxId' existed and is still accepted, letting a node
-- replay an event log from an earlier version.
snapshotCBORTag :: Text
snapshotCBORTag = "Snapshot2"

-- | Tag of the layout without 'depositTxId'. Decoded, never written.
snapshotCBORTagV1 :: Text
snapshotCBORTagV1 = "Snapshot"

-- NOTE: Like the JSON encoding, the accumulators are not transmitted (only
-- derived data) and get rebuilt from the UTxO sets on decode. This is why
-- the codec stays hand-written.
instance IsTx tx => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, depositTxId, utxoToDecommit} =
    toCBOR snapshotCBORTag
      <> toCBOR headId
      <> toCBOR version
      <> toCBOR number
      <> toCBOR confirmed
      <> toCBOR utxo
      <> toCBOR utxoToCommit
      <> toCBOR depositTxId
      <> toCBOR utxoToDecommit

instance IsTx tx => FromCBOR (Snapshot tx) where
  fromCBOR =
    fromCBOR >>= \case
      (tag :: Text)
        | tag == snapshotCBORTag -> decodeSnapshot True
        | tag == snapshotCBORTagV1 -> decodeSnapshot False
        | otherwise -> fail $ show tag <> " is not a proper CBOR-encoded Snapshot"
   where
    decodeSnapshot hasDepositTxId = do
      headId <- fromCBOR
      version <- fromCBOR
      number <- fromCBOR
      confirmed <- fromCBOR
      utxo <- fromCBOR
      utxoToCommit <- fromCBOR
      -- A snapshot from before this field existed names no deposit, so an
      -- increment of it cannot validate; only replaying it has to work.
      depositTxId <- if hasDepositTxId then fromCBOR else pure Nothing
      utxoToDecommit <- fromCBOR
      -- SECURITY: as in the 'FromJSON' instance above, both accumulators are
      -- rebuilt (never trusted from the wire) and are bottom above
      -- 'Accumulator.maxAccumulatorSize'; the bound is enforced at the client
      -- API boundary, not here.
      let (accumulator, appliedAccumulator) = Accumulator.buildFromSnapshotUTxOs @tx utxo utxoToCommit utxoToDecommit
      pure Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, depositTxId, utxoToDecommit, accumulator, appliedAccumulator}

-- | All UTxOs represented by this snapshot: settled plus any pending commit/decommit.
snapshotUTxO :: IsTx tx => Snapshot tx -> UTxOType tx
snapshotUTxO Snapshot{utxo, utxoToCommit, utxoToDecommit} =
  combinedUTxO utxo utxoToCommit utxoToDecommit

-- * ConfirmedSnapshot

-- | A snapshot that can be used to close a head with. Either the initial one,
-- or when it was signed by all parties, i.e. it is confirmed.
data ConfirmedSnapshot tx
  = InitialSnapshot
      { -- XXX: 'headId' is actually unused. Only 'getSnapshot' forces this to exist.
        headId :: HeadId
      }
  | ConfirmedSnapshot
      { snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

instance IsTx tx => ToCBOR (ConfirmedSnapshot tx) where
  toCBOR = genericToCBOR

instance IsTx tx => FromCBOR (ConfirmedSnapshot tx) where
  fromCBOR = genericFromCBOR

-- | Safely get a 'Snapshot' from a confirmed snapshot.
--
-- NOTE: While we could use 'snapshot' directly, this is a record-field accessor
-- which may become partial (and lead to unnoticed runtime errors) if we ever
-- add a new branch to the sumtype. So, we explicitly define a getter which
-- will force us into thinking about changing the signature properly if this
-- happens.
getSnapshot :: forall tx. IsTx tx => ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{headId} ->
    Snapshot
      { headId
      , version = 0
      , number = 0
      , confirmed = []
      , utxo = mempty
      , utxoToCommit = Nothing
      , utxoToDecommit = Nothing
      , depositTxId = Nothing
      , accumulator = emptyAccumulator
      , appliedAccumulator = emptyAccumulator
      }
  ConfirmedSnapshot{snapshot} -> snapshot
 where
  emptyAccumulator = Accumulator.buildFromUTxO @tx mempty
