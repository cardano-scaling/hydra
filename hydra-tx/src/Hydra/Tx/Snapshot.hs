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
  -- ^ The cryptographic accumulator built from UTxO hashes. Spec: A
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

-- | Binary representation of snapshot signatures. That is, concatenated CBOR for
-- 'headId', 'version', 'number', 'accumulatorHash', 'decommitOutputsHash', and
-- 'commitOutputsHash' according to CDDL schemata:
--
-- headId = bytes .size 28
-- version = uint
-- number = uint
-- accumulatorHash = bytes .size 32  ; blake2b-256 hash of the compressed G1 accumulator commitment
-- decommitOutputsHash = bytes .size 32  ; sha2-256 of the ordered decommit outputs (Uω)
-- commitOutputsHash = bytes .size 32  ; sha2-256 of the ordered commit outputs (Uα)
--                                     ; and of the deposit transaction id
--
-- The BLS accumulator commitment (bound via accumulatorHash) commits to the full
-- UTxO set. 'decommitOutputsHash' and 'commitOutputsHash' additionally bind the
-- exact ordered sets of decommit (Uω) and commit (Uα) outputs, so the on-chain
-- decrement and increment validators can recompute them from the materialized L1
-- decommit outputs / claimed deposit and reject any redirected/altered output.
--
-- 'commitOutputsHash' further binds 'depositTxId'. Committed content on its own
-- does not identify a deposit: a deposit datum is unauthenticated data anyone can
-- copy into a look-alike deposit holding less value, which would otherwise hash
-- the same and accept this snapshot's signatures. See the matching computation in
-- 'Hydra.Contract.Head.checkIncrement'.
instance IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation snapshot@Snapshot{headId, version, number, accumulator, utxoToDecommit} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger version)
        <> serialise (toData . toBuiltin $ toInteger number)
        <> serialise (toData $ toBuiltin accumulatorBytes)
        <> serialise (toData $ toBuiltin decommitOutputsHash)
        <> serialise (toData $ toBuiltin (commitOutputsHash snapshot))
   where
    accumulatorBytes = Accumulator.getAccumulatorHash accumulator
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
  toJSON Snapshot{headId, number, utxo, confirmed, utxoToCommit, utxoToDecommit, version, accumulator, depositTxId} =
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
    -- Reconstruct accumulator from all UTxOs (including commit/decommit).
    -- The "accumulator" JSON field stores only the hash (consistent with signing
    -- and on-chain datum), so we always rebuild the full accumulator from UTxOs.
    -- SECURITY: never trust a hash from the JSON instead of rebuilding. This
    -- instance is reachable from untrusted client input (SideLoadSnapshot),
    -- and the accumulator hash is what multisignatures verify against, so it
    -- must always be derived from the UTxO content.
    let accumulator = Accumulator.buildFromSnapshotUTxOs utxo utxoToCommit utxoToDecommit
    pure $ Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit, depositTxId, accumulator}

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

-- NOTE: Like the JSON encoding, the accumulator is not transmitted (only
-- derived data) and gets rebuilt from the UTxO sets on decode. This is why
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
      let accumulator = Accumulator.buildFromSnapshotUTxOs @tx utxo utxoToCommit utxoToDecommit
      pure Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, depositTxId, utxoToDecommit, accumulator}

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
      , accumulator = Accumulator.buildFromUTxO @tx mempty
      }
  ConfirmedSnapshot{snapshot} -> snapshot
