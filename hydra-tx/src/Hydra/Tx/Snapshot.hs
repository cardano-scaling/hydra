{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Tx.Snapshot where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (deserialiseOrFail, serialise)
import Data.Aeson (Value (String), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (SerialiseAsRawBytes (..))
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (IsTx (..))
import PlutusLedgerApi.V3 (toBuiltin, toData)

-- * SnapshotNumber and SnapshotVersion

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

-- NOTE: On-chain scripts ensure snapshot number does not become negative.
fromChainSnapshotNumber :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshotNumber =
  UnsafeSnapshotNumber . fromMaybe 0 . integerToNatural

newtype SnapshotVersion
  = UnsafeSnapshotVersion Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

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
  , utxoToDecommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be decommitted. Spec: Uω
  , accumulator :: Accumulator.HydraAccumulator
  -- ^ The cryptographic accumulator built from UTxO hashes. Spec: A
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

-- | Binary representation of snapshot signatures. That is, concatenated CBOR for
-- 'headId', 'version', 'number', 'utxoHash', 'utxoToCommitHash', 'utxoToDecommitHash',
-- and 'accumulator' according to CDDL schemata:
--
-- headId = bytes .size 16
-- version = uint
-- number = uint
-- utxoHash = bytes
-- utxoToCommitHash = bytes
-- utxoToDecommitHash = bytes
-- accumulator = bytes  ; serialized HydraAccumulator
--
-- The accumulator is built from [utxoHash, utxoToCommitHash, utxoToDecommitHash].
-- Parties sign the snapshot containing both the individual hashes (for backward compatibility)
-- and the accumulator structure (for on-chain verification against the datum).
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{headId, version, number, utxo, utxoToCommit, utxoToDecommit, accumulator} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger version)
        <> serialise (toData . toBuiltin $ toInteger number)
        <> serialise (toData $ toBuiltin utxoHash)
        <> serialise (toData $ toBuiltin utxoToCommitHash)
        <> serialise (toData $ toBuiltin utxoToDecommitHash)
        <> serialise (toData $ toBuiltin accumulatorBytes)
   where
    utxoHash = hashUTxO utxo
    utxoToCommitHash = hashUTxO @tx $ fromMaybe mempty utxoToCommit
    utxoToDecommitHash = hashUTxO @tx $ fromMaybe mempty utxoToDecommit
    -- Serialize the accumulator for signing
    accumulatorBytes = Accumulator.getAccumulatorHash accumulator

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON Snapshot{headId, number, utxo, confirmed, utxoToCommit, utxoToDecommit, version, accumulator} =
    object
      [ "headId" .= headId
      , "version" .= version
      , "number" .= number
      , "confirmed" .= confirmed
      , "utxo" .= utxo
      , "utxoToCommit" .= utxoToCommit
      , "utxoToDecommit" .= utxoToDecommit
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
    -- Parse accumulator, or reconstruct it if not present (for backward compatibility)
    accumulator <-
      (obj .:? "accumulator" >>= traverse parseBase16) >>= \case
        Just accBytes | not (BS.null accBytes) -> do
          -- Deserialize the accumulator from its serialized form
          case deserialiseOrFail (fromStrict accBytes) of
            Left _ -> fail "Failed to deserialize accumulator"
            Right acc -> pure $ Accumulator.HydraAccumulator acc
        _ -> do
          -- Reconstruct accumulator from utxo hashes for backward compatibility (or if empty)
          pure $ Accumulator.buildFromUTxO utxo
    pure $ Snapshot{headId, version, number, confirmed, utxo, utxoToCommit, utxoToDecommit, accumulator}
   where
    parseBase16 :: Text -> Parser ByteString
    parseBase16 t =
      case Base16.decode (encodeUtf8 t) of
        Left e -> fail $ "invalid base16: " <> show e
        Right bs -> pure bs

-- * ConfirmedSnapshot

-- | A snapshot that can be used to close a head with. Either the initial one,
-- or when it was signed by all parties, i.e. it is confirmed.
data ConfirmedSnapshot tx
  = InitialSnapshot
      { -- XXX: 'headId' is actually unused. Only 'getSnapshot' forces this to exist.
        headId :: HeadId
      , initialUTxO :: UTxOType tx
      }
  | ConfirmedSnapshot
      { snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  deriving stock (Generic, Eq, Show)
  deriving anyclass (ToJSON, FromJSON)

-- | Safely get a 'Snapshot' from a confirmed snapshot.
--
-- NOTE: While we could use 'snapshot' directly, this is a record-field accessor
-- which may become partial (and lead to unnoticed runtime errors) if we ever
-- add a new branch to the sumtype. So, we explicitly define a getter which
-- will force us into thinking about changing the signature properly if this
-- happens.
getSnapshot :: forall tx. IsTx tx => ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{headId, initialUTxO} ->
    Snapshot
      { headId
      , version = 0
      , number = 0
      , confirmed = []
      , utxo = initialUTxO
      , utxoToCommit = Nothing
      , utxoToDecommit = Nothing
      , accumulator = Accumulator.buildFromUTxO initialUTxO
      }
  ConfirmedSnapshot{snapshot} -> snapshot
