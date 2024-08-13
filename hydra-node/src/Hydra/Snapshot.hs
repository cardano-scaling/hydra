{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (serialise)
import Data.Aeson (object, withObject, (.:), (.:?), (.=))
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (SigningKey)
import Hydra.Cardano.Api.Prelude (serialiseToRawBytes)
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.HeadId (HeadId)
import Hydra.Ledger (IsTx (..))
import PlutusLedgerApi.V3 (toBuiltin, toData)
import Test.QuickCheck (frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

instance Arbitrary SnapshotNumber where
  arbitrary = UnsafeSnapshotNumber <$> arbitrary

-- NOTE: On-chain scripts ensure snapshot number does not become negative.
fromChainSnapshotNumber :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshotNumber =
  UnsafeSnapshotNumber . fromMaybe 0 . integerToNatural

newtype SnapshotVersion
  = UnsafeSnapshotVersion Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

instance Arbitrary SnapshotVersion where
  arbitrary = UnsafeSnapshotVersion <$> arbitrary

-- NOTE: On-chain scripts ensure snapshot version does not become negative.
fromChainSnapshotVersion :: Onchain.SnapshotVersion -> SnapshotVersion
fromChainSnapshotVersion =
  UnsafeSnapshotVersion . fromMaybe 0 . integerToNatural

data Snapshot tx = Snapshot
  { headId :: HeadId
  , version :: SnapshotVersion
  -- ^ Open state version this snapshot is based on.
  , number :: SnapshotNumber
  -- ^ Monotonically increasing snapshot number.
  , confirmed :: [TxIdType tx]
  , utxo :: UTxOType tx
  -- ^ The set of transactions that lead to 'utxo'
  , utxoToDecommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be decommitted. Spec: Ûω
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON Snapshot{headId, number, utxo, confirmed, utxoToDecommit, version} =
    object
      [ "headId" .= headId
      , "version" .= version
      , "snapshotNumber" .= number
      , "confirmedTransactions" .= confirmed
      , "utxo" .= utxo
      , "utxoToDecommit" .= utxoToDecommit
      ]

instance IsTx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "headId")
      <*> (obj .: "version")
      <*> (obj .: "snapshotNumber")
      <*> (obj .: "confirmedTransactions")
      <*> (obj .: "utxo")
      <*> ( obj .:? "utxoToDecommit" >>= \case
              Nothing -> pure mempty
              (Just utxo) -> pure utxo
          )

instance IsTx tx => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink Snapshot{headId, version, number, utxo, confirmed, utxoToDecommit} =
    [ Snapshot headId version number confirmed' utxo' utxoToDecommit'
    | confirmed' <- shrink confirmed
    , utxo' <- shrink utxo
    , utxoToDecommit' <- shrink utxoToDecommit
    ]

-- | Binary representation of snapshot signatures. That is, concatenated CBOR for
-- 'headId', 'version', 'number', 'utxoHash' and 'utxoToDecommitHash' according
-- to CDDL schemata:
--
-- headId = bytes .size 16
-- version = uint
-- number = uint
-- utxoHash = bytes
-- utxoToDecommitHash = bytes
--
-- where hashes are the result of applying 'hashUTxO'.
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{headId, version, number, utxo, utxoToDecommit} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger version)
        <> serialise (toData . toBuiltin $ toInteger number)
        <> serialise (toData . toBuiltin $ hashUTxO @tx utxo)
        <> serialise (toData . toBuiltin . hashUTxO @tx $ fromMaybe mempty utxoToDecommit)

instance (Typeable tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{headId, number, utxo, confirmed, utxoToDecommit, version} =
    toCBOR headId
      <> toCBOR version
      <> toCBOR number
      <> toCBOR confirmed
      <> toCBOR utxo
      <> toCBOR utxoToDecommit

instance (Typeable tx, FromCBOR (UTxOType tx), FromCBOR (TxIdType tx)) => FromCBOR (Snapshot tx) where
  fromCBOR =
    Snapshot
      <$> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR
      <*> fromCBOR

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

-- NOTE: While we could use 'snapshot' directly, this is a record-field accessor
-- which may become partial (and lead to unnoticed runtime errors) if we ever
-- add a new branch to the sumtype. So, we explicitely define a getter which
-- will force us into thinking about changing the signature properly if this
-- happens.

-- | Safely get a 'Snapshot' from a confirmed snapshot.
getSnapshot :: ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{headId, initialUTxO} ->
    Snapshot
      { headId
      , version = 0
      , number = 0
      , confirmed = []
      , utxo = initialUTxO
      , utxoToDecommit = Nothing
      }
  ConfirmedSnapshot{snapshot} -> snapshot

-- | Tell whether a snapshot is the initial snapshot coming from the collect-com
-- transaction.
isInitialSnapshot :: ConfirmedSnapshot tx -> Bool
isInitialSnapshot = \case
  InitialSnapshot{} -> True
  ConfirmedSnapshot{} -> False

instance IsTx tx => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = do
    ks <- arbitrary
    utxo <- arbitrary
    utxoToDecommit <- arbitrary
    headId <- arbitrary
    genConfirmedSnapshot headId 0 0 utxo utxoToDecommit ks

  shrink = \case
    InitialSnapshot hid sn -> [InitialSnapshot hid sn' | sn' <- shrink sn]
    ConfirmedSnapshot sn sigs -> ConfirmedSnapshot <$> shrink sn <*> shrink sigs

genConfirmedSnapshot ::
  IsTx tx =>
  HeadId ->
  -- | Exact snapshot version to generate.
  SnapshotVersion ->
  -- | The lower bound on snapshot number to generate.
  -- If this is 0, then we can generate an `InitialSnapshot` or a `ConfirmedSnapshot`.
  -- Otherwise we generate only `ConfirmedSnapshot` with a number strictly superior to
  -- this lower bound.
  SnapshotNumber ->
  UTxOType tx ->
  Maybe (UTxOType tx) ->
  [SigningKey HydraKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot headId version minSn utxo utxoToDecommit sks
  | minSn > 0 = confirmedSnapshot
  | otherwise =
      frequency
        [ (1, initialSnapshot)
        , (9, confirmedSnapshot)
        ]
 where
  initialSnapshot =
    InitialSnapshot <$> arbitrary <*> pure utxo

  confirmedSnapshot = do
    -- FIXME: This is another nail in the coffin to our current modeling of
    -- snapshots
    number <- arbitrary `suchThat` (> minSn)
    let snapshot = Snapshot{headId, version, number, confirmed = [], utxo, utxoToDecommit}
    let signatures = aggregate $ fmap (`sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}
