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
import PlutusLedgerApi.V2 (toBuiltin, toData)
import Test.QuickCheck (frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

instance Arbitrary SnapshotNumber where
  arbitrary = UnsafeSnapshotNumber <$> arbitrary

newtype SnapshotVersion
  = UnsafeSnapshotVersion Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

instance Arbitrary SnapshotVersion where
  arbitrary = UnsafeSnapshotVersion <$> arbitrary

data Snapshot tx = Snapshot
  { headId :: HeadId
  , number :: SnapshotNumber
  , utxo :: UTxOType tx
  , confirmed :: [TxIdType tx]
  -- ^ The set of transactions that lead to 'utxo'
  , utxoToDecommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be decommitted. Spec: Ûω
  -- TODO: what is the difference between Noting and (Just mempty) here?
  -- | Snapshot version is 0 at start and is only bumped further on each
  -- decommit that happens.
  , version :: SnapshotVersion
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON Snapshot{headId, number, utxo, confirmed, utxoToDecommit, version} =
    object
      ( [ "headId" .= headId
        , "snapshotNumber" .= number
        , "utxo" .= utxo
        , "confirmedTransactions" .= confirmed
        ]
          <> maybe mempty (pure . ("utxoToDecommit" .=)) utxoToDecommit
          <> ["version" .= version]
      )

instance IsTx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "headId")
      <*> (obj .: "snapshotNumber")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmedTransactions")
      <*> ( obj .:? "utxoToDecommit" >>= \case
              Nothing -> pure mempty
              (Just utxo) -> pure utxo
          )
      <*> (obj .: "version")

instance IsTx tx => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink Snapshot{headId, number, utxo, confirmed, utxoToDecommit, version} =
    [ Snapshot headId number utxo' confirmed' utxoToDecommit' version'
    | utxo' <- shrink utxo
    , confirmed' <- shrink confirmed
    , utxoToDecommit' <- shrink utxoToDecommit
    , version' <- shrink version
    ]

-- | Binary representation of snapshot signatures
-- CDDL definition for 'Snapshot'. Parseable either from JSON Snapshot
-- representation or hex encoded CBOR string:
--
-- snapshot = {
--    headId : text
--    snapshotNumber : uint
--    confirmedTransactions  : []
--    utxo : {}
--    utxoToDecommit : {}
--    version : uint
--   } / bytes
--
-- root = [* snapshot ]
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{number, headId, utxo, utxoToDecommit, version} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger number) -- CBOR(I(integer))
        <> serialise (toData . toBuiltin $ hashUTxO @tx utxo) -- CBOR(B(bytestring)
        <> serialise (toData . toBuiltin . hashUTxO @tx $ fromMaybe mempty utxoToDecommit) -- CBOR(B(bytestring)
        <> serialise (toData . toBuiltin $ toInteger version) -- CBOR(I(integer))

instance (Typeable tx, ToCBOR (UTxOType tx), ToCBOR (TxIdType tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{headId, number, utxo, confirmed, utxoToDecommit, version} =
    toCBOR headId
      <> toCBOR number
      <> toCBOR utxo
      <> toCBOR confirmed
      <> toCBOR utxoToDecommit
      <> toCBOR version

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
getSnapshot :: Monoid (UTxOType tx) => ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{headId, initialUTxO} ->
    Snapshot
      { headId
      , number = 0
      , utxo = initialUTxO
      , confirmed = []
      , utxoToDecommit = mempty
      , version = 0
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
  -- | The lower bound on snapshot number to generate.
  -- If this is 0, then we can generate an `InitialSnapshot` or a `ConfirmedSnapshot`.
  -- Otherwise we generate only `ConfirmedSnapshot` with a number strictly superior to
  -- this lower bound.
  SnapshotNumber ->
  SnapshotVersion ->
  UTxOType tx ->
  Maybe (UTxOType tx) ->
  [SigningKey HydraKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot headId minSn version utxo utxoToDecommit sks
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
    let snapshot = Snapshot{headId, number, utxo, confirmed = [], utxoToDecommit, version}
    let signatures = aggregate $ fmap (`sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}

fromChainSnapshot :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshot onChainSnapshotNumber =
  maybe
    (error "Failed to convert on-chain SnapShotNumber to off-chain one.")
    UnsafeSnapshotNumber
    (integerToNatural onChainSnapshotNumber)
