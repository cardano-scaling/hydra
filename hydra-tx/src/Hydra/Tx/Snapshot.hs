{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Tx.Snapshot where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (serialise)
import Data.Aeson (Value (String), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LBS
import Hydra.Cardano.Api (SerialiseAsRawBytes (..), SigningKey)
import Hydra.Contract.HeadState qualified as Onchain
import Hydra.Tx.Crypto (HydraKey, MultiSignature, aggregate, sign)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (IsTx (..))
import PlutusLedgerApi.V3 (toBuiltin, toData)
import Test.QuickCheck (frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

-- * SnapshotNumber and SnapshotVersion

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral, Arbitrary)

-- NOTE: On-chain scripts ensure snapshot number does not become negative.
fromChainSnapshotNumber :: Onchain.SnapshotNumber -> SnapshotNumber
fromChainSnapshotNumber =
  UnsafeSnapshotNumber . fromMaybe 0 . integerToNatural

newtype SnapshotVersion
  = UnsafeSnapshotVersion Natural
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral, Arbitrary)

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
  , utxoHash :: ByteString
  -- ^ The 'UTxO' hash of this snapshot. Spec: η
  , utxoToCommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be committed. Spec: Uα
  , utxoToDecommit :: Maybe (UTxOType tx)
  -- ^ UTxO to be decommitted. Spec: Uω
  }
  deriving stock (Generic)

deriving stock instance IsTx tx => Eq (Snapshot tx)
deriving stock instance IsTx tx => Show (Snapshot tx)

-- | Binary representation of snapshot signatures. That is, concatenated CBOR for
-- 'headId', 'version', 'number', 'utxoHash' and 'utxoToDecommitHash' according
-- to CDDL schemata:
--
-- headId = bytes .size 16
-- version = uint
-- number = uint
-- utxoHash = bytes
-- utxoToCommitHash = bytes
-- utxoToDecommitHash = bytes
--
-- where hashes are the result of applying 'hashUTxO'.
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{headId, version, number, utxoHash, utxoToCommit, utxoToDecommit} =
    LBS.toStrict $
      serialise (toData . toBuiltin $ serialiseToRawBytes headId)
        <> serialise (toData . toBuiltin $ toInteger version)
        <> serialise (toData . toBuiltin $ toInteger number)
        <> serialise (toData $ toBuiltin utxoHash)
        <> serialise (toData . toBuiltin . hashUTxO @tx $ fromMaybe mempty utxoToCommit)
        <> serialise (toData . toBuiltin . hashUTxO @tx $ fromMaybe mempty utxoToDecommit)

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON Snapshot{headId, number, utxo, utxoHash, confirmed, utxoToCommit, utxoToDecommit, version} =
    object
      [ "headId" .= headId
      , "version" .= version
      , "number" .= number
      , "confirmed" .= confirmed
      , "utxo" .= utxo
      , "utxoHash" .= String (decodeUtf8 $ Base16.encode utxoHash)
      , "utxoToCommit" .= utxoToCommit
      , "utxoToDecommit" .= utxoToDecommit
      ]

instance IsTx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "headId")
      <*> (obj .: "version")
      <*> (obj .: "number")
      <*> (obj .: "confirmed")
      <*> (obj .: "utxo")
      <*> (obj .: "utxoHash" >>= parseBase16)
      <*> ( obj .:? "utxoToCommit" >>= \case
              Nothing -> pure mempty
              (Just utxo) -> pure utxo
          )
      <*> ( obj .:? "utxoToDecommit" >>= \case
              Nothing -> pure mempty
              (Just utxo) -> pure utxo
          )
   where
    parseBase16 :: Text -> Parser ByteString
    parseBase16 t =
      case Base16.decode (encodeUtf8 t) of
        Left e -> fail $ "invalid base16: " <> show e
        Right bs -> pure bs

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink Snapshot{headId, version, number, utxo, confirmed, utxoHash, utxoToCommit, utxoToDecommit} =
    [ Snapshot headId version number confirmed' utxo' utxoHash' utxoToCommit' utxoToDecommit'
    | confirmed' <- shrink confirmed
    , utxo' <- shrink utxo
    , utxoHash' <- shrink utxoHash
    , utxoToCommit' <- shrink utxoToCommit
    , utxoToDecommit' <- shrink utxoToDecommit
    ]

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
getSnapshot :: IsTx tx => ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{headId, initialUTxO} ->
    let utxoHash = hashUTxO initialUTxO
     in Snapshot
          { headId
          , version = 0
          , number = 0
          , confirmed = []
          , utxo = initialUTxO
          , utxoHash
          , utxoToCommit = Nothing
          , utxoToDecommit = Nothing
          }
  ConfirmedSnapshot{snapshot} -> snapshot

instance (Arbitrary tx, Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = do
    ks <- arbitrary
    utxo <- arbitrary
    utxoToCommit <- arbitrary
    utxoToDecommit <- arbitrary
    headId <- arbitrary
    genConfirmedSnapshot headId 0 0 utxo utxoToCommit utxoToDecommit ks

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
  Maybe (UTxOType tx) ->
  [SigningKey HydraKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot headId version minSn utxo utxoToCommit utxoToDecommit sks
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
    let utxoHash = hashUTxO utxo
    let snapshot = Snapshot{headId, version, number, confirmed = [], utxo, utxoHash, utxoToCommit, utxoToDecommit}
    let signatures = aggregate $ fmap (`sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}
