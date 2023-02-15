{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.API.Snapshot where

import Prelude

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import Cardano.Crypto.Util (SignableRepresentation)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import GHC.Generics (Generic)
import Generic.Random (genericArbitrary, uniform)
import Hydra.API.Crypto (HydraKey, MultiSignature, aggregate, generateSigningKey, sign)
import Hydra.API.Ledger (IsTx (..))
import Hydra.Cardano.Api (SigningKey)
import Numeric.Natural (Natural)
import Test.QuickCheck (Arbitrary (..), Gen, frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

newtype SnapshotNumber
  = UnsafeSnapshotNumber Natural
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (ToJSON, FromJSON, ToCBOR, FromCBOR, Real, Num, Enum, Integral)

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxOType tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance IsTx tx => Eq (Snapshot tx)
deriving instance IsTx tx => Show (Snapshot tx)

instance Arbitrary SnapshotNumber where
  arbitrary = UnsafeSnapshotNumber <$> arbitrary

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary uniform

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

instance IsTx tx => ToJSON (Snapshot tx) where
  toJSON s =
    object
      [ "snapshotNumber" .= number s
      , "utxo" .= utxo s
      , "confirmedTransactions" .= confirmed s
      ]

instance IsTx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "snapshotNumber")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmedTransactions")

instance (ToCBOR tx, ToCBOR (UTxOType tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} =
    toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UTxOType tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | A snapshot that can be used to close a head with. Either the initial one, or when it was signed by all parties, i.e. it is confirmed.
data ConfirmedSnapshot tx
  = InitialSnapshot {initialUTxO :: UTxOType tx}
  | ConfirmedSnapshot
      { snapshot :: Snapshot tx
      , signatures :: MultiSignature (Snapshot tx)
      }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

instance (IsTx tx, SignableRepresentation (Snapshot tx)) => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = do
    ks <- fmap generateSigningKey <$> arbitrary
    utxo <- arbitrary
    genConfirmedSnapshot 0 utxo ks

genConfirmedSnapshot ::
  (SignableRepresentation (Snapshot tx)) =>
  -- | The lower bound on snapshot number to generate.
  -- If this is 0, then we can generate an `InitialSnapshot` or a `ConfirmedSnapshot`.
  -- Otherwise we generate only `ConfirmedSnapshot` with a number strictly superior to
  -- this lower bound.
  SnapshotNumber ->
  UTxOType tx ->
  [SigningKey HydraKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot minSn utxo sks
  | minSn > 0 = confirmedSnapshot
  | otherwise =
    frequency
      [ (1, initialSnapshot)
      , (9, confirmedSnapshot)
      ]
 where
  initialSnapshot =
    pure $ InitialSnapshot utxo

  confirmedSnapshot = do
    -- FIXME: This is another nail in the coffin to our current modeling of
    -- snapshots
    number <- arbitrary `suchThat` (> minSn)
    let snapshot = Snapshot{number, utxo, confirmed = []}
    let signatures = aggregate $ fmap (`sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}
