{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (..))
import Data.Aeson (object, withObject, (.:), (.=))
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger (IsTx (..))
import Test.QuickCheck (frequency, suchThat)

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxOType tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance IsTx tx => Eq (Snapshot tx)
deriving instance IsTx tx => Show (Snapshot tx)

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

-- REVIEW: Why is the @tx necessary here? It surprised us a bit that we need it.
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{number, utxo} =
    serialize' number <> hashUTxO @tx utxo

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

-- | Snapshot when it was signed by all parties, i.e. it is confirmed.
data ConfirmedSnapshot tx
  = InitialSnapshot
      { snapshot :: Snapshot tx
      }
  | ConfirmedSnapshot
      { snapshot :: Snapshot tx
      , signatures :: Hydra.MultiSignature (Snapshot tx)
      }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

-- NOTE: While we could use 'snapshot' directly, this is a record-field accessor
-- which may become partial (and lead to unnoticed runtime errors) if we ever
-- add a new branch to the sumtype. So, we explicitely define a getter which
-- will force us into thinking about changing the signature properly if this
-- happens.

-- | Safely get a 'Snapshot' from a confirmed snapshot.
getSnapshot :: ConfirmedSnapshot tx -> Snapshot tx
getSnapshot = \case
  InitialSnapshot{snapshot} -> snapshot
  ConfirmedSnapshot{snapshot} -> snapshot

-- | Tell whether a snapshot is the initial snapshot coming from the collect-com
-- transaction.
isInitialSnapshot :: ConfirmedSnapshot tx -> Bool
isInitialSnapshot = \case
  InitialSnapshot{} -> True
  ConfirmedSnapshot{} -> False

instance IsTx tx => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = do
    ks <- fmap Hydra.generateSigningKey <$> arbitrary
    genConfirmedSnapshot ks

genConfirmedSnapshot ::
  IsTx tx =>
  [Hydra.SigningKey] ->
  Gen (ConfirmedSnapshot tx)
genConfirmedSnapshot sks =
  frequency
    [ (1, initialSnapshot)
    , (9, confirmedSnapshot)
    ]
 where
  initialSnapshot = do
    s <- arbitrary
    -- FIXME: The fact that we need to set a constant 0 here is a code smell.
    -- Initial snapshots with a different snapshot number are not valid and we
    -- should model 'InitialSnapshot' differently, i.e not holding a
    -- SnapshotNumber
    pure InitialSnapshot{snapshot = s{number = 0}}

  confirmedSnapshot = do
    -- FIXME: This is another nail in the coffin to our current modeling of
    -- snapshots
    snapshot <- arbitrary `suchThat` \Snapshot{number} -> number > 0
    let signatures = Hydra.aggregate $ fmap (`Hydra.sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}
