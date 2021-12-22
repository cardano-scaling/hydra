{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Hydra.Prelude

import Cardano.Binary (serialize')
import Cardano.Crypto.Util (SignableRepresentation (..))
import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Ledger (IsTx (..))
import Hydra.Party (MultiSigned)

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UtxoType tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance IsTx tx => Eq (Snapshot tx)
deriving instance IsTx tx => Show (Snapshot tx)

instance (Arbitrary tx, Arbitrary (UtxoType tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

-- FIXME(1): Use extra key:value map to pass signable representation to on-chain code
--
-- We should use a proper signable representation which we can also
-- get back to on-chain. In practice, we probably want to define it as an extra
-- map data-hash -> data so that we can:
--
-- (a) Leverage the Ledger phase-1 validation to do the hashing off-chain and
-- verify it.
-- (b) Have an easy way to lookup data to get a hash representation of it, for
-- signature verification.
--
-- FIXME(2): Also include UTXO in the signable representation.
--
-- FIXME(3): Use hash digest as signable representations (not pre-images).
instance SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{number} =
    serialize' number

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

instance (ToCBOR tx, ToCBOR (UtxoType tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} =
    toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UtxoType tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR

-- | Snapshot when it was signed by all parties, i.e. it is confirmed.
data ConfirmedSnapshot tx
  = InitialSnapshot
      { snapshot :: Snapshot tx
      }
  | ConfirmedSnapshot
      { snapshot :: Snapshot tx
      , signatures :: MultiSigned (Snapshot tx)
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

instance (Arbitrary tx, Arbitrary (UtxoType tx)) => Arbitrary (ConfirmedSnapshot tx) where
  arbitrary = genericArbitrary
