{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Cardano.Crypto.Util (SignableRepresentation (..))
import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Ledger (Tx, Utxo)
import Hydra.Prelude

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: Utxo tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance Tx tx => Eq (Snapshot tx)
deriving instance Tx tx => Show (Snapshot tx)

instance (Arbitrary tx, Arbitrary (Utxo tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

instance Tx tx => SignableRepresentation (Snapshot tx) where
  -- FIXME: This should really use the CBOR representation for signing.
  --
  -- getSignableRepresentation = CBOR.toStrictByteString . toCBOR
  getSignableRepresentation = encodeUtf8 . show @Text

instance Tx tx => ToJSON (Snapshot tx) where
  toJSON s =
    object
      [ "snapshotNumber" .= number s
      , "utxo" .= utxo s
      , "confirmedTransactions" .= confirmed s
      ]

instance Tx tx => FromJSON (Snapshot tx) where
  parseJSON = withObject "Snapshot" $ \obj ->
    Snapshot
      <$> (obj .: "snapshotNumber")
      <*> (obj .: "utxo")
      <*> (obj .: "confirmedTransactions")

instance (ToCBOR tx, ToCBOR (Utxo tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} =
    toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (Utxo tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR
