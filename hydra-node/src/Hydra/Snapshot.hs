{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Hydra.Prelude
import Hydra.Ledger (UTxO, Tx)
import Cardano.Crypto.Util (SignableRepresentation(..))
import Data.Aeson (object, (.=), withObject, (.:))

type SnapshotNumber = Natural

data Snapshot tx = Snapshot
  { number :: SnapshotNumber
  , utxo :: UTxO tx
  , -- | The set of transactions that lead to 'utxo'
    confirmed :: [tx]
  }
  deriving (Generic)

deriving instance Tx tx => Eq (Snapshot tx)
deriving instance Tx tx => Show (Snapshot tx)
deriving instance Tx tx => Read (Snapshot tx)

instance (Arbitrary tx, Arbitrary (UTxO tx)) => Arbitrary (Snapshot tx) where
  arbitrary = genericArbitrary

  -- NOTE: See note on 'Arbitrary (ClientInput tx)'
  shrink s =
    [ Snapshot (number s) utxo' confirmed'
    | utxo' <- shrink (utxo s)
    , confirmed' <- shrink (confirmed s)
    ]

instance Tx tx => SignableRepresentation (Snapshot tx) where
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

instance (ToCBOR tx, ToCBOR (UTxO tx)) => ToCBOR (Snapshot tx) where
  toCBOR Snapshot{number, utxo, confirmed} =
    toCBOR number <> toCBOR utxo <> toCBOR confirmed

instance (FromCBOR tx, FromCBOR (UTxO tx)) => FromCBOR (Snapshot tx) where
  fromCBOR = Snapshot <$> fromCBOR <*> fromCBOR <*> fromCBOR
