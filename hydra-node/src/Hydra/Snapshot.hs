{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Hydra.Snapshot where

import Hydra.Prelude

import Cardano.Crypto.Util (SignableRepresentation (..))
import Codec.Serialise (serialise)
import Data.Aeson (object, withObject, (.:), (.=))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Crypto (HydraKey)
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger (IsTx (..))
import Plutus.V2.Ledger.Api (toBuiltin, toData)
import Test.QuickCheck (frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

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

-- | Binary representation of snapshot signatures
-- TODO: document CDDL format, either here or on in 'Hydra.Contract.Head.verifyPartySignature'
instance forall tx. IsTx tx => SignableRepresentation (Snapshot tx) where
  getSignableRepresentation Snapshot{number, utxo} =
    toStrict $
      serialise (toData $ toInteger number) -- CBOR(I(integer))
        <> serialise (toData . toBuiltin $ hashUTxO @tx utxo) -- CBOR(B(bytestring)

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
    utxo <- arbitrary
    genConfirmedSnapshot 0 utxo ks

genConfirmedSnapshot ::
  IsTx tx =>
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
  initialSnapshot = do
    -- FIXME: The fact that we need to set a constant 0 here is a code smell.
    -- Initial snapshots with a different snapshot number are not valid and we
    -- should model 'InitialSnapshot' differently, i.e not holding a
    -- SnapshotNumber
    pure
      InitialSnapshot
        { snapshot =
            Snapshot
              { confirmed = []
              , utxo
              , number = 0
              }
        }

  confirmedSnapshot = do
    -- FIXME: This is another nail in the coffin to our current modeling of
    -- snapshots
    number <- arbitrary `suchThat` (> minSn)
    let snapshot = Snapshot{number, utxo, confirmed = []}
    let signatures = Hydra.aggregate $ fmap (`Hydra.sign` snapshot) sks
    pure $ ConfirmedSnapshot{snapshot, signatures}
