{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hydra.Tx.Snapshot where

import Hydra.Prelude

import Hydra.Cardano.Api (SigningKey)
import Hydra.Tx.Crypto (HydraKey, aggregate, sign)
import Hydra.Tx.HeadId (HeadId)
import Hydra.Tx.IsTx (IsTx (..))
import Hydra.Tx.Snapshot (ConfirmedSnapshot (..), Snapshot (..), SnapshotNumber (..), SnapshotVersion (..))
import Test.QuickCheck (frequency, suchThat)
import Test.QuickCheck.Instances.Natural ()

instance Arbitrary SnapshotNumber where
  arbitrary = UnsafeSnapshotNumber <$> arbitrary

instance Arbitrary SnapshotVersion where
  arbitrary = UnsafeSnapshotVersion <$> arbitrary

instance (Arbitrary (UTxOType tx), Arbitrary (TxIdType tx)) => Arbitrary (Snapshot tx) where
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
instance (Arbitrary (UTxOType tx), Arbitrary (TxIdType tx), IsTx tx) => Arbitrary (ConfirmedSnapshot tx) where
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
