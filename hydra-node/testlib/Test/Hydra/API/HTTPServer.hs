{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.API.HTTPServer where

import "hydra-node" Hydra.API.HTTPServer (DraftCommitTxRequest (..), DraftCommitTxResponse (..), SideLoadSnapshotRequest (..), SubmitL2TxRequest (..), SubmitL2TxResponse, SubmitTxRequest (..), TransactionSubmitted)
import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "hydra-tx" Hydra.Tx (IsTx (..), UTxOType)
import "hydra-tx" Test.Hydra.Tx.Gen ()

instance Arbitrary tx => Arbitrary (DraftCommitTxResponse tx) where
  arbitrary = genericArbitrary

  shrink = \case
    DraftCommitTxResponse xs -> DraftCommitTxResponse <$> shrink xs

instance (Arbitrary tx, Arbitrary (UTxOType tx)) => Arbitrary (DraftCommitTxRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    SimpleCommitRequest u -> SimpleCommitRequest <$> shrink u
    FullCommitRequest a b c -> FullCommitRequest <$> shrink a <*> shrink b <*> shrink c

deriving newtype instance Arbitrary tx => Arbitrary (SubmitTxRequest tx)

instance Arbitrary TransactionSubmitted where
  arbitrary = genericArbitrary

instance (Arbitrary tx, Arbitrary (UTxOType tx), IsTx tx) => Arbitrary (SideLoadSnapshotRequest tx) where
  arbitrary = genericArbitrary

  shrink = \case
    SideLoadSnapshotRequest snapshot -> SideLoadSnapshotRequest <$> shrink snapshot

deriving newtype instance Arbitrary tx => Arbitrary (SubmitL2TxRequest tx)

instance Arbitrary SubmitL2TxResponse where
  arbitrary = genericArbitrary
