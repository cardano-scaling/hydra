{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Hydra.HeadLogic.State where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.ChainState (IsChainState (..))
import Hydra.HeadLogic.State (
  ClosedState (..),
  CoordinatedHeadState (..),
  FanoutMode (..),
  FanoutStepLanded (..),
  HeadState (..),
  IdleState (..),
  OpenState (..),
  PartialFanoutState (..),
  SeenSnapshot (..),
  Settlement (..),
  SettlementStatus (..),
  Settlements,
  emptySettlements,
  mkSeenSnapshot,
  tryInsertSettlement,
 )
import Hydra.Tx.Snapshot qualified as Snapshot
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck (listOf, oneof, resize)

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (HeadState tx) where
  arbitrary = genericArbitrary

instance Arbitrary (ChainStateType tx) => Arbitrary (IdleState tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (OpenState tx) where
  arbitrary =
    OpenState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ArbitraryIsTx tx => Arbitrary (CoordinatedHeadState tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (SeenSnapshot tx) where
  arbitrary =
    oneof
      [ pure NoSeenSnapshot
      , LastSeenSnapshot <$> arbitrary
      , RequestedSnapshot <$> arbitrary <*> arbitrary
      , mkSeenSnapshot <$> arbitrary <*> arbitrary
      ]

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (ClosedState tx) where
  arbitrary =
    ClosedState
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance ArbitraryIsTx tx => Arbitrary (FanoutMode tx) where
  arbitrary = genericArbitrary

instance ArbitraryIsTx tx => Arbitrary (FanoutStepLanded tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx)) => Arbitrary (PartialFanoutState tx) where
  arbitrary = genericArbitrary

instance Arbitrary SettlementStatus where
  arbitrary = genericArbitrary

instance forall tx. ArbitraryIsTx tx => Arbitrary (Settlements tx) where
  arbitrary = do
    candidates <- reasonablySized $ listOf genEntry
    pure $ foldl' keepAccepted emptySettlements candidates
   where
    genEntry :: Gen (Settlement tx)
    genEntry = do
      version <- arbitrary
      snap <- resize 2 arbitrary
      status <- arbitrary
      pure
        Settlement
          { snapshot =
              Snapshot.ConfirmedSnapshot
                (snap{Snapshot.version = version})
                mempty
          , status
          }

    keepAccepted :: Settlements tx -> Settlement tx -> Settlements tx
    keepAccepted current entry =
      case tryInsertSettlement entry current of
        Left _ -> current
        Right updated -> updated
