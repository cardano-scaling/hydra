{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hydra.Chain where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain (ChainEvent, ChainStateHistory, OnChainTx, PostChainTx (..), PostTxError)
import Hydra.Chain.ChainState (ChainStateType (..), IsChainState)
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Hydra.Tx.Gen (ArbitraryIsTx)
import Test.QuickCheck.Instances.Semigroup ()
import Test.QuickCheck.Instances.Time ()

instance ArbitraryIsTx tx => Arbitrary (PostChainTx tx) where
  arbitrary = genericArbitrary
  shrink = \case
    InitTx{participants, headParameters} -> InitTx <$> shrink participants <*> shrink headParameters
    AbortTx{utxo, headSeed} -> AbortTx <$> shrink utxo <*> shrink headSeed
    CollectComTx{utxo, headId, headParameters} -> CollectComTx <$> shrink utxo <*> shrink headId <*> shrink headParameters
    IncrementTx{headId, headParameters, incrementingSnapshot, depositTxId} ->
      IncrementTx <$> shrink headId <*> shrink headParameters <*> shrink incrementingSnapshot <*> shrink depositTxId
    RecoverTx{headId, recoverTxId, deadline, recoverUTxO} ->
      RecoverTx <$> shrink headId <*> shrink recoverTxId <*> shrink deadline <*> shrink recoverUTxO
    DecrementTx{headId, headParameters, decrementingSnapshot} -> DecrementTx <$> shrink headId <*> shrink headParameters <*> shrink decrementingSnapshot
    CloseTx{headId, headParameters, openVersion, closingSnapshot} -> CloseTx <$> shrink headId <*> shrink headParameters <*> shrink openVersion <*> shrink closingSnapshot
    ContestTx{headId, headParameters, openVersion, contestingSnapshot} -> ContestTx <$> shrink headId <*> shrink headParameters <*> shrink openVersion <*> shrink contestingSnapshot
    FanoutTx{utxo, utxoToCommit, utxoToDecommit, headSeed, contestationDeadline} -> FanoutTx <$> shrink utxo <*> shrink utxoToCommit <*> shrink utxoToDecommit <*> shrink headSeed <*> shrink contestationDeadline

instance ArbitraryIsTx tx => Arbitrary (OnChainTx tx) where
  arbitrary = genericArbitrary

instance (ArbitraryIsTx tx, Arbitrary (ChainStateType tx), IsChainState tx) => Arbitrary (PostTxError tx) where
  arbitrary = genericArbitrary

instance
  ( Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  ) =>
  Arbitrary (ChainStateHistory tx)
  where
  arbitrary = genericArbitrary

instance
  ( ArbitraryIsTx tx
  , Arbitrary (ChainPointType tx)
  , Arbitrary (ChainStateType tx)
  , IsChainState tx
  ) =>
  Arbitrary (ChainEvent tx)
  where
  arbitrary = genericArbitrary
