{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.ChainObserverSpec where

import "hydra-prelude" Hydra.Prelude
import "hydra-test-utils" Test.Hydra.Prelude
import "QuickCheck" Test.QuickCheck (counterexample, forAll, forAllBlind, property, (=/=), (===))
import "QuickCheck" Test.QuickCheck.Property (checkCoverage)
import "hspec-golden-aeson" Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
 )
import "hydra-cardano-api" Hydra.Cardano.Api (utxoFromTx)
import "hydra-node" Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO))
import "hydra-node" Hydra.Chain.Direct.State qualified as Transition
import "hydra-node" Test.Hydra.Chain.Direct.State (genChainStateWithTx)
import "hydra-node" Test.Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import "hydra-tx" Hydra.Tx.Observe (HeadObservation (..))
import "hydra-tx" Test.Hydra.Tx.Fixture (testNetworkId)

import Hydra.ChainObserver.NodeClient (ChainObservation, observeAll, observeTx)

instance Arbitrary ChainObservation where
  arbitrary = genericArbitrary
  shrink = genericShrink

spec :: Spec
spec =
  parallel $ do
    -- NOTE: Detect regressions in interface to hydra-explorer
    let settings = defaultSettings{sampleSize = 1}
    roundtripAndGoldenSpecsWithSettings settings $ Proxy @ChainObservation
    roundtripAndGoldenADTSpecsWithSettings settings $ Proxy @(MinimumSized HeadObservation)

    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllBlind genChainStateWithTx $ \(_ctx, st, additionalUTxO, tx, transition) ->
          genericCoverTable [transition] $
            counterexample (show transition) $
              let utxo = getKnownUTxO st <> utxoFromTx tx <> additionalUTxO
               in case snd $ observeTx testNetworkId utxo tx of
                    Just (Init{}) -> transition === Transition.Init
                    Just (Commit{}) -> transition === Transition.Commit
                    Just (CollectCom{}) -> transition === Transition.Collect
                    Just (Increment{}) -> transition === Transition.Increment
                    Just (Decrement{}) -> transition === Transition.Decrement
                    Just (Abort{}) -> transition === Transition.Abort
                    Just (Close{}) -> transition === Transition.Close
                    Just (Contest{}) -> transition === Transition.Contest
                    Just (Fanout{}) -> transition === Transition.Fanout
                    _ -> property False

    prop "Updates UTxO state given transaction part of Head lifecycle" $
      forAllBlind genChainStateWithTx $ \(_ctx, st, additionalUTxO, tx, _transition) ->
        let utxo = getKnownUTxO st <> additionalUTxO
         in fst (observeTx testNetworkId utxo tx) =/= utxo

    prop "Does not updates UTxO state given transactions outside of Head lifecycle" $
      forAll genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
        fst (observeAll testNetworkId utxo txs) === utxo
