module Hydra.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO), genChainStateWithTx)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.ChainObserver (ChainObserverLog (..), observeAll, observeTx)
import Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Test.QuickCheck (counterexample, forAll, forAllBlind, property, (=/=), (===))
import Test.QuickCheck.Property (checkCoverage)

spec :: Spec
spec =
  parallel $ do
    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllBlind genChainStateWithTx $ \(_ctx, st, tx, transition) ->
          genericCoverTable [transition] $
            counterexample (show transition) $
              let utxo = getKnownUTxO st
               in case snd $ observeTx testNetworkId utxo tx of
                    Just (HeadInitTx{}) -> transition === Transition.Init
                    Just (HeadCommitTx{}) -> transition === Transition.Commit
                    Just (HeadCollectComTx{}) -> transition === Transition.Collect
                    Just (HeadDecrementTx{}) -> transition === Transition.Decrement
                    Just (HeadAbortTx{}) -> transition === Transition.Abort
                    Just (HeadCloseTx{}) -> transition === Transition.Close
                    Just (HeadContestTx{}) -> transition === Transition.Contest
                    Just (HeadFanoutTx{}) -> transition === Transition.Fanout
                    _ -> property False

    prop "Updates UTxO state given transaction part of Head lifecycle" $
      forAllBlind genChainStateWithTx $ \(_ctx, st, tx, _transition) ->
        let utxo = getKnownUTxO st
         in fst (observeTx testNetworkId utxo tx) =/= utxo

    prop "Does not updates UTxO state given transactions outside of Head lifecycle" $
      forAll genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
        fst (observeAll testNetworkId utxo txs) === utxo
