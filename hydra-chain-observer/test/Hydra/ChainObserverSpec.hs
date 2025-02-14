module Hydra.ChainObserverSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Cardano.Api (Tx)
import Hydra.Chain (OnChainTx)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO), genChainStateWithTx)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.Chain.Direct.Tx (HeadObservation (..))
import Hydra.ChainObserver (ChainObservation, observeAll, observeTx)
import Hydra.Ledger.Cardano (genSequenceOfSimplePaymentTransactions)
import Test.Aeson.GenericSpecs (
  Settings (..),
  defaultSettings,
  roundtripAndGoldenADTSpecsWithSettings,
  roundtripAndGoldenSpecsWithSettings,
 )
import Test.Hydra.Tx.Fixture (testNetworkId)
import Test.QuickCheck (counterexample, forAll, forAllBlind, property, (=/=), (===))
import Test.QuickCheck.Property (checkCoverage)

spec :: Spec
spec =
  parallel $ do
    -- NOTE: Detect regressions in interface to hydra-explorer
    let settings = defaultSettings{sampleSize = 1}
    roundtripAndGoldenSpecsWithSettings settings $ Proxy @ChainObservation
    roundtripAndGoldenADTSpecsWithSettings settings $ Proxy @(OnChainTx Tx)

    prop "All valid transitions for all possible states can be observed." $
      checkCoverage $
        forAllBlind genChainStateWithTx $ \(_ctx, st, tx, transition) ->
          genericCoverTable [transition] $
            counterexample (show transition) $
              let utxo = getKnownUTxO st
               in case snd $ observeTx testNetworkId utxo tx of
                    Just (Init{}) -> transition === Transition.Init
                    Just (Commit{}) -> transition === Transition.Commit
                    Just (CollectCom{}) -> transition === Transition.Collect
                    Just (Decrement{}) -> transition === Transition.Decrement
                    Just (Abort{}) -> transition === Transition.Abort
                    Just (Close{}) -> transition === Transition.Close
                    Just (Contest{}) -> transition === Transition.Contest
                    Just (Fanout{}) -> transition === Transition.Fanout
                    _ -> property False

    prop "Updates UTxO state given transaction part of Head lifecycle" $
      forAllBlind genChainStateWithTx $ \(_ctx, st, tx, _transition) ->
        let utxo = getKnownUTxO st
         in fst (observeTx testNetworkId utxo tx) =/= utxo

    prop "Does not updates UTxO state given transactions outside of Head lifecycle" $
      forAll genSequenceOfSimplePaymentTransactions $ \(utxo, txs) ->
        fst (observeAll testNetworkId utxo txs) === utxo
