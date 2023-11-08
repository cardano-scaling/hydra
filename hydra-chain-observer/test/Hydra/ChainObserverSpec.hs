module Hydra.ChainObserverSpec where

import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO), genChainStateWithTx)
import Hydra.Chain.Direct.State qualified as Transition
import Hydra.ChainObserver (ChainObserverLog (..), observeTx)
import Hydra.Prelude
import Test.Hydra.Prelude
import Test.QuickCheck (counterexample, forAllBlind, property, (===))
import Test.QuickCheck.Property (checkCoverage)

spec :: Spec
spec =
  parallel $ do
    describe "observeTx" $ do
      prop "All valid transitions for all possible states can be observed." $
        checkCoverage $
          forAllBlind genChainStateWithTx $ \(_ctx, st, tx, transition) ->
            genericCoverTable [transition] $
              counterexample (show transition) $
                let utxo = getKnownUTxO st
                 in case observeTx testNetworkId utxo tx of
                      Just (HeadInitTx{}) -> transition === Transition.Init
                      Just (HeadCommitTx{}) -> transition === Transition.Commit
                      Just (HeadCollectComTx{}) -> transition === Transition.Collect
                      Just (HeadAbortTx{}) -> transition === Transition.Abort
                      Just (HeadCloseTx{}) -> transition === Transition.Close
                      Just (HeadContestTx{}) -> transition === Transition.Contest
                      Just (HeadFanoutTx{}) -> transition === Transition.Fanout
                      _ -> property False
