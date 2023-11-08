module Hydra.ChainObserverSpec where

import Test.Hydra.Prelude
import Test.QuickCheck (counterexample, forAllBlind, property, (===))
import Hydra.Prelude
import Hydra.ChainObserver (ChainObserverLog(..), observeTx)
import Test.QuickCheck.Property (checkCoverage)
import Hydra.Chain.Direct.State (HasKnownUTxO (getKnownUTxO), genChainStateWithTx)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.State qualified as Transition

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
                 in case (observeTx testNetworkId utxo tx) of
                      Just (HeadInitTx{}) -> transition === Transition.Init
                      _ -> property False
