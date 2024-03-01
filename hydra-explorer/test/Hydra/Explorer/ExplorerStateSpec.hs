module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.ChainObserver (HeadObservationAt (..))
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState (..), aggregateHeadObservations)
import Hydra.Explorer.ExplorerState qualified as ExplorerState
import Hydra.HeadId (HeadId)
import Hydra.OnChainId ()
import Test.QuickCheck (forAll, suchThat, (=/=))

spec :: Spec
spec = do
  describe "aggregate head observation into explorer state" $ do
    -- This ensures that the explorer always at least knows about the existence of a head.
    -- Even if we only observe a part of the life cycle of some head.
    prop "Any head observations (of some head id) must yield an entry of that head id" $
      forAll genObservations $ \observations ->
        let ExplorerState{heads} = aggregateHeadObservations observations (ExplorerState [] 0 0)
         in -- headsObserved = filter (\ExplorerState{heads} -> null heads) explorerStates
            heads =/= []
    prop "Given any observations, the resulting list of head ids is a prefix of the original" $
      forAll genObservations $ \observations ->
        forAll arbitrary $ \initialState -> do
          let resultExplorerState = aggregateHeadObservations observations (ExplorerState initialState 0 0)
          getHeadIds initialState `isPrefixOf` getHeadIds (heads resultExplorerState)
 where
  genObservations :: Gen [HeadObservationAt]
  genObservations = arbitrary `suchThat` (not . null) `suchThat` any (isJust . onChainTx)

  getHeadIds :: [HeadState] -> [HeadId]
  getHeadIds = fmap ExplorerState.headId
