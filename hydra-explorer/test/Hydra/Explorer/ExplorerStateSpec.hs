module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.ChainObserver (HeadObservationAt)
import Hydra.Explorer.ExplorerState (ExplorerState, aggregateHeadObservations)
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
        aggregateHeadObservations observations [] =/= []
    prop "Given any observations, the resulting list of head ids is a prefix of the original" $
      forAll genObservations $ \observations ->
        forAll arbitrary $ \initialState -> do
          let resultHeads = aggregateHeadObservations observations initialState
          getHeadIds initialState `isPrefixOf` getHeadIds resultHeads
 where
  genObservations :: Gen [HeadObservationAt]
  genObservations = arbitrary `suchThat` (not . null)

  getHeadIds :: ExplorerState -> [HeadId]
  getHeadIds = fmap ExplorerState.headId
