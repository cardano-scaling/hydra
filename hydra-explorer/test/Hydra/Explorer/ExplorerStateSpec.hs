module Hydra.Explorer.ExplorerStateSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.ChainObserver (ChainObservation (..))
import Hydra.Explorer.ExplorerState (ExplorerState (..), HeadState (..), aggregateHeadObservations, initialTickState)
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
        let ExplorerState{heads} = aggregateHeadObservations observations (ExplorerState [] initialTickState)
         in heads =/= []
    prop "Given any observations, the resulting list of head ids is a prefix of the original" $
      forAll genObservations $ \observations ->
        forAll arbitrary $ \initialHeads -> do
          let resultExplorerState = aggregateHeadObservations observations (ExplorerState initialHeads initialTickState)
          getHeadIds initialHeads `isPrefixOf` getHeadIds (heads resultExplorerState)
 where
  genObservations :: Gen [ChainObservation]
  genObservations =
    arbitrary
      `suchThat` (not . null)
      `suchThat` ( not
                    . any
                      ( \case
                          HeadObservation{} -> False
                          Tick{} -> True
                      )
                 )

  getHeadIds :: [HeadState] -> [HeadId]
  getHeadIds = fmap headId
