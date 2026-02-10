module Hydra.Node.InputQueueSpec where

import "hydra-prelude" Hydra.Prelude
import "QuickCheck" Test.QuickCheck (NonEmptyList (NonEmpty), Property, counterexample)
import "hspec" Test.Hspec (Spec)
import "hspec" Test.Hspec.QuickCheck (prop)
import "io-sim" Control.Monad.IOSim (IOSim, runSimOrThrow)

import Hydra.Node.InputQueue (Queued (queuedId), createInputQueue, dequeue, enqueue)
import Test.Util (isContinuous)

spec :: Spec
spec =
  prop "adds sequential id to all enqueued items" prop_identify_enqueued_items

newtype DummyInput = DummyInput Int
  deriving newtype (Eq, Show)

prop_identify_enqueued_items :: NonEmptyList Int -> Property
prop_identify_enqueued_items (NonEmpty inputs) =
  let test :: IOSim s [Word64]
      test = do
        q <- createInputQueue
        forM inputs $ \i -> do
          enqueue q i
          queuedId <$> dequeue q
      ids = runSimOrThrow test
   in isContinuous ids
        & counterexample ("queued ids: " <> show ids)
