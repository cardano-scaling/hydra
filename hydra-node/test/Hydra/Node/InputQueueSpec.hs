module Hydra.Node.InputQueueSpec where

import Hydra.Prelude

import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Hydra.Node.InputQueue (Queued (queuedId), createInputQueue, dequeue, enqueue, park, queueStats, tryEnqueue)
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (NonEmpty), Property, counterexample)
import Test.Util (isContinuous)

spec :: Spec
spec = do
  it "samples a full queue and refuses advisory admission without blocking" $ do
    let result = runSimOrThrow $ do
          q <- createInputQueue
          forM_ [1 .. 100 :: Int] $ enqueue q
          admitted <- tryEnqueue q 101
          stats <- queueStats q
          firstItem <- dequeue q
          park q firstItem
          parked <- queueStats q
          pure (admitted, stats, parked)
    result `shouldBe` (False, (100, 0, 0), (99, 1, 0))

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
