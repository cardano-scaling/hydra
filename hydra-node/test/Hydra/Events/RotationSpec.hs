module Hydra.Events.RotationSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Hydra.Events (HasEventId (..))
import Hydra.Events.Rotation
import Hydra.NodeSpec (createMockSourceSink)
import Test.QuickCheck.Instances.Natural ()

spec :: Spec
spec = do
  describe "Node" $ do
    it "rotates on startup" $ pendingWith "TODO"
    it "rotates while running" $ pendingWith "TODO"
    it "consistent state after restarting with rotation" $ pendingWith "TODO"
    prop "a rotated an non rotated node have consistent state" $ pendingWith "TODO"

  describe "Rotation algorithm" $ do
    let checkpoint = trivialCheckpoint

    prop "rotates after configured number of events" $ do
      -- given some event store (source + sink)
      -- lets configure a rotated event store that rotates after x events
      -- forall y > 0: put x*y events
      -- load all events returns a suffix of put events with length <= x
      pendingWith "TODO"

    prop "puts checkpoint event as first event" $ \x -> do
      mockEventStore <- createMockSourceSink
      rotatingEventStore <- newRotatedEventStore (RotateAfter x) checkpoint mockEventStore
      -- forall y. y > 0 && y < x: put x+y events (= ensures rotation)
      -- load one event === checkpoint of first x of events
      pendingWith "TODO"

checkpoint :: [e] -> e
checkpoint = undefined

newtype TrivialEvent = TrivialEvent Word64
  deriving newtype (Num)

instance HasEventId TrivialEvent where
  getEventId (TrivialEvent w) = w

trivialCheckpoint :: [TrivialEvent] -> TrivialEvent
trivialCheckpoint = sum
