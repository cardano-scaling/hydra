{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hydra.Node.EventQueueSpec where

import Hydra.Prelude

import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Hydra.Node (Queued (eventId), createEventQueue, nextEvent, putEvent)
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList (NonEmpty), Property, counterexample)

spec :: Spec
spec =
  prop "adds sequential id to all events enqueued" prop_identify_enqueued_events

newtype DummyEvent = DummyEvent Int
  deriving newtype (Eq, Show, Arbitrary)

prop_identify_enqueued_events :: NonEmptyList DummyEvent -> Property
prop_identify_enqueued_events (NonEmpty events) =
  let test :: IOSim s [Word64]
      test = do
        q <- createEventQueue
        forM events $ \e -> do
          putEvent q e
          eventId <$> nextEvent q
      eventIds = runSimOrThrow test
   in eventIds == [0 .. fromIntegral (length events - 1)]
        & counterexample ("queued ids: " <> show eventIds)
