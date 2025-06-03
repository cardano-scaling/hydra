module Hydra.Events.RotationSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.List qualified as List
import Hydra.Chain (OnChainTx (..))
import Hydra.Chain.ChainState (ChainSlot (..), IsChainState)
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Events (EventSink (..), HasEventId (..), getEvents)
import Hydra.Events.Rotation (EventStore (..), RotationConfig (..), newRotatedEventStore)
import Hydra.HeadLogic (HeadState (..), IdleState (..), StateChanged (..), aggregate)
import Hydra.HeadLogic.StateEvent (StateEvent (..), mkCheckpoint)
import Hydra.Ledger.Simple (SimpleChainState (..), simpleLedger)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Node (hydrate)
import Hydra.NodeSpec (createMockEventStore, inputsToOpenHead, notConnect, observationInput, primeWith, runToCompletion)
import Hydra.Plutus.Extras (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx.ContestationPeriod (toChain)
import Test.Hydra.Node.Fixture (testEnvironment, testHeadId)
import Test.Hydra.Tx.Fixture (cperiod)
import Test.QuickCheck (Positive (..))
import Test.QuickCheck.Instances.Natural ()

spec :: Spec
spec = parallel $ do
  describe "Log rotation" $ do
    -- Set up a hydrate function with fixtures curried
    let setupHydrate action =
          showLogsOnFailure "RotationSpec" $ \tracer -> do
            let testHydrate = hydrate tracer testEnvironment simpleLedger SimpleChainState{slot = ChainSlot 0}
            action testHydrate
    around setupHydrate $ do
      it "rotates while running" $ \testHydrate -> do
        failAfter 1 $ do
          eventStore <- createMockEventStore
          -- NOTE: this is hardcoded to ensure we get a checkpoint + a single event at the end
          let rotationConfig = RotateAfter 4
          let s0 = Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}
          rotatingEventStore <- newRotatedEventStore rotationConfig s0 mkAggregator mkCheckpoint eventStore
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion
          rotatedHistory <- getEvents (eventSource rotatingEventStore)
          length rotatedHistory `shouldBe` 2
      it "consistent state after restarting with rotation" $ \testHydrate -> do
        failAfter 1 $ do
          eventStore <- createMockEventStore
          -- NOTE: this is hardcoded to ensure we get a single checkpoint event at the end
          let rotationConfig = RotateAfter 3
          let s0 = Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}
          rotatingEventStore <- newRotatedEventStore rotationConfig s0 mkAggregator mkCheckpoint eventStore
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion
          now <- getCurrentTime
          let contestationDeadline = addContestationPeriod (posixFromUTCTime now) (toChain cperiod)
          let closeInput = observationInput $ OnCloseTx testHeadId 0 (posixToUTCTime contestationDeadline)
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith [closeInput]
            >>= runToCompletion
          [checkpoint] <- getEvents (eventSource rotatingEventStore)
          case stateChanged checkpoint of
            Checkpoint{state = Closed{}} -> pure ()
            _ -> fail ("unexpected: " <> show checkpoint)
      it "a rotated and non-rotated node have consistent state" $ \testHydrate -> do
        -- prepare inputs
        now <- getCurrentTime
        let contestationDeadline = addContestationPeriod (posixFromUTCTime now) (toChain cperiod)
        let closeInput = observationInput $ OnCloseTx testHeadId 0 (posixToUTCTime contestationDeadline)
        let inputs = inputsToOpenHead ++ [closeInput]
        failAfter 1 $
          do
            eventStore <- createMockEventStore
            -- NOTE: this is hardcoded to ensure we get a single checkpoint event at the end
            let rotationConfig = RotateAfter 3
            -- run rotated event store with prepared inputs
            let s0 = Idle IdleState{chainState = SimpleChainState{slot = ChainSlot 0}}
            rotatingEventStore <- newRotatedEventStore rotationConfig s0 mkAggregator mkCheckpoint eventStore
            testHydrate rotatingEventStore []
              >>= notConnect
              >>= primeWith inputs
              >>= runToCompletion
            -- run non-rotated event store with prepared inputs
            eventStore' <- createMockEventStore
            testHydrate eventStore' []
              >>= notConnect
              >>= primeWith inputs
              >>= runToCompletion
            -- aggregating stored events should yield consistent states
            [StateEvent{stateChanged = checkpoint}] <- getEvents (eventSource rotatingEventStore)
            events' <- getEvents (eventSource eventStore')
            let checkpoint' = foldl' mkAggregator s0 events'
            checkpoint `shouldBe` Checkpoint checkpoint'

  describe "Rotation algorithm" $ do
    prop "rotates on startup" $
      \(Positive x, Positive delta) -> do
        eventStore@EventStore{eventSource, eventSink} <- createMockEventStore
        let y = x + delta
        let totalEvents = toInteger y
        let events = TrivialEvent <$> [1 .. fromInteger totalEvents]
        mapM_ (putEvent eventSink) events
        unrotatedHistory <- getEvents eventSource
        toInteger (length unrotatedHistory) `shouldBe` totalEvents
        let rotationConfig = RotateAfter x
        let s0 = []
        let aggregator s e = e : s
        let checkpointer s _ _ = trivialCheckpoint s
        EventStore{eventSource = rotatedEventSource} <- newRotatedEventStore rotationConfig s0 aggregator checkpointer eventStore
        rotatedHistory <- getEvents rotatedEventSource
        length rotatedHistory `shouldBe` 1

    -- given some event store (source + sink)
    -- lets configure a rotated event store that rotates after x events
    -- forall y > 0: put x*y events
    -- load all events returns a suffix of put events with length <= x
    prop "rotates after configured number of events" $
      \(Positive x, Positive y) -> do
        mockEventStore <- createMockEventStore
        let rotationConfig = RotateAfter x
        let s0 = []
        let aggregator s e = e : s
        let checkpointer s _ _ = trivialCheckpoint s
        rotatingEventStore <- newRotatedEventStore rotationConfig s0 aggregator checkpointer mockEventStore
        let EventStore{eventSource, eventSink = EventSink{putEvent}} = rotatingEventStore
        let totalEvents = toInteger x * y
        let events = TrivialEvent . fromInteger <$> [1 .. totalEvents]
        forM_ events putEvent
        currentHistory <- getEvents eventSource
        let rotatedElements = fromInteger totalEvents
        let expectRotated = take rotatedElements events
        let expectRemaining = drop rotatedElements events
        let expectedCurrentHistory = trivialCheckpoint expectRotated : expectRemaining
        expectedCurrentHistory `shouldBe` currentHistory

    -- forall y. y > 0 && y < x: put x+y events (= ensures rotation)
    -- load one event === checkpoint of first x of events
    prop "puts checkpoint event as first event" $
      \(Positive y, Positive delta) -> do
        let x = y + delta
        mockEventStore <- createMockEventStore
        let rotationConfig = RotateAfter x
        let s0 = []
        let aggregator s e = e : s
        let checkpointer s _ _ = trivialCheckpoint s
        rotatingEventStore <- newRotatedEventStore rotationConfig s0 aggregator checkpointer mockEventStore
        let EventStore{eventSource, eventSink = EventSink{putEvent}} = rotatingEventStore
        let totalEvents = toInteger x + toInteger y
        let events = TrivialEvent . fromInteger <$> [1 .. totalEvents]
        forM_ events putEvent
        currentHistory <- getEvents eventSource
        let expectRotated = take (fromInteger $ toInteger x) events
        trivialCheckpoint expectRotated `shouldBe` List.head currentHistory

newtype TrivialEvent = TrivialEvent Word64
  deriving newtype (Num, Show, Eq)

instance HasEventId TrivialEvent where
  getEventId (TrivialEvent w) = w

trivialCheckpoint :: [TrivialEvent] -> TrivialEvent
trivialCheckpoint = sum

mkAggregator :: IsChainState tx => HeadState tx -> StateEvent tx -> HeadState tx
mkAggregator s StateEvent{stateChanged} = aggregate s stateChanged
