module Hydra.Events.RotationSpec where

import Hydra.Prelude
import Test.Hydra.Prelude

import Data.List qualified as List
import Hydra.Chain (OnChainTx (..))
import Hydra.Chain.ChainState (ChainSlot (..))
import Hydra.Data.ContestationPeriod (addContestationPeriod)
import Hydra.Events (EventSink (..), HasEventId (..), StateEvent (..), getEvents)
import Hydra.Events.Rotation
import Hydra.HeadLogic (HeadState (..), StateChanged (..))
import Hydra.Ledger.Simple (SimpleChainState (..), simpleLedger)
import Hydra.Logging (showLogsOnFailure)
import Hydra.Node (hydrate)
import Hydra.NodeSpec (createMockSourceSink, inputsToOpenHead, notConnect, observationInput, primeWith, runToCompletion)
import Hydra.Plutus.Extras (posixFromUTCTime, posixToUTCTime)
import Hydra.Tx.ContestationPeriod (toChain)
import Test.Hydra.Node.Fixture (testEnvironment, testHeadId)
import Test.Hydra.Tx.Fixture (cperiod)
import Test.QuickCheck (Positive (..))
import Test.QuickCheck.Instances.Natural ()
import Test.QuickCheck.Monadic (monadicIO, run)

spec :: Spec
spec = parallel $ do
  describe "Node" $ do
    -- Set up a hydrate function with fixtures curried
    let setupHydrate action =
          showLogsOnFailure "NodeSpec" $ \tracer -> do
            let testHydrate = hydrate tracer testEnvironment simpleLedger SimpleChainState{slot = ChainSlot 0}
            action testHydrate
    around setupHydrate $ do
      it "rotates while running" $ \testHydrate -> do
        failAfter 1 $ do
          now <- getCurrentTime
          let initialChainState = SimpleChainState{slot = ChainSlot 0}
          let checkpointer = mkChechpointer initialChainState now
          let logId = 0
          -- XXX: this is hardcoded to ensure we get a checkpoint + a single event at the end
          let rotationConfig = RotateAfter 4
          eventStore <- createMockSourceSink
          rotatingEventStore <- newRotatedEventStore rotationConfig checkpointer logId eventStore
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion
          rotatedHistory <- getEvents (fst rotatingEventStore)
          length rotatedHistory `shouldBe` 2
      it "consistent state after restarting with rotation" $ \testHydrate -> do
        failAfter 1 $ do
          now <- getCurrentTime
          let initialChainState = SimpleChainState{slot = ChainSlot 0}
          let checkpointer = mkChechpointer initialChainState now
          let logId = 0
          -- XXX: this is hardcoded to ensure we get a single checkpoint event at the end
          let rotationConfig = RotateAfter 3
          eventStore <- createMockSourceSink
          rotatingEventStore <- newRotatedEventStore rotationConfig checkpointer logId eventStore
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion
          let contestationDeadline = addContestationPeriod (posixFromUTCTime now) (toChain cperiod)
          let closeInput = observationInput $ OnCloseTx testHeadId 0 (posixToUTCTime contestationDeadline)
          testHydrate rotatingEventStore []
            >>= notConnect
            >>= primeWith [closeInput]
            >>= runToCompletion
          [checkpoint] <- getEvents (fst rotatingEventStore)
          case stateChanged checkpoint of
            Checkpoint{state = Closed{}} -> pure ()
            _ -> fail ("unexpected: " <> show checkpoint)
    prop "a rotated and non-rotated node have consistent state" $
      monadicIO . run $
        setupHydrate $ \testHydrate -> do
          -- XXX: prepare inputs
          now <- getCurrentTime
          let contestationDeadline = addContestationPeriod (posixFromUTCTime now) (toChain cperiod)
          let closeInput = observationInput $ OnCloseTx testHeadId 0 (posixToUTCTime contestationDeadline)
          let inputs = inputsToOpenHead ++ [closeInput]
          failAfter 1 $
            do
              -- XXX: run rotated event store with prepared inputs
              let initialChainState = SimpleChainState{slot = ChainSlot 0}
              let checkpointer = mkChechpointer initialChainState now
              let logId = 0
              -- XXX: this is hardcoded to ensure we get a single checkpoint event at the end
              let rotationConfig = RotateAfter 3
              eventStore <- createMockSourceSink
              rotatingEventStore <- newRotatedEventStore rotationConfig checkpointer logId eventStore
              testHydrate rotatingEventStore []
                >>= notConnect
                >>= primeWith inputs
                >>= runToCompletion
              -- XXX: run non-rotated event store with prepared inputs
              eventStore' <- createMockSourceSink
              testHydrate eventStore' []
                >>= notConnect
                >>= primeWith inputs
                >>= runToCompletion
              -- XXX: aggregating stored events should yield consistent states
              [checkpoint] <- getEvents (fst rotatingEventStore)
              events' <- getEvents (fst eventStore')
              let checkpoint' = checkpointer events'
              checkpoint `shouldBe` checkpoint'

  describe "Rotation algorithm" $ do
    prop "rotates on startup" $
      \(Positive x, Positive delta) -> do
        eventStore@(eventSource, eventSink) <- createMockSourceSink
        let y = x + delta
        let totalEvents = toInteger y
        let events = TrivialEvent <$> [1 .. fromInteger totalEvents]
        mapM_ (putEvent eventSink) events
        unrotatedHistory <- getEvents eventSource
        toInteger (length unrotatedHistory) `shouldBe` totalEvents
        let logId = 0
        let rotationConfig = RotateAfter x
        (rotatedEventSource, _) <- newRotatedEventStore rotationConfig trivialCheckpoint logId eventStore
        rotatedHistory <- getEvents rotatedEventSource
        length rotatedHistory `shouldBe` 1

    -- given some event store (source + sink)
    -- lets configure a rotated event store that rotates after x events
    -- forall y > 0: put x*y events
    -- load all events returns a suffix of put events with length <= x
    prop "rotates after configured number of events" $
      \(Positive x, Positive y) -> do
        let rotationConfig = RotateAfter x
        mockEventStore <- createMockSourceSink
        let logId = 0
        rotatingEventStore <- newRotatedEventStore rotationConfig trivialCheckpoint logId mockEventStore
        let (eventSource, EventSink{putEvent}) = rotatingEventStore
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
        let rotationConfig = RotateAfter x
        mockEventStore <- createMockSourceSink
        let logId = 0
        rotatingEventStore <- newRotatedEventStore rotationConfig trivialCheckpoint logId mockEventStore
        let (eventSource, EventSink{putEvent}) = rotatingEventStore
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
