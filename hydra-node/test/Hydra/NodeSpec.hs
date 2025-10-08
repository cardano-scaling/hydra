{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.NodeSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Conduit (MonadUnliftIO, yieldMany)
import Control.Concurrent.Class.MonadSTM (modifyTVar, readTVarIO, writeTVar)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.Server (Server (..), mkTimedServerOutputFromStateEvent)
import Hydra.API.ServerOutput (ClientMessage (..), ServerOutput (..), TimedServerOutput (..))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (Chain (..), ChainEvent (..), OnChainTx (..), PostTxError (..))
import Hydra.Chain.ChainState (ChainSlot (ChainSlot), IsChainState)
import Hydra.Chain.SyncedStatus (SyncedStatus (..))
import Hydra.Events (EventSink (..), EventSource (..), getEventId)
import Hydra.Events.Rotation (EventStore (..), LogId)
import Hydra.HeadLogic (Input (..), TTL)
import Hydra.HeadLogic.Outcome (StateChanged (HeadInitialized), genStateChanged)
import Hydra.HeadLogic.StateEvent (StateEvent (..), genStateEvent)
import Hydra.HeadLogicSpec (inInitialState, receiveMessage, receiveMessageFrom, testSnapshot)
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer, showLogsOnFailure, traceInTVar)
import Hydra.Logging qualified as Logging
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..), NetworkEvent (..))
import Hydra.Node (
  DraftHydraNode,
  HydraNode (..),
  HydraNodeLog (..),
  checkHeadState,
  connect,
  hydrate,
  stepHydraNode,
 )
import Hydra.Node.Environment as Environment
import Hydra.Node.InputQueue (InputQueue (..))
import Hydra.Node.ParameterMismatch (ParameterMismatch (..))
import Hydra.Node.State (NodeState (..))
import Hydra.Options (defaultContestationPeriod, defaultDepositPeriod)
import Hydra.Tx.ContestationPeriod (ContestationPeriod (..))
import Hydra.Tx.Crypto (HydraKey, sign)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Party (Party, deriveParty)
import Test.Hydra.Node.Fixture (testEnvironment)
import Test.Hydra.Tx.Fixture (
  alice,
  aliceSk,
  bob,
  bobSk,
  carol,
  carolSk,
  cperiod,
  deriveOnChainId,
  testHeadId,
  testHeadSeed,
 )
import Test.QuickCheck (classify, counterexample, elements, forAllBlind, forAllShrink, forAllShrinkBlind, idempotentIOProperty, listOf, listOf1, resize, (==>))
import Test.QuickCheck.Gen (generate)
import Test.Util (isStrictlyMonotonic)

spec :: Spec
spec = parallel $ do
  -- Set up a hydrate function with fixtures curried
  let setupHydrate ::
        ( ( EventStore (StateEvent SimpleTx) IO ->
            [EventSink (StateEvent SimpleTx) IO] ->
            IO (DraftHydraNode SimpleTx IO)
          ) ->
          IO ()
        ) ->
        IO ()
      setupHydrate action =
        showLogsOnFailure "NodeSpec" $ \tracer -> do
          let testHydrate = hydrate tracer testEnvironment simpleLedger SimpleChainState{slot = ChainSlot 0}
          action testHydrate

  describe "hydrate" $ do
    around setupHydrate $ do
      it "loads events from source into all sinks" $ \testHydrate ->
        forAllShrink (listOf $ genStateChanged testEnvironment >>= genStateEvent) shrink $
          \someEvents -> do
            (mockSink1, getMockSinkEvents1) <- createRecordingSink
            (mockSink2, getMockSinkEvents2) <- createRecordingSink
            void $ testHydrate (mockEventStore someEvents) [mockSink1, mockSink2]

            getMockSinkEvents1 `shouldReturn` someEvents
            getMockSinkEvents2 `shouldReturn` someEvents

      it "event ids are consistent" $ \testHydrate ->
        forAllShrink (listOf $ genStateChanged testEnvironment >>= genStateEvent) shrink $
          \someEvents -> do
            (sink, getSinkEvents) <- createRecordingSink

            void $ testHydrate (mockEventStore someEvents) [sink]

            seenEvents <- getSinkEvents
            getEventId <$> seenEvents `shouldBe` getEventId <$> someEvents

      it "fails if one sink fails" $ \testHydrate ->
        forAllShrink (listOf1 $ genStateChanged testEnvironment >>= genStateEvent) shrink $
          \someEvents -> do
            let genSinks :: Gen (EventSink (StateEvent SimpleTx) IO)
                genSinks = elements [mockSink, failingSink]
                failingSink :: EventSink (StateEvent SimpleTx) IO
                failingSink =
                  EventSink
                    { putEvent = \_ -> failure "failing putEvent sink called"
                    }
            forAllBlind (listOf genSinks) $ \sinks ->
              testHydrate (mockEventStore someEvents) (sinks <> [failingSink])
                `shouldThrow` \(_ :: HUnitFailure) -> True

      it "checks head state" $ \testHydrate ->
        forAllShrink arbitrary shrink $ \env ->
          forAllShrink arbitrary shrink $ \now ->
            env
              /= testEnvironment
              ==> do
                -- XXX: This is very tied to the fact that 'HeadInitialized' results in
                -- a head state that gets checked by 'checkHeadState'
                let genEvent = do
                      StateEvent
                        <$> arbitrary
                        <*> (HeadInitialized (mkHeadParameters env) <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary)
                        <*> pure now
                forAllShrink genEvent shrink $ \incompatibleEvent ->
                  testHydrate (mockEventStore [incompatibleEvent]) []
                    `shouldThrow` \(_ :: ParameterMismatch) -> True

  describe "stepHydraNode" $ do
    around setupHydrate $ do
      it "events are sent to all sinks" $ \testHydrate -> do
        (mockSink1, getMockSinkEvents1) <- createRecordingSink
        (mockSink2, getMockSinkEvents2) <- createRecordingSink

        testHydrate (mockEventStore []) [mockSink1, mockSink2]
          >>= notConnect
          >>= primeWith inputsToOpenHead
          >>= runToCompletion

        events <- getMockSinkEvents1
        events `shouldNotBe` []
        getMockSinkEvents2 `shouldReturn` events

      it "event ids are strictly monotonic" $ \testHydrate -> do
        -- NOTE: Arbitrary inputs in open head state results more likely in
        -- multiple state change events per input (during tx processing).
        let genInputs = do
              -- Resize to reducing complexity of additional input contents
              someInput <- resize 1 arbitrary
              pure $ inputsToOpenHead <> [someInput]

        forAllShrinkBlind genInputs shrink $ \someInputs ->
          idempotentIOProperty $ do
            (sink, getSinkEvents) <- createRecordingSink
            testHydrate (mockEventStore []) [sink]
              >>= notConnect
              >>= primeWith someInputs
              >>= runToCompletion

            events <- getSinkEvents
            let eventIds = getEventId <$> events
            pure $
              isStrictlyMonotonic eventIds
                & counterexample "Not strictly monotonic"
                & counterexample ("Event ids: " <> show eventIds)
                & counterexample ("Events: " <> show events)
                & counterexample ("Inputs: " <> show someInputs)
                & classify (null eventIds) "empty list of events"

      it "can continue after re-hydration" $ \testHydrate ->
        failAfter 1 $ do
          eventStore <- createMockEventStore

          testHydrate eventStore []
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion

          let reqTx = receiveMessage ReqTx{transaction = tx1}
              tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}

          (recordingSink, getRecordedEvents) <- createRecordingSink

          (node, getServerOutputs) <-
            testHydrate eventStore [recordingSink]
              >>= notConnect
              >>= primeWith [reqTx]
              >>= recordServerOutputs
          runToCompletion node

          getServerOutputs >>= (`shouldContain` [Left TxValid{headId = testHeadId, transactionId = 1}])

          -- Ensures that event ids are correctly loaded in hydrate
          events <- getRecordedEvents
          getEventId <$> events `shouldSatisfy` isStrictlyMonotonic

    it "emits a single ReqSn as leader, even after multiple ReqTxs" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        -- NOTE(SN): Sequence of parties in OnInitTx of
        -- 'inputsToOpenHead' is relevant, so 10 is the (initial) snapshot leader
        let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
            tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
            tx3 = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [5], txOutputs = utxoRefs [6]}
            inputs =
              inputsToOpenHead
                <> [ receiveMessage ReqTx{transaction = tx1}
                   , receiveMessage ReqTx{transaction = tx2}
                   , receiveMessage ReqTx{transaction = tx3}
                   ]
        (node, getNetworkEvents) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node
        getNetworkEvents `shouldReturn` [ReqSn 0 1 [1] Nothing Nothing]

    it "rotates snapshot leaders" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
            sn1 = testSnapshot 1 0 [] (utxoRefs [1, 2, 3])
            inputs =
              inputsToOpenHead
                <> [ receiveMessage ReqSn{snapshotVersion = 0, snapshotNumber = 1, transactionIds = mempty, depositTxId = Nothing, decommitTx = Nothing}
                   , receiveMessageFrom alice $ AckSn (sign aliceSk sn1) 1
                   , receiveMessageFrom bob $ AckSn (sign bobSk sn1) 1
                   , receiveMessageFrom carol $ AckSn (sign carolSk sn1) 1
                   , receiveMessage ReqTx{transaction = tx1}
                   ]

        (node, getNetworkEvents) <-
          testHydraNode tracer bobSk [alice, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node

        getNetworkEvents `shouldReturn` [AckSn (sign bobSk sn1) 1, ReqSn 0 2 [1] Nothing Nothing]

    it "processes out-of-order AckSn" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let snapshot = testSnapshot 1 0 [] (utxoRefs [1, 2, 3])
            sigBob = sign bobSk snapshot
            sigAlice = sign aliceSk snapshot
            inputs =
              inputsToOpenHead
                <> [ receiveMessageFrom bob AckSn{signed = sigBob, snapshotNumber = 1}
                   , receiveMessage ReqSn{snapshotVersion = 0, snapshotNumber = 1, transactionIds = [], decommitTx = Nothing, depositTxId = Nothing}
                   ]
        (node, getNetworkEvents) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node
        getNetworkEvents `shouldReturn` [AckSn{signed = sigAlice, snapshotNumber = 1}]

    it "notifies client when postTx throws PostTxError" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let inputs :: [Input SimpleTx] = [ClientInput Init]
        let tx = aValidTx 1
        let expectedError = FailedToPostTx{failureReason = "unknown failure", failingTx = tx}
        (node, getServerOutputs) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= throwExceptionOnPostTx expectedError
            >>= recordServerOutputs

        runToCompletion node

        outputs <- getServerOutputs
        let isPostTxOnChainFailed :: Either (ServerOutput SimpleTx) (ClientMessage SimpleTx) -> Bool
            isPostTxOnChainFailed = \case
              Right PostTxOnChainFailed{postTxError} -> postTxError == expectedError
              _ -> False

        any isPostTxOnChainFailed outputs `shouldBe` True

    it "signs snapshot even if it has seen conflicting transactions" $
      failAfter 1 $
        showLogsOnFailure "NodeSpec" $ \tracer -> do
          let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
              tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [2], txOutputs = utxoRefs [5]}
              snapshot = testSnapshot 1 0 [tx2] (utxoRefs [1, 3, 5])
              sigBob = sign bobSk snapshot
              inputs =
                inputsToOpenHead
                  <> [ NetworkInput testTTL $ ReceivedMessage{sender = bob, msg = ReqTx{transaction = tx1}}
                     , NetworkInput testTTL $ ReceivedMessage{sender = bob, msg = ReqTx{transaction = tx2}}
                     , NetworkInput testTTL $ ReceivedMessage{sender = alice, msg = ReqSn{snapshotVersion = 0, snapshotNumber = 1, transactionIds = [2], decommitTx = Nothing, depositTxId = Nothing}}
                     ]
          (node, getNetworkEvents) <-
            testHydraNode tracer bobSk [alice, carol] cperiod inputs
              >>= recordNetwork
          runToCompletion node
          getNetworkEvents `shouldReturn` [AckSn{signed = sigBob, snapshotNumber = 1}]

  describe "checkHeadState" $ do
    let defaultEnv =
          Environment
            { party = alice
            , signingKey = aliceSk
            , otherParties = [bob]
            , contestationPeriod = defaultContestationPeriod
            , depositPeriod = defaultDepositPeriod
            , participants = error "should not be recorded in head state"
            , configuredPeers = ""
            }
        nodeState = inInitialState [alice, bob]

    it "accepts configuration consistent with HeadState" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        checkHeadState tracer defaultEnv (headState nodeState) `shouldReturn` ()

    it "throws exception given contestation period differs" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let invalidPeriodEnv =
              defaultEnv{Environment.contestationPeriod = 42}
        checkHeadState tracer invalidPeriodEnv (headState nodeState)
          `shouldThrow` \(_ :: ParameterMismatch) -> True

    it "throws exception given parties differ" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let invalidPeriodEnv = defaultEnv{otherParties = []}
        checkHeadState tracer invalidPeriodEnv (headState nodeState)
          `shouldThrow` \(_ :: ParameterMismatch) -> True

    it "log error given configuration mismatches head state" $ do
      logs <- newLabelledTVarIO "logs" []
      let invalidPeriodEnv = defaultEnv{otherParties = []}
          isContestationPeriodMismatch :: HydraNodeLog SimpleTx -> Bool
          isContestationPeriodMismatch = \case
            Misconfiguration{} -> True
            _ -> False

      checkHeadState (traceInTVar logs "NodeSpec") invalidPeriodEnv (headState nodeState)
        `catch` \(_ :: ParameterMismatch) -> pure ()

      entries <- fmap Logging.message <$> readTVarIO logs
      entries `shouldSatisfy` any isContestationPeriodMismatch

-- | Add given list of inputs to the 'InputQueue'. This is returning the node to
-- allow for chaining with 'runToCompletion'.
primeWith :: Monad m => [Input tx] -> HydraNode tx m -> m (HydraNode tx m)
primeWith inputs node@HydraNode{inputQueue = InputQueue{enqueue}} = do
  forM_ inputs enqueue
  pure node

-- | Convert a 'DraftHydraNode' to a 'HydraNode' by providing mock implementations.
notConnect :: (MonadThrow m, MonadIO m) => DraftHydraNode tx m -> m (HydraNode tx m)
notConnect =
  connect mockChain mockNetwork mockServer

mockServer :: Monad m => Server tx m
mockServer =
  Server
    { sendMessage = \_ -> pure ()
    }

mockNetwork :: Monad m => Network m (Message tx)
mockNetwork =
  Network{broadcast = \_ -> pure ()}

mockChain :: (MonadThrow m, MonadIO m) => Chain tx m
mockChain =
  Chain
    { mkChainState = error "mockChain: unexpected mkChainState"
    , chainSyncedStatus = do
        chainPoint <- liftIO . generate $ arbitrary
        pure $ SyncedStatus{point = Just chainPoint, tip = chainPoint}
    , postTx = \_ -> pure ()
    , draftCommitTx = \_ _ -> failure "mockChain: unexpected draftCommitTx"
    , draftDepositTx = \_ _ _ _ _ -> failure "mockChain: unexpected draftDepositTx"
    , submitTx = \_ -> failure "mockChain: unexpected submitTx"
    , checkNonADAAssets = \_ -> error "mockChain: unexpected checkNonADAAssets"
    }

mockSink :: Monad m => EventSink a m
mockSink = EventSink{putEvent = const $ pure ()}

mockEventStore :: forall a m. Monad m => [a] -> EventStore a m
mockEventStore events =
  EventStore{eventSource, eventSink, rotate}
 where
  eventSource = mockSource events

  eventSink :: EventSink a m
  eventSink = mockSink

  rotate :: LogId -> a -> m ()
  rotate = const . const $ pure ()

mockSource :: Monad m => [a] -> EventSource a m
mockSource events =
  EventSource
    { sourceEvents = yieldMany events
    }

createRecordingSink :: IO (EventSink a IO, IO [a])
createRecordingSink = do
  (putEvent, getAll) <- messageRecorder
  pure (EventSink{putEvent}, getAll)

createMockEventStore :: MonadLabelledSTM m => m (EventStore a m)
createMockEventStore = do
  tvar <- newLabelledTVarIO "in-memory-source-sink" []
  let source =
        EventSource
          { sourceEvents = do
              es <- lift . lift $ readTVarIO tvar
              yieldMany es
          }
      sink =
        EventSink
          { putEvent = \x ->
              atomically $ modifyTVar tvar (<> [x])
          }
      rotate _ checkpoint = atomically $ writeTVar tvar [checkpoint]
  pure (EventStore source sink rotate)

inputsToOpenHead :: [Input SimpleTx]
inputsToOpenHead =
  [ observationInput $ OnInitTx testHeadId testHeadSeed headParameters participants
  , observationInput $ OnCommitTx testHeadId carol (utxoRef 3)
  , observationInput $ OnCommitTx testHeadId bob (utxoRef 2)
  , observationInput $ OnCommitTx testHeadId alice (utxoRef 1)
  , observationInput $ OnCollectComTx testHeadId
  ]
 where
  parties = [alice, bob, carol]
  headParameters = HeadParameters cperiod parties
  participants = deriveOnChainId <$> parties

observationInput :: OnChainTx SimpleTx -> Input SimpleTx
observationInput observedTx =
  ChainInput
    { chainEvent =
        Observation
          { observedTx
          , newChainState = SimpleChainState{slot = ChainSlot 0}
          }
    }

runToCompletion ::
  IsChainState tx =>
  HydraNode tx IO ->
  IO ()
runToCompletion node@HydraNode{inputQueue = InputQueue{isEmpty}} = go
 where
  go =
    unlessM isEmpty $
      stepHydraNode node >> go

-- | Creates a full 'HydraNode' with given parameters and primed 'Input's. Note
-- that this node is 'notConnect'ed to any components.
testHydraNode ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m, MonadUnliftIO m) =>
  Tracer m (HydraNodeLog SimpleTx) ->
  SigningKey HydraKey ->
  [Party] ->
  ContestationPeriod ->
  [Input SimpleTx] ->
  m (HydraNode SimpleTx m)
testHydraNode tracer signingKey otherParties contestationPeriod inputs = do
  let eventStore :: Monad m => EventStore (StateEvent SimpleTx) m
      eventStore = mockEventStore []
  hydrate tracer env simpleLedger SimpleChainState{slot = ChainSlot 0} eventStore []
    >>= notConnect
    >>= primeWith inputs
 where
  env =
    Environment
      { party
      , signingKey
      , otherParties
      , contestationPeriod
      , depositPeriod = defaultDepositPeriod
      , participants
      , configuredPeers = ""
      }

  party = deriveParty signingKey

  -- NOTE: We use the hydra-keys as on-chain identities directly. This is fine
  -- as this is a simulated network.
  participants = deriveOnChainId <$> (party : otherParties)

-- | A lot shorter than the real world ttl which requires to be long to overcome
-- some longer chain observation delays, but would just make tests longer.
testTTL :: TTL
testTTL = 5

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  (record, query) <- messageRecorder
  pure (node{hn = Network{broadcast = record}}, query)

recordServerOutputs :: IsChainState tx => HydraNode tx IO -> IO (HydraNode tx IO, IO [Either (ServerOutput tx) (ClientMessage tx)])
recordServerOutputs node = do
  (record, query) <- messageRecorder
  let apiSink =
        EventSink
          { putEvent = \event ->
              case mkTimedServerOutputFromStateEvent event of
                Nothing -> pure ()
                Just TimedServerOutput{output} -> record $ Left output
          }
  pure
    ( node{eventSinks = apiSink : eventSinks node, server = Server{sendMessage = record . Right}}
    , query
    )

messageRecorder :: IO (msg -> IO (), IO [msg])
messageRecorder = do
  ref <- newIORef []
  pure (appendMsg ref, readIORef ref)
 where
  appendMsg :: IORef [msg] -> msg -> IO ()
  appendMsg ref x = atomicModifyIORef' ref $ \old -> (old <> [x], ())

throwExceptionOnPostTx ::
  IsChainState tx =>
  PostTxError tx ->
  HydraNode tx IO ->
  IO (HydraNode tx IO)
throwExceptionOnPostTx exception node =
  pure
    node
      { oc =
          Chain
            { mkChainState = error "mkChainState not implemented"
            , chainSyncedStatus = do
                chainPoint <- generate arbitrary
                pure $ SyncedStatus{point = Just chainPoint, tip = chainPoint}
            , postTx = \_ -> throwIO exception
            , draftCommitTx = \_ -> error "draftCommitTx not implemented"
            , draftDepositTx = \_ -> error "draftDepositTx not implemented"
            , submitTx = \_ -> error "submitTx not implemented"
            , checkNonADAAssets = \_ -> error "checkNonADAAssets not implemented"
            }
      }
