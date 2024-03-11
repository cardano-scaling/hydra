{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.NodeSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Control.Concurrent.Class.MonadSTM (MonadLabelledSTM, labelTVarIO, modifyTVar, newTVarIO, readTVarIO)
import Hydra.API.ClientInput (ClientInput (..))
import Hydra.API.Server (Server (..))
import Hydra.API.ServerOutput (ServerOutput (..))
import Hydra.Cardano.Api (SigningKey)
import Hydra.Chain (Chain (..), ChainEvent (..), HeadParameters (..), IsChainState, OnChainTx (..), PostTxError (NoSeedInput))
import Hydra.ContestationPeriod (ContestationPeriod (..))
import Hydra.Crypto (HydraKey, sign)
import Hydra.Events (EventSink (..), EventSource (..))
import Hydra.HeadLogic (
  Environment (..),
  Input (..),
  defaultTTL,
 )
import Hydra.HeadLogic qualified as HeadLogic
import Hydra.HeadLogicSpec (inInitialState, testSnapshot)
import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer, showLogsOnFailure, traceInTVar)
import Hydra.Logging qualified as Logging
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (
  DryHydraNode (..),
  HydraNode (..),
  HydraNodeLog (..),
  WetHydraNode,
  checkHeadState,
  connect,
  hydrate,
  mkHydraNode,
  stepHydraNode,
 )
import Hydra.Node.InputQueue (InputQueue (..))
import Hydra.Node.ParameterMismatch (ParameterMismatch (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (PersistenceIncremental (..), eventPairFromPersistenceIncremental)
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, cperiod, deriveOnChainId, testEnvironment, testHeadId, testHeadSeed)
import Test.QuickCheck (NonEmptyList (..), listOf, oneof, property)
import Test.QuickCheck.Property (forAllBlind)

spec :: Spec
spec = parallel $ do
  let setupDryNode test =
        showLogsOnFailure "NodeSpec" $ \tracer ->
          test $ testDryHydraNode tracer

  describe "hydrate" $ do
    around setupDryNode $ do
      it "loads events from source into all sinks" $ \node -> do
        property $ \someEvents -> do
          (mockSink1, getMockSinkEvents1) <- createRecordingSink
          (mockSink2, getMockSinkEvents2) <- createRecordingSink

          void $ hydrate (mockSource someEvents) [mockSink1, mockSink2] node

          getMockSinkEvents1 `shouldReturn` someEvents
          getMockSinkEvents2 `shouldReturn` someEvents

      it "fails if one sink fails" $ \node -> do
        property $ \(NonEmpty someEvents) -> do
          let genSinks = oneof [pure mockSink, pure failingSink]
              failingSink = EventSink{putEvent = \_ -> failure "failing sink called"}
          forAllBlind (listOf genSinks) $ \sinks -> do
            hydrate (mockSource someEvents) (sinks <> [failingSink]) node
              `shouldThrow` \(_ :: HUnitFailure) -> True

  describe "stepHydraNode" $ do
    around setupDryNode $ do
      it "events are sent to all sinks" $ \dryNode -> do
        (mockSink1, getMockSinkEvents1) <- createRecordingSink
        (mockSink2, getMockSinkEvents2) <- createRecordingSink

        hydrate (mockSource []) [mockSink1, mockSink2] dryNode
          >>= notConnect
          >>= primeWith inputsToOpenHead
          >>= runToCompletion

        events <- getMockSinkEvents1
        events `shouldNotBe` []
        getMockSinkEvents2 `shouldReturn` events

      it "can continue after re-hydration" $ \dryNode ->
        failAfter 1 $ do
          persistence <- createPersistenceInMemory
          let (eventSource, eventSink) = eventPairFromPersistenceIncremental persistence

          hydrate eventSource [eventSink] dryNode
            >>= notConnect
            >>= primeWith inputsToOpenHead
            >>= runToCompletion

          let reqTx = NetworkInput{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
              tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}

          (node, getServerOutputs) <-
            hydrate eventSource [eventSink] dryNode
              >>= notConnect
              >>= primeWith [reqTx]
              >>= recordServerOutputs
          runToCompletion node

          getServerOutputs >>= (`shouldContain` [TxValid{headId = testHeadId, transaction = tx1}])

    it "emits a single ReqSn and AckSn as leader, even after multiple ReqTxs" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        -- NOTE(SN): Sequence of parties in OnInitTx of
        -- 'inputsToOpenHead' is relevant, so 10 is the (initial) snapshot leader
        let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
            tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
            tx3 = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [5], txOutputs = utxoRefs [6]}
            inputs =
              inputsToOpenHead
                <> [ NetworkInput{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
                   , NetworkInput{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx2}}
                   , NetworkInput{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx3}}
                   ]
            signedSnapshot = sign aliceSk $ testSnapshot 1 (utxoRefs [1, 3, 4]) [1]
        (node, getNetworkMessages) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node
        getNetworkMessages `shouldReturn` [ReqSn 1 [1], AckSn signedSnapshot 1]

    it "rotates snapshot leaders" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
            sn1 = testSnapshot 1 (utxoRefs [1, 2, 3]) mempty
            sn2 = testSnapshot 2 (utxoRefs [1, 3, 4]) [1]
            inputs =
              inputsToOpenHead
                <> [ NetworkInput{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = mempty}}
                   , NetworkInput{ttl = defaultTTL, party = alice, message = AckSn (sign aliceSk sn1) 1}
                   , NetworkInput{ttl = defaultTTL, party = carol, message = AckSn (sign carolSk sn1) 1}
                   , NetworkInput{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
                   ]

        (node, getNetworkMessages) <-
          testHydraNode tracer bobSk [alice, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node

        getNetworkMessages `shouldReturn` [AckSn (sign bobSk sn1) 1, ReqSn 2 [1], AckSn (sign bobSk sn2) 2]

    it "processes out-of-order AckSn" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let snapshot = testSnapshot 1 (utxoRefs [1, 2, 3]) []
            sigBob = sign bobSk snapshot
            sigAlice = sign aliceSk snapshot
            inputs =
              inputsToOpenHead
                <> [ NetworkInput{ttl = defaultTTL, party = bob, message = AckSn{signed = sigBob, snapshotNumber = 1}}
                   , NetworkInput{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = []}}
                   ]
        (node, getNetworkMessages) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= recordNetwork
        runToCompletion node
        getNetworkMessages `shouldReturn` [AckSn{signed = sigAlice, snapshotNumber = 1}]

    it "notifies client when postTx throws PostTxError" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let inputs = [ClientInput Init]
        (node, getServerOutputs) <-
          testHydraNode tracer aliceSk [bob, carol] cperiod inputs
            >>= throwExceptionOnPostTx NoSeedInput
            >>= recordServerOutputs

        runToCompletion node

        outputs <- getServerOutputs
        let isPostTxOnChainFailed = \case
              PostTxOnChainFailed{postTxError} -> postTxError == NoSeedInput
              _ -> False
        any isPostTxOnChainFailed outputs `shouldBe` True

    it "signs snapshot even if it has seen conflicting transactions" $
      failAfter 1 $
        showLogsOnFailure "NodeSpec" $ \tracer -> do
          let snapshot = testSnapshot 1 (utxoRefs [1, 3, 5]) [2]
              sigBob = sign bobSk snapshot
              inputs =
                inputsToOpenHead
                  <> [ NetworkInput{ttl = defaultTTL, party = bob, message = ReqTx{transaction = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}}}
                     , NetworkInput{ttl = defaultTTL, party = bob, message = ReqTx{transaction = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [2], txOutputs = utxoRefs [5]}}}
                     , NetworkInput{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = [2]}}
                     ]
          (node, getNetworkMessages) <-
            testHydraNode tracer bobSk [alice, carol] cperiod inputs
              >>= recordNetwork
          runToCompletion node
          getNetworkMessages `shouldReturn` [AckSn{signed = sigBob, snapshotNumber = 1}]

  describe "checkHeadState" $ do
    let defaultEnv =
          Environment
            { party = alice
            , signingKey = aliceSk
            , otherParties = [bob]
            , contestationPeriod = defaultContestationPeriod
            , participants = error "should not be recorded in head state"
            }
        headState = inInitialState [alice, bob]

    it "accepts configuration consistent with HeadState" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        checkHeadState tracer defaultEnv headState `shouldReturn` ()

    it "throws exception given contestation period differs" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let invalidPeriodEnv =
              defaultEnv{HeadLogic.contestationPeriod = UnsafeContestationPeriod 42}
        checkHeadState tracer invalidPeriodEnv headState
          `shouldThrow` \(_ :: ParameterMismatch) -> True

    it "throws exception given parties differ" $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        let invalidPeriodEnv = defaultEnv{otherParties = []}
        checkHeadState tracer invalidPeriodEnv headState
          `shouldThrow` \(_ :: ParameterMismatch) -> True

    it "log error given configuration mismatches head state" $ do
      logs <- newTVarIO []
      let invalidPeriodEnv = defaultEnv{otherParties = []}
          isContestationPeriodMismatch = \case
            Misconfiguration{} -> True
            _ -> False

      checkHeadState (traceInTVar logs "NodeSpec") invalidPeriodEnv headState
        `catch` \(_ :: ParameterMismatch) -> pure ()

      entries <- fmap Logging.message <$> readTVarIO logs
      entries `shouldSatisfy` any isContestationPeriodMismatch

-- | Add given list of inputs to the 'InputQueue'. This is returning the node to
-- allow for chaining with 'runToCompletion'.
primeWith :: Monad m => [Input SimpleTx] -> HydraNode SimpleTx m -> m (HydraNode SimpleTx m)
primeWith inputs node@HydraNode{inputQueue = InputQueue{enqueue}} = do
  forM_ inputs enqueue
  pure node

-- | Convert a 'WetHydraNode' to a 'HydraNode' by providing mock implementations.
notConnect :: MonadThrow m => WetHydraNode SimpleTx m -> m (HydraNode SimpleTx m)
notConnect =
  connect mockChain mockNetwork mockServer

mockServer :: Monad m => Server SimpleTx m
mockServer =
  Server{sendOutput = \_ -> pure ()}

mockNetwork :: Monad m => Network m (Message SimpleTx)
mockNetwork =
  Network{broadcast = \_ -> pure ()}

mockChain :: MonadThrow m => Chain SimpleTx m
mockChain =
  Chain
    { postTx = \_ -> pure ()
    , draftCommitTx = \_ _ -> failure "mockChain: unexpected draftCommitTx"
    , submitTx = \_ -> failure "mockChain: unexpected submitTx"
    }

mockSink :: Monad m => EventSink a m
mockSink = EventSink{putEvent = const $ pure ()}

mockSource :: Monad m => [a] -> EventSource a m
mockSource events = EventSource{getEvents = pure events}

createRecordingSink :: IO (EventSink a IO, IO [a])
createRecordingSink = do
  (putEvent, getAll) <- messageRecorder
  pure (EventSink{putEvent}, getAll)

createPersistenceInMemory :: MonadLabelledSTM m => m (PersistenceIncremental a m)
createPersistenceInMemory = do
  tvar <- newTVarIO []
  labelTVarIO tvar "persistence-in-memory"
  pure
    PersistenceIncremental
      { append = \x -> atomically $ modifyTVar tvar (<> [x])
      , loadAll = readTVarIO tvar
      }

isReqSn :: Message tx -> Bool
isReqSn = \case
  ReqSn{} -> True
  _ -> False

inputsToOpenHead :: [Input SimpleTx]
inputsToOpenHead =
  [ observationInput $ OnInitTx testHeadId testHeadSeed headParameters participants
  , observationInput $ OnCommitTx testHeadId carol (utxoRef 3)
  , observationInput $ OnCommitTx testHeadId bob (utxoRef 2)
  , observationInput $ OnCommitTx testHeadId alice (utxoRef 1)
  , observationInput $ OnCollectComTx testHeadId
  ]
 where
  observationInput :: OnChainTx SimpleTx -> Input SimpleTx
  observationInput observedTx =
    ChainInput
      { chainEvent =
          Observation
            { observedTx
            , newChainState = SimpleChainState{slot = ChainSlot 0}
            }
      }

  parties = [alice, bob, carol]
  headParameters = HeadParameters cperiod parties
  participants = deriveOnChainId <$> parties

runToCompletion ::
  IsChainState tx =>
  HydraNode tx IO ->
  IO ()
runToCompletion node@HydraNode{inputQueue = InputQueue{isEmpty}} = go
 where
  go =
    unlessM isEmpty $
      stepHydraNode node >> go

-- | DryHydraNode with 'testEnvironment' and a simple ledger.
testDryHydraNode ::
  Tracer m (HydraNodeLog SimpleTx) ->
  DryHydraNode SimpleTx m
testDryHydraNode tracer =
  DryHydraNode
    { tracer
    , env = testEnvironment
    , ledger = simpleLedger
    , initialChainState = SimpleChainState{slot = ChainSlot 0}
    }

-- | Createa a full 'HydraNode' with given parameters and primed 'Input's. Note
-- that this node is 'notConnect'ed to any components.
testHydraNode ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m) =>
  Tracer m (HydraNodeLog SimpleTx) ->
  SigningKey HydraKey ->
  [Party] ->
  ContestationPeriod ->
  [Input SimpleTx] ->
  m (HydraNode SimpleTx m)
testHydraNode tracer signingKey otherParties contestationPeriod inputs = do
  mkHydraNode tracer env simpleLedger SimpleChainState{slot = ChainSlot 0}
    >>= hydrate (mockSource []) []
    >>= notConnect
    >>= primeWith inputs
 where
  env =
    Environment
      { party
      , signingKey
      , otherParties
      , contestationPeriod
      , participants
      }

  party = deriveParty signingKey

  -- NOTE: We use the hydra-keys as on-chain identities directly. This is fine
  -- as this is a simulated network.
  participants = deriveOnChainId <$> (party : otherParties)

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  (record, query) <- messageRecorder
  pure (node{hn = Network{broadcast = record}}, query)

recordServerOutputs :: HydraNode tx IO -> IO (HydraNode tx IO, IO [ServerOutput tx])
recordServerOutputs node = do
  (record, query) <- messageRecorder
  pure (node{server = Server{sendOutput = record}}, query)

messageRecorder :: IO (msg -> IO (), IO [msg])
messageRecorder = do
  ref <- newIORef []
  pure (appendMsg ref, readIORef ref)
 where
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
            { postTx = \_ -> throwIO exception
            , draftCommitTx = \_ -> error "draftCommitTx not implemented"
            , submitTx = \_ -> error "submitTx not implemented"
            }
      }
