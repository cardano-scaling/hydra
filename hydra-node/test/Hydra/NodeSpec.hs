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
import Hydra.HeadLogic (
  Environment (..),
  Event (..),
  StateChanged,
  defaultTTL,
 )
import Hydra.HeadLogic qualified as HeadLogic
import Hydra.HeadLogicSpec (inInitialState, testSnapshot)
import Hydra.Ledger (ChainSlot (ChainSlot))
import Hydra.Ledger.Simple (SimpleChainState (..), SimpleTx (..), simpleLedger, utxoRef, utxoRefs)
import Hydra.Logging (Tracer, nullTracer, showLogsOnFailure, traceInTVar)
import Hydra.Logging qualified as Logging
import Hydra.Network (Network (..))
import Hydra.Network.Message (Message (..))
import Hydra.Node (
  HydraNode (..),
  HydraNodeLog (..),
  checkHeadState,
  createNodeState,
  loadState,
  stepHydraNode,
 )
import Hydra.Node.EventQueue (EventQueue (..), createEventQueue)
import Hydra.Node.ParameterMismatch (ParameterMismatch (..))
import Hydra.Options (defaultContestationPeriod)
import Hydra.Party (Party, deriveParty)
import Hydra.Persistence (PersistenceIncremental (..))
import Test.Hydra.Fixture (alice, aliceSk, bob, bobSk, carol, carolSk, deriveOnChainId, testHeadId, testHeadSeed)

spec :: Spec
spec = parallel $ do
  it "emits a single ReqSn and AckSn as leader, even after multiple ReqTxs" $
    showLogsOnFailure "NodeSpec" $ \tracer -> do
      -- NOTE(SN): Sequence of parties in OnInitTx of
      -- 'eventsToOpenHead' is relevant, so 10 is the (initial) snapshot leader
      let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
          tx2 = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [4], txOutputs = utxoRefs [5]}
          tx3 = SimpleTx{txSimpleId = 3, txInputs = utxoRefs [5], txOutputs = utxoRefs [6]}
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
                 , NetworkEvent{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx2}}
                 , NetworkEvent{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx3}}
                 ]
          signedSnapshot = sign aliceSk $ testSnapshot 1 (utxoRefs [1, 3, 4]) [1]
      node <- createHydraNode aliceSk [bob, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'
      getNetworkMessages `shouldReturn` [ReqSn 1 [1] Nothing, AckSn signedSnapshot 1]

  it "rotates snapshot leaders" $
    showLogsOnFailure "NodeSpec" $ \tracer -> do
      let tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}
          sn1 = testSnapshot 1 (utxoRefs [1, 2, 3]) mempty
          sn2 = testSnapshot 2 (utxoRefs [1, 3, 4]) [1]
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = mempty, decommitTx = Nothing}}
                 , NetworkEvent{ttl = defaultTTL, party = alice, message = AckSn (sign aliceSk sn1) 1}
                 , NetworkEvent{ttl = defaultTTL, party = carol, message = AckSn (sign carolSk sn1) 1}
                 , NetworkEvent{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
                 ]

      node <- createHydraNode bobSk [alice, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'

      getNetworkMessages `shouldReturn` [AckSn (sign bobSk sn1) 1, ReqSn 2 [1] Nothing, AckSn (sign bobSk sn2) 2]

  it "processes out-of-order AckSn" $
    showLogsOnFailure "NodeSpec" $ \tracer -> do
      let snapshot = testSnapshot 1 (utxoRefs [1, 2, 3]) []
          sigBob = sign bobSk snapshot
          sigAlice = sign aliceSk snapshot
          events =
            eventsToOpenHead
              <> [ NetworkEvent{ttl = defaultTTL, party = bob, message = AckSn{signed = sigBob, snapshotNumber = 1}}
                 , NetworkEvent{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = [], decommitTx = Nothing}}
                 ]
      node <- createHydraNode aliceSk [bob, carol] defaultContestationPeriod events
      (node', getNetworkMessages) <- recordNetwork node
      runToCompletion tracer node'
      getNetworkMessages `shouldReturn` [AckSn{signed = sigAlice, snapshotNumber = 1}]

  it "notifies client when postTx throws PostTxError" $
    showLogsOnFailure "NodeSpec" $ \tracer -> do
      let events = [ClientEvent Init]
      (node, getServerOutputs) <-
        createHydraNode aliceSk [bob, carol] defaultContestationPeriod events
          >>= throwExceptionOnPostTx NoSeedInput
          >>= recordServerOutputs

      runToCompletion tracer node

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
            events =
              eventsToOpenHead
                <> [ NetworkEvent{ttl = defaultTTL, party = bob, message = ReqTx{transaction = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}}}
                   , NetworkEvent{ttl = defaultTTL, party = bob, message = ReqTx{transaction = SimpleTx{txSimpleId = 2, txInputs = utxoRefs [2], txOutputs = utxoRefs [5]}}}
                   , NetworkEvent{ttl = defaultTTL, party = alice, message = ReqSn{snapshotNumber = 1, transactionIds = [2], decommitTx = Nothing}}
                   ]
        node <- createHydraNode bobSk [alice, carol] defaultContestationPeriod events
        (node', getNetworkMessages) <- recordNetwork node
        runToCompletion tracer node'
        getNetworkMessages `shouldReturn` [AckSn{signed = sigBob, snapshotNumber = 1}]

  it "can continue after restart via persisted state" $
    failAfter 1 $
      showLogsOnFailure "NodeSpec" $ \tracer -> do
        persistence <- createPersistenceInMemory

        createHydraNode' persistence bobSk [alice, carol] defaultContestationPeriod eventsToOpenHead
          >>= runToCompletion tracer

        let reqTxEvent = NetworkEvent{ttl = defaultTTL, party = alice, message = ReqTx{transaction = tx1}}
            tx1 = SimpleTx{txSimpleId = 1, txInputs = utxoRefs [2], txOutputs = utxoRefs [4]}

        (node, getServerOutputs) <-
          createHydraNode' persistence bobSk [alice, carol] defaultContestationPeriod [reqTxEvent]
            >>= recordServerOutputs
        runToCompletion tracer node

        getServerOutputs >>= (`shouldContain` [TxValid{headId = testHeadId, transaction = tx1}])

  describe "Configuration mismatch" $ do
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

createPersistenceInMemory :: MonadLabelledSTM m => m (PersistenceIncremental a m)
createPersistenceInMemory = do
  tvar <- newTVarIO []
  labelTVarIO tvar "persistence-in-memory"
  pure
    PersistenceIncremental
      { append = \x -> atomically $ modifyTVar tvar (<> [x])
      , loadAll = readTVarIO tvar
      }

eventsToOpenHead :: [Event SimpleTx]
eventsToOpenHead =
  [ observationEvent $ OnInitTx testHeadId testHeadSeed headParameters participants
  , observationEvent $ OnCommitTx testHeadId carol (utxoRef 3)
  , observationEvent $ OnCommitTx testHeadId bob (utxoRef 2)
  , observationEvent $ OnCommitTx testHeadId alice (utxoRef 1)
  , observationEvent $ OnCollectComTx testHeadId
  ]
 where
  observationEvent :: OnChainTx SimpleTx -> Event SimpleTx
  observationEvent observedTx =
    OnChainEvent
      { chainEvent =
          Observation
            { observedTx
            , newChainState = SimpleChainState{slot = ChainSlot 0}
            }
      }

  parties = [alice, bob, carol]
  headParameters = HeadParameters defaultContestationPeriod parties
  participants = deriveOnChainId <$> parties

runToCompletion ::
  IsChainState tx =>
  Tracer IO (HydraNodeLog tx) ->
  HydraNode tx IO ->
  IO ()
runToCompletion tracer node@HydraNode{eq = EventQueue{isEmpty}} = go
 where
  go =
    unlessM isEmpty $
      stepHydraNode tracer node >> go

createHydraNode ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m) =>
  SigningKey HydraKey ->
  [Party] ->
  ContestationPeriod ->
  [Event SimpleTx] ->
  m (HydraNode SimpleTx m)
createHydraNode =
  createHydraNode'
    PersistenceIncremental
      { append = const $ pure ()
      , loadAll = pure []
      }

createHydraNode' ::
  (MonadDelay m, MonadAsync m, MonadLabelledSTM m, MonadThrow m) =>
  PersistenceIncremental (StateChanged SimpleTx) m ->
  SigningKey HydraKey ->
  [Party] ->
  ContestationPeriod ->
  [Event SimpleTx] ->
  m (HydraNode SimpleTx m)
createHydraNode' persistence signingKey otherParties contestationPeriod events = do
  eq@EventQueue{putEvent} <- createEventQueue
  forM_ events putEvent
  (headState, _) <- loadState nullTracer persistence SimpleChainState{slot = ChainSlot 0}
  nodeState <- createNodeState headState
  pure $
    HydraNode
      { eq
      , hn = Network{broadcast = \_ -> pure ()}
      , nodeState
      , oc =
          Chain
            { postTx = \_ -> pure ()
            , draftCommitTx = \_ -> error "draftCommitTx not implemented"
            , submitTx = \_ -> error "submitTx not implemented"
            }
      , server = Server{sendOutput = \_ -> pure ()}
      , ledger = simpleLedger
      , env =
          Environment
            { party
            , signingKey
            , otherParties
            , contestationPeriod
            , participants
            }
      , persistence
      }
 where
  party = deriveParty signingKey

  -- NOTE: We use the hydra-keys as on-chain identities directly. This is fine
  -- as this is a simulated network.
  participants = deriveOnChainId <$> (party : otherParties)

recordNetwork :: HydraNode tx IO -> IO (HydraNode tx IO, IO [Message tx])
recordNetwork node = do
  (record, query) <- messageRecorder
  pure (node{hn = Network{broadcast = record}}, query)

recordPersistedItems :: HydraNode tx IO -> IO (HydraNode tx IO, IO [StateChanged tx])
recordPersistedItems node = do
  (record, query) <- messageRecorder
  pure (node{persistence = PersistenceIncremental{append = record, loadAll = pure []}}, query)

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
