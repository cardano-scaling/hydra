{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.BehaviorSpec where

import Hydra.Prelude
import Test.Hydra.Prelude hiding (shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)

import Control.Monad.Class.MonadAsync (forConcurrently_)
import Control.Monad.Class.MonadSTM (
  modifyTVar,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVarIO,
  writeTQueue,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Control.Monad.IOSim (Failure (FailureDeadlock), IOSim, runSimTrace, selectTraceEventsDynamic)
import GHC.Records (getField)
import Hydra.API.Server (Server (..))
import Hydra.Chain (Chain (..), HeadParameters (..), OnChainTx (..), PostChainTx (..))
import Hydra.ClientInput
import Hydra.HeadLogic (
  Effect (ClientEffect),
  Environment (..),
  Event (ClientEvent),
  HeadState (ReadyState),
 )
import Hydra.Ledger (IsTx, ValidationError (ValidationError))
import Hydra.Ledger.Simple (SimpleTx (..), aValidTx, simpleLedger, utxoRef, utxoRefs)
import Hydra.Network (Network (..))
import Hydra.Node (
  HydraNode (..),
  HydraNodeLog (..),
  createEventQueue,
  createHydraHead,
  handleChainTx,
  handleClientInput,
  handleMessage,
  runHydraNode,
 )
import Hydra.Party (Party, SigningKey, deriveParty)
import Hydra.ServerOutput (ServerOutput (..))
import Hydra.Snapshot (Snapshot (..), getSnapshot)
import Test.Aeson.GenericSpecs (roundtripAndGoldenSpecs)
import Test.Util (shouldBe, shouldNotBe, shouldReturn, shouldRunInSim, shouldSatisfy, traceInIOSim)

spec :: Spec
spec = parallel $ do
  describe "Behavior of one ore more hydra nodes" $ do
    describe "Sanity tests of test suite" $ do
      it "does not delay for real" $
        failAfter 1 $ shouldRunInSim $ threadDelay 600

      it "does detect when no responses are sent" $ do
        let action = shouldRunInSim $ do
              chain <- simulatedChainAndNetwork
              withHydraNode 1 [] chain $ \n ->
                waitForNext n >> failure "unexpected output"
        action `shouldThrow` \case
          FailureDeadlock _ -> True
          _ -> False

    describe "Single participant Head" $ do
      it "accepts Init command" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [] chain $ \n ->
            send n (Init testContestationPeriod)

      it "accepts Commit after successful Init" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [] chain $ \n1 -> do
            send n1 (Init testContestationPeriod)
            waitFor [n1] $ ReadyToCommit (fromList [1])
            send n1 (Commit (utxoRef 1))
            waitFor [n1] $ Committed 1 (utxoRef 1)

      it "not accepts commits when the head is open" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [] chain $ \n1 -> do
            send n1 (Init testContestationPeriod)
            waitFor [n1] $ ReadyToCommit (fromList [1])
            send n1 (Commit (utxoRef 1))
            waitFor [n1] $ Committed 1 (utxoRef 1)
            waitFor [n1] $ HeadIsOpen (utxoRef 1)
            send n1 (Commit (utxoRef 2))
            waitFor [n1] CommandFailed

      it "can close an open head" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [] chain $ \n1 -> do
            send n1 (Init testContestationPeriod)
            waitFor [n1] $ ReadyToCommit (fromList [1])
            send n1 (Commit (utxoRef 1))
            waitFor [n1] $ Committed 1 (utxoRef 1)
            waitFor [n1] $ HeadIsOpen (utxoRef 1)
            send n1 Close
            waitForNext n1 >>= assertHeadIsClosed

      it "does finalize head after contestation period" $
        failAfter 5 $
          shouldRunInSim $ do
            chain <- simulatedChainAndNetwork
            withHydraNode 1 [] chain $ \n1 -> do
              send n1 (Init testContestationPeriod)
              waitFor [n1] $ ReadyToCommit (fromList [1])
              send n1 (Commit (utxoRef 1))
              waitFor [n1] $ Committed 1 (utxoRef 1)
              waitFor [n1] $ HeadIsOpen (utxoRef 1)
              send n1 Close
              waitForNext n1 >>= assertHeadIsClosed
              threadDelay testContestationPeriod
              waitFor [n1] $ HeadIsFinalized (utxoRef 1)

    describe "Two participant Head" $ do
      it "only opens the head after all nodes committed" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              send n1 (Init testContestationPeriod)
              waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])

              send n1 (Commit (utxoRef 1))
              waitFor [n1] $ Committed 1 (utxoRef 1)
              let veryLong = timeout 1000
              veryLong (waitForNext n1) >>= (`shouldNotBe` Just (HeadIsOpen (utxoRef 1)))

              send n2 (Commit (utxoRef 2))
              waitFor [n1] $ Committed 2 (utxoRef 2)
              waitFor [n1] $ HeadIsOpen (utxoRefs [1, 2])

      it "can abort and re-open a head when one party has not committed" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              send n1 (Init testContestationPeriod)
              waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])
              send n1 (Commit (utxoRefs [1, 2]))
              waitFor [n1, n2] $ Committed 1 (utxoRefs [1, 2])
              send n2 Abort
              waitFor [n1, n2] $ HeadIsAborted (utxoRefs [1, 2])
              send n1 (Init testContestationPeriod)
              waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])

      it "cannot abort head when commits have been collected" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              send n1 (Init testContestationPeriod)
              waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])
              send n1 (Commit (utxoRef 1))
              send n2 (Commit (utxoRef 2))

              waitUntil [n1, n2] $ HeadIsOpen (utxoRefs [1, 2])

              send n1 Abort
              waitFor [n1] CommandFailed

      it "cannot commit twice" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              send n1 (Init testContestationPeriod)
              waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])

              send n1 (Commit (utxoRef 1))
              waitFor [n1] $ Committed 1 (utxoRef 1)
              send n1 (Commit (utxoRef 11))
              waitFor [n1] CommandFailed

              send n2 (Commit (utxoRef 2))
              waitFor [n1] $ Committed 2 (utxoRef 2)
              waitFor [n1] $ HeadIsOpen (utxoRefs [1, 2])

              send n1 (Commit (utxoRef 11))
              waitFor [n1] CommandFailed

      it "outputs committed utxo when client requests it" $
        shouldRunInSim $
          do
            chain <- simulatedChainAndNetwork
            withHydraNode 1 [2] chain $ \n1 ->
              withHydraNode 2 [1] chain $ \n2 -> do
                send n1 (Init testContestationPeriod)
                waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])
                send n1 (Commit (utxoRef 1))

                waitFor [n2] $ Committed 1 (utxoRef 1)
                send n2 GetUtxo

                waitFor [n2] $ Utxo (utxoRefs [1])

    describe "in an open head" $ do
      let openHead n1 n2 = do
            send n1 (Init testContestationPeriod)
            waitFor [n1, n2] $ ReadyToCommit (fromList [1, 2])
            send n1 (Commit (utxoRef 1))
            waitFor [n1, n2] $ Committed 1 (utxoRef 1)
            send n2 (Commit (utxoRef 2))
            waitFor [n1, n2] $ Committed 2 (utxoRef 2)
            waitFor [n1, n2] $ HeadIsOpen (utxoRefs [1, 2])

      it "sees the head closed by other nodes" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2

              send n1 Close
              waitForNext n2
                >>= assertHeadIsClosedWith (Snapshot 0 (utxoRefs [1, 2]) [])

      it "valid new transactions are seen by all parties" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              waitFor [n1] $ TxValid (aValidTx 42)
              waitFor [n1, n2] $ TxSeen (aValidTx 42)

      it "valid new transactions get snapshotted" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              waitFor [n1] $ TxValid (aValidTx 42)
              waitFor [n1, n2] $ TxSeen (aValidTx 42)

              waitFor [n1] $ SnapshotConfirmed (Snapshot 1 (utxoRefs [1, 2, 42]) [aValidTx 42])

              send n1 Close
              let expectedSnapshot =
                    Snapshot
                      { number = 1
                      , utxo = utxoRefs [42, 1, 2]
                      , confirmed = [aValidTx 42]
                      }
              waitForNext n1 >>= assertHeadIsClosedWith expectedSnapshot

      it "reports transactions as seen only when they validate (against the confirmed ledger)" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2

              let firstTx = SimpleTx 3 (utxoRef 1) (utxoRef 3)
                  secondTx = SimpleTx 4 (utxoRef 3) (utxoRef 4)

              send n2 (NewTx secondTx)
              waitFor [n2] $ TxInvalid (utxoRefs [1, 2]) secondTx (ValidationError "cannot apply transaction")
              send n1 (NewTx firstTx)
              waitFor [n1] $ TxValid firstTx

              waitFor [n1, n2] $ TxSeen firstTx
              waitFor [n1, n2] $ SnapshotConfirmed (Snapshot 1 (utxoRefs [2, 3]) [firstTx])

              send n2 (NewTx secondTx)
              waitFor [n2] $ TxValid secondTx
              waitFor [n1, n2] $ TxSeen secondTx

      it "multiple transactions get snapshotted" $ do
        pendingWith "This test is not longer true after recent changes which simplify the snapshot construction."
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2

              send n1 (NewTx (aValidTx 42))
              send n1 (NewTx (aValidTx 43))

              waitFor [n1] $ TxValid (aValidTx 42)
              waitFor [n1] $ TxValid (aValidTx 43)

              waitFor [n1] $ TxSeen (aValidTx 42)
              waitFor [n1] $ TxSeen (aValidTx 43)

              waitFor [n1] $ SnapshotConfirmed (Snapshot 1 (utxoRefs [1, 2, 42, 43]) [aValidTx 42, aValidTx 43])

      it "outputs utxo from confirmed snapshot when client requests it" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2
              let newTx = (aValidTx 42){txInputs = utxoRefs [1]}
              send n1 (NewTx newTx)
              waitUntil [n1, n2] $ SnapshotConfirmed (Snapshot 1 (utxoRefs [2, 42]) [newTx])

              send n1 GetUtxo

              waitFor [n1] $ Utxo (utxoRefs [2, 42])

      it "leader post fanout tx after contestation period" $
        shouldRunInSim $ do
          chain <- simulatedChainAndNetwork
          withHydraNode 1 [2] chain $ \n1 ->
            withHydraNode 2 [1] chain $ \n2 -> do
              openHead n1 n2
              send n1 Close
              forM_ [n1, n2] $ waitForNext >=> assertHeadIsClosed
              threadDelay testContestationPeriod
              waitFor [n1, n2] $ HeadIsFinalized (utxoRefs [1, 2])
              allTxs <- history chain
              length (filter matchFanout allTxs) `shouldBe` 1

    describe "Hydra Node Logging" $ do
      it "traces processing of events" $ do
        let result = runSimTrace $ do
              chain <- simulatedChainAndNetwork
              withHydraNode 1 [] chain $ \n1 -> do
                send n1 (Init testContestationPeriod)
                waitFor [n1] $ ReadyToCommit (fromList [1])
                send n1 (Commit (utxoRef 1))

            logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

        logs
          `shouldContain` [ProcessingEvent 1 $ ClientEvent $ Init testContestationPeriod]
        logs
          `shouldContain` [ProcessedEvent 1 $ ClientEvent $ Init testContestationPeriod]

      it "traces handling of effects" $ do
        let result = runSimTrace $ do
              chain <- simulatedChainAndNetwork
              withHydraNode 1 [] chain $ \n1 -> do
                send n1 (Init testContestationPeriod)
                waitFor [n1] $ ReadyToCommit (fromList [1])
                send n1 (Commit (utxoRef 1))

            logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

        logs `shouldContain` [ProcessingEffect 1 (ClientEffect $ ReadyToCommit $ fromList [1])]
        logs `shouldContain` [ProcessedEffect 1 (ClientEffect $ ReadyToCommit $ fromList [1])]

      roundtripAndGoldenSpecs (Proxy @(HydraNodeLog SimpleTx))

waitFor ::
  (HasCallStack, MonadThrow m, IsTx tx, MonadAsync m, MonadTimer m) =>
  [TestHydraNode tx m] ->
  ServerOutput tx ->
  m ()
waitFor nodes expected =
  failAfter 1 $
    forConcurrently_ nodes $ \n ->
      waitForNext n `shouldReturn` expected

-- | Wait for some output at some node(s) to be produced /eventually/.
-- The difference with 'waitFor' is that there can be other messages in between which
-- are simply discarded.
waitUntil ::
  (HasCallStack, MonadThrow m, IsTx tx, MonadAsync m, MonadTimer m) =>
  [TestHydraNode tx m] ->
  ServerOutput tx ->
  m ()
waitUntil nodes expected =
  failAfter 1 $ forConcurrently_ nodes go
 where
  go n = do
    next <- waitForNext n
    unless (next == expected) $ go n

-- | A thin layer around 'HydraNode' to be able to 'waitFor'.
data TestHydraNode tx m = TestHydraNode
  { send :: ClientInput tx -> m ()
  , waitForNext :: m (ServerOutput tx)
  }

data ConnectToChain tx m = ConnectToChain
  { chainComponent :: HydraNode tx m -> m (HydraNode tx m)
  , history :: m [PostChainTx tx]
  }

-- | Creates a simulated chain and network by returning a function to "monkey
-- patch" a 'HydraNode' such that it is connected. This is necessary, to get to
-- know all nodes which use this function and simulate network and chain
-- messages being sent around.
simulatedChainAndNetwork :: (MonadSTM m, MonadTime m) => m (ConnectToChain tx m)
simulatedChainAndNetwork = do
  refHistory <- newTVarIO []
  nodes <- newTVarIO []
  pure $
    ConnectToChain
      { chainComponent = \node -> do
          atomically $ modifyTVar nodes (node :)
          pure $
            node
              { oc = Chain{postTx = postTx nodes refHistory}
              , hn = Network{broadcast = broadcast node nodes}
              }
      , history = do
          reverse <$> readTVarIO refHistory
      }
 where
  postTx nodes refHistory tx = do
    res <- atomically $ do
      modifyTVar' refHistory (tx :)
      Just <$> readTVar nodes
    case res of
      Nothing -> pure ()
      Just ns -> do
        time <- getCurrentTime
        mapM_ (`handleChainTx` toOnChainTx time tx) ns

  broadcast node nodes msg = do
    allNodes <- readTVarIO nodes
    let otherNodes = filter (\n -> getNodeId n /= getNodeId node) allNodes
    mapM_ (`handleMessage` msg) otherNodes

  getNodeId = getField @"party" . env

-- | Derive an 'OnChainTx' from 'PostChainTx' to simulate a "perfect" chain.
-- NOTE(SN): This implementation does *NOT* honor the 'HeadParameters' and
-- announces hard-coded contestationDeadlines.
toOnChainTx :: UTCTime -> PostChainTx tx -> OnChainTx tx
toOnChainTx currentTime = \case
  InitTx HeadParameters{contestationPeriod, parties} -> OnInitTx{contestationPeriod, parties}
  (CommitTx pa ut) -> OnCommitTx pa ut
  AbortTx{} -> OnAbortTx
  CollectComTx{} -> OnCollectComTx
  (CloseTx confirmedSnapshot) ->
    OnCloseTx
      { contestationDeadline = addUTCTime 10 currentTime
      , snapshotNumber = number (getSnapshot confirmedSnapshot)
      }
  ContestTx{} -> OnContestTx
  FanoutTx{} -> OnFanoutTx

-- NOTE(SN): Deliberately long to emphasize that we run these tests in IOSim.
testContestationPeriod :: DiffTime
testContestationPeriod = 3600

withHydraNode ::
  forall s a.
  SigningKey ->
  [Party] ->
  ConnectToChain SimpleTx (IOSim s) ->
  (TestHydraNode SimpleTx (IOSim s) -> IOSim s a) ->
  IOSim s a
withHydraNode signingKey otherParties connectToChain action = do
  outputs <- atomically newTQueue
  node <- createHydraNode outputs

  withAsync (runHydraNode traceInIOSim node) $ \_ ->
    action $
      TestHydraNode
        { send = handleClientInput node
        , waitForNext = atomically $ readTQueue outputs
        }
 where
  party = deriveParty signingKey

  createHydraNode outputs = do
    eq <- createEventQueue
    hh <- createHydraHead ReadyState simpleLedger
    chainComponent connectToChain $
      HydraNode
        { eq
        , hn = Network{broadcast = const $ pure ()}
        , hh
        , oc = Chain (const $ pure ())
        , server = Server{sendOutput = atomically . writeTQueue outputs}
        , env =
            Environment
              { party
              , signingKey
              , otherParties
              }
        }

matchFanout :: PostChainTx tx -> Bool
matchFanout = \case
  FanoutTx{} -> True
  _ -> False

assertHeadIsClosed :: (HasCallStack, MonadThrow m, MonadTime m) => ServerOutput tx -> m ()
assertHeadIsClosed = \case
  HeadIsClosed{contestationDeadline} -> do
    getCurrentTime >>= \t -> contestationDeadline `shouldSatisfy` (> t)
  _ -> failure "expected HeadIsClosed"

assertHeadIsClosedWith :: (HasCallStack, MonadThrow m, MonadTime m, IsTx tx) => Snapshot tx -> ServerOutput tx -> m ()
assertHeadIsClosedWith expectedSnapshot = \case
  HeadIsClosed{contestationDeadline, latestSnapshot} -> do
    getCurrentTime >>= \t -> contestationDeadline `shouldSatisfy` (> t)
    latestSnapshot `shouldBe` expectedSnapshot
  _ -> failure "expected HeadIsClosed"
