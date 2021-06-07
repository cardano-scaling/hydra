{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.BehaviorSpec where

import Cardano.Prelude hiding (Async, STM, async, atomically, cancel, check, link, poll, threadDelay)
import Control.Monad.Class.MonadAsync (MonadAsync, async, cancel, link, poll)
import Control.Monad.Class.MonadFork (MonadFork)
import Control.Monad.Class.MonadSTM (
  MonadSTM,
  TVar,
  atomically,
  check,
  modifyTVar,
  modifyTVar',
  newEmptyTMVar,
  newTVarIO,
  putTMVar,
  readTVar,
  takeTMVar,
 )
import Control.Monad.Class.MonadThrow (MonadMask)
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, threadDelay, timeout)
import Hydra.HeadLogic (
  ClientRequest (..),
  ClientResponse (..),
  Effect (ClientEffect),
  Environment (..),
  Event (ClientEvent),
  HeadParameters (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.Ledger (LedgerState)
import Hydra.Ledger.Mock (MockTx (..), mockLedger)
import Hydra.Logging (traceInTVar)
import Hydra.Network (HydraNetwork (..))
import Hydra.Node (
  HydraNode (..),
  HydraNodeLog (..),
  OnChain (..),
  createEventQueue,
  createHydraHead,
  handleChainTx,
  handleClientRequest,
  handleMessage,
  queryLedgerState,
  runHydraNode,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldContain,
  shouldNotBe,
  shouldReturn,
 )
import Test.Util (failAfter, failure)

spec :: Spec
spec = describe "Behavior of one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $ do
    it "is Ready when started" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Single participant Head" $ do
    it "accepts Init command" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequest n (Init [1]) `shouldReturn` ()

    it "accepts Commit after successful Init" $ do
      n :: HydraProcess IO MockTx <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequest n (Init [1])
      sendRequest n (Commit 1)

    it "not accepts commits when the head is open" $ do
      n1 <- simulatedChainAndNetwork >>= startHydraNode 1

      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequestAndWaitFor n1 (Commit 1) (HeadIsOpen [])
      sendRequestAndWaitFor n1 (Commit 1) CommandFailed

    it "can close an open head" $ do
      n1 <- simulatedChainAndNetwork >>= startHydraNode 1

      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequestAndWaitFor n1 (Commit 1) (HeadIsOpen [])
      sendRequestAndWaitFor n1 Close (HeadIsClosed testContestationPeriod [] 0 [])

    it "does finalize head after contestation period" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain

      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequest n1 (Commit 1)
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen []
      sendRequest n1 Close
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsClosed testContestationPeriod [] 0 []
      threadDelay testContestationPeriod
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsFinalized []

  describe "Two participant Head" $ do
    it "accepts a tx after the head was opened between two nodes" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 (Commit 1)

      failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
      sendRequest n2 (Commit 1)
      failAfter 1 $ waitForResponse n2 `shouldReturn` HeadIsOpen []
      sendRequest n2 (NewTx $ ValidTx 1)

    it "sees the head closed by other nodes" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 (Commit 1)

      failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
      sendRequestAndWaitFor n2 (Commit 1) (HeadIsOpen [])

      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen []
      sendRequest n1 Close

      failAfter 1 $ waitForResponse n2 `shouldReturn` HeadIsClosed testContestationPeriod [] 0 []

    it "only opens the head after all nodes committed" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 (Commit 1)
      timeout 1 (waitForResponse n1) >>= (`shouldNotBe` Just (HeadIsOpen []))

      failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
      sendRequestAndWaitFor n2 (Commit 1) (HeadIsOpen [])

      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen []

    it "valid new transactions get confirmed without snapshotting" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 (Commit 1)
      failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
      sendRequestAndWaitFor n2 (Commit 1) (HeadIsOpen [])
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen []

      sendRequest n1 (NewTx $ ValidTx 42)
      failAfter 1 $ waitForResponse n1 `shouldReturn` TxConfirmed (ValidTx 42)
      failAfter 1 $ waitForResponse n2 `shouldReturn` TxConfirmed (ValidTx 42)

      sendRequest n1 Close
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsClosed 3 [] 0 [ValidTx 42]

    it "valid new transactions get snapshotted" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode' (SnapshotAfter 1) 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 (Commit 1)
      failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
      sendRequestAndWaitFor n2 (Commit 1) (HeadIsOpen [])
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen []

      sendRequest n1 (NewTx $ ValidTx 42)
      failAfter 1 $ waitForResponse n1 `shouldReturn` TxConfirmed (ValidTx 42)
      failAfter 1 $ waitForResponse n2 `shouldReturn` TxConfirmed (ValidTx 42)

      sendRequest n1 Close
      failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsClosed 3 [] 0 [ValidTx 42]

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain

      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequest n1 (Commit 1)

      traces <- atomically $ readTVar (capturedLogs n1)

      traces `shouldContain` [ProcessingEvent (ClientEvent $ Init [1])]
      traces `shouldContain` [ProcessedEvent (ClientEvent $ Init [1])]

    it "traces handling of effects" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain

      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequest n1 (Commit 1)

      traces <- atomically $ readTVar (capturedLogs n1)

      traces `shouldContain` [ProcessingEffect (ClientEffect ReadyToCommit)]
      traces `shouldContain` [ProcessedEffect (ClientEffect ReadyToCommit)]

sendRequestAndWaitFor :: HasCallStack => HydraProcess IO MockTx -> ClientRequest MockTx -> ClientResponse MockTx -> IO ()
sendRequestAndWaitFor node req expected = do
  sendRequest node req
  failAfter 1 $ waitForResponse node `shouldReturn` expected

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m tx = HydraProcess
  { nodeId :: Natural
  , stopHydraNode :: m ()
  , sendRequest :: ClientRequest tx -> m ()
  , waitForResponse :: m (ClientResponse MockTx)
  , waitForLedgerState :: Maybe (LedgerState tx) -> m ()
  , queryNodeState :: m NodeState
  , capturedLogs :: TVar m [HydraNodeLog tx]
  }

data Connections m = Connections {chain :: OnChain m, network :: HydraNetwork MockTx m}

-- | Creates a simulated chain by returning a function to create the chain
-- client interface for a node. This is necessary, to get to know all nodes
-- which use this function and simulate an 'OnChainTx' happening.
--
-- NOTE: This implementation currently ensures that no two equal 'OnChainTx' can
-- be posted on chain assuming the construction of the real transaction is
-- referentially transparent.
simulatedChainAndNetwork :: MonadSTM m => m (HydraNode MockTx m -> m (Connections m))
simulatedChainAndNetwork = do
  refHistory <- newTVarIO []
  nodes <- newTVarIO []
  pure $ \n -> do
    atomically $ modifyTVar nodes (n :)
    pure $ Connections OnChain{postTx = postTx nodes refHistory} HydraNetwork{broadcast = broadcast nodes}
 where
  postTx nodes refHistory tx = do
    res <- atomically $ do
      h <- readTVar refHistory
      if tx `elem` h
        then pure Nothing
        else do
          modifyTVar' refHistory (tx :)
          Just <$> readTVar nodes
    case res of
      Nothing -> pure ()
      Just ns -> mapM_ (`handleChainTx` tx) ns

  broadcast nodes msg = atomically (readTVar nodes) >>= mapM_ (`handleMessage` msg)

-- NOTE(SN): Deliberately not configurable via 'startHydraNode'
testContestationPeriod :: DiffTime
testContestationPeriod = 10

startHydraNode ::
  (MonadAsync m, MonadTimer m, MonadFork m, MonadMask m) =>
  Natural ->
  (HydraNode MockTx m -> m (Connections m)) ->
  m (HydraProcess m MockTx)
startHydraNode = startHydraNode' NoSnapshots

startHydraNode' ::
  (MonadAsync m, MonadTimer m, MonadFork m, MonadMask m) =>
  SnapshotStrategy ->
  Natural ->
  (HydraNode MockTx m -> m (Connections m)) ->
  m (HydraProcess m MockTx)
startHydraNode' snapshotStrategy nodeId connectToChain = do
  capturedLogs <- newTVarIO []
  response <- atomically newEmptyTMVar
  node <- createHydraNode response
  -- TODO(SN): trace directly into io-sim's 'Trace'
  nodeThread <- async $ runHydraNode (traceInTVar capturedLogs) node
  link nodeThread
  pure $
    HydraProcess
      { stopHydraNode = cancel nodeThread
      , queryNodeState =
          poll nodeThread >>= \case
            Nothing -> pure Ready
            Just _ -> pure NotReady
      , sendRequest = handleClientRequest node
      , waitForResponse = atomically $ takeTMVar response
      , waitForLedgerState =
          \st -> do
            result <- timeout 1 $
              atomically $ do
                st' <- queryLedgerState node
                check (st == st')
            when (isNothing result) $ failure ("Expected ledger state of node " <> show nodeId <> " to be " <> show st)
      , nodeId
      , capturedLogs
      }
 where
  createHydraNode response = do
    let env = Environment nodeId
    eq <- createEventQueue
    let headState = createHeadState [] (HeadParameters testContestationPeriod mempty) snapshotStrategy
    hh <- createHydraHead headState mockLedger
    let hn' = HydraNetwork{broadcast = const $ pure ()}
    let node = HydraNode{eq, hn = hn', hh, oc = OnChain (const $ pure ()), sendResponse = atomically . putTMVar response, env}
    Connections oc hn <- connectToChain node
    pure node{oc, hn}
