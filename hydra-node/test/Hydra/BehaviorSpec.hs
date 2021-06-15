{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.BehaviorSpec where

import Cardano.Prelude hiding (Async, STM, async, atomically, cancel, check, link, poll, threadDelay, withAsync)
import Control.Monad.Class.MonadAsync (MonadAsync, withAsync)
import Control.Monad.Class.MonadSTM (
  MonadSTM,
  TVar,
  atomically,
  modifyTVar,
  modifyTVar',
  newEmptyTMVarIO,
  newTVarIO,
  putTMVar,
  readTVar,
  takeTMVar,
 )
import Control.Monad.Class.MonadThrow (MonadMask, MonadThrow)
import Control.Monad.Class.MonadTimer (DiffTime, MonadTimer, threadDelay, timeout)
import Hydra.HeadLogic (
  ClientRequest (..),
  ClientResponse (..),
  Effect (ClientEffect),
  Environment (..),
  Event (ClientEvent),
  HeadParameters (..),
  Snapshot (..),
  SnapshotStrategy (..),
  createHeadState,
 )
import Hydra.Ledger (Tx)
import Hydra.Ledger.Mock (MockTx (..), mockLedger)
import Hydra.Logging (traceInTVar)
import Hydra.Network (Network (..))
import Hydra.Node (
  HydraNode (..),
  HydraNodeLog (..),
  OnChain (..),
  createEventQueue,
  createHydraHead,
  handleChainTx,
  handleClientRequest,
  handleMessage,
  runHydraNode,
 )
import Test.Hspec (Spec, describe, it)
import Test.Util (failAfter, shouldContain, shouldNotBe, shouldReturn, shouldRunInSim)

spec :: Spec
spec = describe "Behavior of one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $ do
    it "does not delay for real" $
      shouldRunInSim $
        threadDelay 600

  describe "Single participant Head" $ do
    it "accepts Init command" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n ->
          sendRequest n (Init [1])

    it "accepts Commit after successful Init" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n -> do
          sendRequest n (Init [1])
          sendRequest n (Commit [ValidTx 1])

    it "not accepts commits when the head is open" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
          sendRequestAndWaitFor n1 (Commit [ValidTx 1]) (HeadIsOpen [ValidTx 1])
          sendRequestAndWaitFor n1 (Commit [ValidTx 2]) CommandFailed

    it "can close an open head" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
          sendRequestAndWaitFor n1 (Commit [ValidTx 1]) (HeadIsOpen [ValidTx 1])
          sendRequestAndWaitFor n1 Close (HeadIsClosed testContestationPeriod (Snapshot 0 [ValidTx 1] []) [])

  it "does finalize head after contestation period" $
    shouldRunInSim $ do
      chain <- simulatedChainAndNetwork
      withHydraNode 1 NoSnapshots chain $ \n1 -> do
        sendRequest n1 $ Init [1]
        sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
        sendRequest n1 (Commit [ValidTx 1])
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen [ValidTx 1]
        sendRequest n1 Close
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsClosed testContestationPeriod (Snapshot 0 [ValidTx 1] []) []
        threadDelay testContestationPeriod
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsFinalized [ValidTx 1]

  describe "Two participant Head" $ do
    it "accepts a tx after the head was opened between two nodes" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          withHydraNode 2 NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
            sendRequest n1 (Commit [ValidTx 1])

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
            sendRequest n2 (Commit [ValidTx 2])
            failAfter 1 $ waitForResponse n2 `shouldReturn` HeadIsOpen [ValidTx 1, ValidTx 2]
            sendRequest n2 (NewTx $ ValidTx 3)

    it "sees the head closed by other nodes" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          withHydraNode 2 NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
            sendRequest n1 (Commit [ValidTx 1])

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
            sendRequestAndWaitFor n2 (Commit [ValidTx 2]) (HeadIsOpen [ValidTx 1, ValidTx 2])

            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen [ValidTx 1, ValidTx 2]
            sendRequest n1 Close

            failAfter 1 $
              waitForResponse n2
                `shouldReturn` HeadIsClosed testContestationPeriod (Snapshot 0 [ValidTx 1, ValidTx 2] []) []

    it "only opens the head after all nodes committed" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          withHydraNode 2 NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
            sendRequest n1 (Commit [ValidTx 1])
            timeout 1 (waitForResponse n1) >>= (`shouldNotBe` Just (HeadIsOpen [ValidTx 1]))

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
            sendRequestAndWaitFor n2 (Commit [ValidTx 2]) (HeadIsOpen [ValidTx 1, ValidTx 2])

            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen [ValidTx 1, ValidTx 2]

    it "valid new transactions get confirmed without snapshotting" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          withHydraNode 2 NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
            sendRequest n1 (Commit [ValidTx 1])
            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
            sendRequestAndWaitFor n2 (Commit [ValidTx 2]) (HeadIsOpen [ValidTx 1, ValidTx 2])
            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen [ValidTx 1, ValidTx 2]

            sendRequest n1 (NewTx $ ValidTx 42)
            failAfter 1 $ waitForResponse n1 `shouldReturn` TxConfirmed (ValidTx 42)
            failAfter 1 $ waitForResponse n2 `shouldReturn` TxConfirmed (ValidTx 42)

            sendRequest n1 Close
            failAfter 1 $
              waitForResponse n1
                `shouldReturn` HeadIsClosed testContestationPeriod (Snapshot 0 [ValidTx 1, ValidTx 2] []) [ValidTx 42]

    it "valid new transactions get snapshotted" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 (SnapshotAfter 1) chain $ \n1 -> do
          withHydraNode 2 NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
            sendRequest n1 (Commit [ValidTx 1])
            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit
            sendRequestAndWaitFor n2 (Commit [ValidTx 2]) (HeadIsOpen [ValidTx 1, ValidTx 2])
            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen [ValidTx 1, ValidTx 2]

            sendRequest n1 (NewTx $ ValidTx 42)
            failAfter 1 $ waitForResponse n1 `shouldReturn` TxConfirmed (ValidTx 42)
            failAfter 1 $ waitForResponse n2 `shouldReturn` TxConfirmed (ValidTx 42)

            failAfter 1 $ waitForResponse n1 `shouldReturn` SnapshotConfirmed 1

            sendRequest n1 Close
            failAfter 1 $ do
              let expectedSnapshot =
                    Snapshot
                      { number = 1
                      , utxo = [ValidTx 42, ValidTx 1, ValidTx 2]
                      , confirmed = [ValidTx 42]
                      }
              waitForResponse n1
                `shouldReturn` HeadIsClosed testContestationPeriod expectedSnapshot []

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
          sendRequest n1 (Commit [ValidTx 1])

          traces <- atomically $ readTVar (capturedLogs n1)

          traces `shouldContain` [ProcessingEvent (ClientEvent $ Init [1])]
          traces `shouldContain` [ProcessedEvent (ClientEvent $ Init [1])]

    it "traces handling of effects" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
          sendRequest n1 (Commit [ValidTx 1])

          traces <- atomically $ readTVar (capturedLogs n1)

          traces `shouldContain` [ProcessingEffect (ClientEffect ReadyToCommit)]
          traces `shouldContain` [ProcessedEffect (ClientEffect ReadyToCommit)]

sendRequestAndWaitFor ::
  ( HasCallStack
  , MonadThrow m
  , MonadTimer m
  , Tx tx
  ) =>
  TestHydraNode tx m ->
  ClientRequest tx ->
  ClientResponse tx ->
  m ()
sendRequestAndWaitFor node req expected = do
  sendRequest node req
  failAfter 1 $ waitForResponse node `shouldReturn` expected

-- | A thin layer around 'HydraNode' to be able to 'waitForResponse'.
data TestHydraNode tx m = TestHydraNode
  { nodeId :: Natural
  , sendRequest :: ClientRequest tx -> m ()
  , waitForResponse :: m (ClientResponse tx)
  , capturedLogs :: TVar m [HydraNodeLog tx]
  }

type ConnectToChain tx m = (HydraNode tx m -> m (HydraNode tx m))

-- | Creates a simulated chain and network by returning a function to "monkey
-- patch" a 'HydraNode' such that it is connected. This is necessary, to get to
-- know all nodes which use this function and simulate network and chain
-- messages being sent around.
--
-- NOTE: This implementation currently ensures that no two equal 'OnChainTx' can
-- be posted on chain assuming the construction of the real transaction is
-- referentially transparent.
simulatedChainAndNetwork :: (Tx tx, MonadSTM m) => m (ConnectToChain tx m)
simulatedChainAndNetwork = do
  refHistory <- newTVarIO []
  nodes <- newTVarIO []
  pure $ \node -> do
    atomically $ modifyTVar nodes (node :)
    pure $
      node
        { oc = OnChain{postTx = postTx nodes refHistory}
        , hn = Network{broadcast = broadcast nodes}
        }
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
testContestationPeriod = 3600

withHydraNode ::
  ( MonadAsync m
  , MonadTimer m
  , MonadMask m
  ) =>
  Natural ->
  SnapshotStrategy ->
  ConnectToChain MockTx m ->
  (TestHydraNode MockTx m -> m ()) ->
  m ()
withHydraNode nodeId snapshotStrategy connectToChain action = do
  capturedLogs <- newTVarIO []
  response <- newEmptyTMVarIO
  node <- createHydraNode response
  -- TODO(SN): trace directly into io-sim's 'Trace'
  withAsync (runHydraNode (traceInTVar capturedLogs) node) $ \_ ->
    action $
      TestHydraNode
        { sendRequest = handleClientRequest node
        , waitForResponse = atomically $ takeTMVar response
        , nodeId
        , capturedLogs
        }
 where
  createHydraNode response = do
    let env = Environment nodeId snapshotStrategy
    eq <- createEventQueue
    let headState = createHeadState [] (HeadParameters testContestationPeriod mempty)
    hh <- createHydraHead headState mockLedger
    let hn' = Network{broadcast = const $ pure ()}
    let node = HydraNode{eq, hn = hn', hh, oc = OnChain (const $ pure ()), sendResponse = atomically . putTMVar response, env}
    connectToChain node
