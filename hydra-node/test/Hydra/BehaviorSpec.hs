{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Hydra.BehaviorSpec where

import Hydra.Prelude

import Control.Monad.Class.MonadSTM (
  modifyTVar,
  modifyTVar',
  newTQueue,
  newTVarIO,
  readTQueue,
  readTVar,
  writeTQueue,
 )
import Control.Monad.Class.MonadTimer (timeout)
import Control.Monad.IOSim (IOSim, runSimTrace, selectTraceEventsDynamic, traceM)
import Hydra.Chain (Chain (..))
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
import Hydra.Ledger (Party, SigningKey, Tx, deriveParty)
import Hydra.Ledger.Builder (aValidTx, utxoRef, utxoRefs)
import Hydra.Ledger.Simple (SimpleTx (..), simpleLedger)
import Hydra.Logging (Tracer (..))
import Hydra.Network (Network (..))
import Hydra.Node (
  HydraNode (..),
  HydraNodeLog (..),
  createEventQueue,
  createHydraHead,
  handleChainTx,
  handleClientRequest,
  handleMessage,
  runHydraNode,
 )
import Test.Hspec (Spec, describe, it, shouldContain)
import Test.Util (failAfter, shouldNotBe, shouldReturn, shouldRunInSim)

spec :: Spec
spec = describe "Behavior of one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $
    it "does not delay for real" $
      shouldRunInSim $
        threadDelay 600

  describe "Single participant Head" $ do
    it "accepts Init command" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [] NoSnapshots chain $ \n ->
          sendRequest n Init

    it "accepts Commit after successful Init" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [] NoSnapshots chain $ \n -> do
          sendRequest n Init
          sendRequest n (Commit (utxoRef 1))

    it "not accepts commits when the head is open" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [] NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 Init (ReadyToCommit [1])
          sendRequestAndWaitFor n1 (Commit (utxoRef 1)) (HeadIsOpen (utxoRef 1))
          sendRequestAndWaitFor n1 (Commit (utxoRef 2)) CommandFailed

    it "can close an open head" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [] NoSnapshots chain $ \n1 -> do
          sendRequestAndWaitFor n1 Init (ReadyToCommit [1])
          sendRequestAndWaitFor n1 (Commit (utxoRef 1)) (HeadIsOpen (utxoRef 1))
          sendRequestAndWaitFor n1 Close (HeadIsClosed testContestationPeriod (Snapshot 0 (utxoRef 1) []))

  it "does finalize head after contestation period" $
    shouldRunInSim $ do
      chain <- simulatedChainAndNetwork
      withHydraNode 1 [] NoSnapshots chain $ \n1 -> do
        sendRequest n1 Init
        sendRequestAndWaitFor n1 Init (ReadyToCommit [1])
        sendRequest n1 (Commit (utxoRef 1))
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRef 1)
        sendRequest n1 Close
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsClosed testContestationPeriod (Snapshot 0 (utxoRef 1) [])
        threadDelay testContestationPeriod
        failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsFinalized (utxoRef 1)

  describe "Two participant Head" $ do
    it "accepts a tx after the head was opened between two nodes" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] NoSnapshots chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequest n2 (Commit (utxoRef 2))
            failAfter 1 $ waitForResponse n2 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])
            sendRequest n2 (NewTx $ aValidTx 3)

    it "confirms depending transactions" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] NoSnapshots chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))
            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequest n2 (Commit (utxoRef 2))
            failAfter 1 $ do
              waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])
              waitForResponse n2 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])

            let firstTx = SimpleTx 3 (utxoRef 1) (utxoRef 3)
                secondTx = SimpleTx 4 (utxoRef 3) (utxoRef 4)

            sendRequest n2 (NewTx secondTx)
            sendRequest n1 (NewTx firstTx)
            failAfter 1 $ do
              waitForResponse n1 `shouldReturn` TxSeen firstTx
              waitForResponse n1 `shouldReturn` TxSeen secondTx

    it "sees the head closed by other nodes" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] NoSnapshots chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequestAndWaitFor n2 (Commit (utxoRef 2)) (HeadIsOpen (utxoRefs [1, 2]))

            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])
            sendRequest n1 Close

            failAfter 1 $
              waitForResponse n2
                `shouldReturn` HeadIsClosed testContestationPeriod (Snapshot 0 (utxoRefs [1, 2]) [])

    it "only opens the head after all nodes committed" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] NoSnapshots chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))
            timeout 1 (waitForResponse n1) >>= (`shouldNotBe` Just (HeadIsOpen (utxoRef 1)))

            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequestAndWaitFor n2 (Commit (utxoRef 2)) (HeadIsOpen (utxoRefs [1, 2]))

            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])

    it "valid new transactions are seen by all parties" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] NoSnapshots chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))
            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequestAndWaitFor n2 (Commit (utxoRef 2)) (HeadIsOpen (utxoRefs [1, 2]))
            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])

            sendRequest n1 (NewTx (aValidTx 42))
            failAfter 1 $ waitForResponse n1 `shouldReturn` TxSeen (aValidTx 42)
            failAfter 1 $ waitForResponse n2 `shouldReturn` TxSeen (aValidTx 42)

    it "valid new transactions get snapshotted" $
      shouldRunInSim $ do
        chain <- simulatedChainAndNetwork
        withHydraNode 1 [2] SnapshotAfterEachTx chain $ \n1 ->
          withHydraNode 2 [1] NoSnapshots chain $ \n2 -> do
            sendRequestAndWaitFor n1 Init (ReadyToCommit [1, 2])
            sendRequest n1 (Commit (utxoRef 1))
            failAfter 1 $ waitForResponse n2 `shouldReturn` ReadyToCommit [1, 2]
            sendRequestAndWaitFor n2 (Commit (utxoRef 2)) (HeadIsOpen (utxoRefs [1, 2]))
            failAfter 1 $ waitForResponse n1 `shouldReturn` HeadIsOpen (utxoRefs [1, 2])

            sendRequest n1 (NewTx (aValidTx 42))
            failAfter 1 $ waitForResponse n1 `shouldReturn` TxSeen (aValidTx 42)
            failAfter 1 $ waitForResponse n2 `shouldReturn` TxSeen (aValidTx 42)

            failAfter 1 $ waitForResponse n1 `shouldReturn` SnapshotConfirmed 1

            sendRequest n1 Close
            failAfter 1 $ do
              let expectedSnapshot =
                    Snapshot
                      { number = 1
                      , utxo = utxoRefs [42, 1, 2]
                      , confirmed = [aValidTx 42]
                      }
              waitForResponse n1
                `shouldReturn` HeadIsClosed testContestationPeriod expectedSnapshot

  describe "Hydra Node Logging" $ do
    it "traces processing of events" $ do
      let result = runSimTrace $ do
            chain <- simulatedChainAndNetwork
            withHydraNode 1 [] NoSnapshots chain $ \n1 -> do
              sendRequestAndWaitFor n1 Init (ReadyToCommit [1])
              sendRequest n1 (Commit (utxoRef 1))

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs `shouldContain` [ProcessingEvent 1 (ClientEvent Init)]
      logs `shouldContain` [ProcessedEvent 1 (ClientEvent Init)]

    it "traces handling of effects" $ do
      let result = runSimTrace $ do
            chain <- simulatedChainAndNetwork
            withHydraNode 1 [] NoSnapshots chain $ \n1 -> do
              sendRequestAndWaitFor n1 Init (ReadyToCommit [1])
              sendRequest n1 (Commit (utxoRef 1))

          logs = selectTraceEventsDynamic @_ @(HydraNodeLog SimpleTx) result

      logs `shouldContain` [ProcessingEffect 1 (ClientEffect $ ReadyToCommit [1])]
      logs `shouldContain` [ProcessedEffect 1 (ClientEffect $ ReadyToCommit [1])]

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
  { sendRequest :: ClientRequest tx -> m ()
  , waitForResponse :: m (ClientResponse tx)
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
        { oc = Chain{postTx = postTx nodes refHistory}
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
  forall s a.
  SigningKey ->
  [Party] ->
  SnapshotStrategy ->
  ConnectToChain SimpleTx (IOSim s) ->
  (TestHydraNode SimpleTx (IOSim s) -> IOSim s a) ->
  IOSim s a
withHydraNode signingKey otherParties snapshotStrategy connectToChain action = do
  response <- atomically newTQueue
  node <- createHydraNode response

  withAsync (runHydraNode traceInIOSim node) $ \_ ->
    action $
      TestHydraNode
        { sendRequest = handleClientRequest node
        , waitForResponse = atomically $ readTQueue response
        }
 where
  party = deriveParty signingKey

  createHydraNode response = do
    let env =
          Environment
            { party
            , signingKey
            , otherParties
            , snapshotStrategy
            }
    eq <- createEventQueue
    let headState = createHeadState [] (HeadParameters testContestationPeriod mempty)
    hh <- createHydraHead headState simpleLedger
    let hn' = Network{broadcast = const $ pure ()}
    let node = HydraNode{eq, hn = hn', hh, oc = Chain (const $ pure ()), sendResponse = atomically . writeTQueue response, env}
    connectToChain node

-- | A 'Tracer' that works in 'IOSim' monad.
-- This tracer uses the 'Output' event which uses converts value traced to 'Dynamic'
-- which requires 'Typeable' constraint. To retrieve the trace use 'selectTraceEventsDynamic'
-- applied to the correct type.
traceInIOSim :: Typeable a => Tracer (IOSim s) a
traceInIOSim = Tracer $ \a -> traceM a
