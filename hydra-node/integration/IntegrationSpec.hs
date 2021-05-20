{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude hiding (atomically, check)
import Control.Concurrent.STM (modifyTVar, newTVarIO, readTVarIO)
import Control.Monad.Class.MonadSTM (atomically, check)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hydra.Ledger (Ledger (..), LedgerState, ValidationError (..), ValidationResult (Invalid, Valid))
import Hydra.Logic (ClientRequest (..), ClientResponse (..), Party (..))
import Hydra.Network (HydraNetwork (..))
import Hydra.Node (
  HydraNode (..),
  OnChain (..),
  createHydraNode,
  handleChainTx,
  handleClientRequest,
  handleMessage,
  queryLedgerState,
  runHydraNode,
 )
import System.Timeout (timeout)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  shouldNotBe,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integrating one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $ do
    it "is Ready when started" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Hydra node integration" $ do
    it "accepts Init command" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequest n (Init [1]) `shouldReturn` ()

    it "accepts Commit after successful Init" $ do
      n <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequest n (Init [1])
      sendRequest n Commit

    it "accepts a tx after the head was opened between two nodes" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 Commit

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      wait1sForResponse n2 `shouldReturn` Just HeadIsOpen
      sendRequest n2 (NewTx $ ValidTx 1)

    it "not accepts commits when the head is open" $ do
      n1 <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequestAndWaitFor n1 Commit HeadIsOpen
      sendRequestAndWaitFor n1 Commit CommandFailed

    it "can close an open head" $ do
      n1 <- simulatedChainAndNetwork >>= startHydraNode 1
      sendRequestAndWaitFor n1 (Init [1]) ReadyToCommit
      sendRequestAndWaitFor n1 Commit HeadIsOpen
      sendRequestAndWaitFor n1 Close HeadIsClosed

    it "sees the head closed by other nodes" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 Commit

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequestAndWaitFor n2 Commit HeadIsOpen

      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Close

      wait1sForResponse n2 `shouldReturn` Just HeadIsClosed

    it "only opens the head after all nodes committed" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n1 >>= (`shouldNotBe` Just HeadIsOpen)

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequestAndWaitFor n2 Commit HeadIsOpen

      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen

    it "valid new transaction in open head is stored in ledger" $ do
      chain <- simulatedChainAndNetwork
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequestAndWaitFor n2 Commit HeadIsOpen
      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen

      sendRequest n1 (NewTx $ ValidTx 1)

      waitForLedgerState n1 (Just [ValidTx 1])
      waitForLedgerState n2 (Just [ValidTx 1])

sendRequestAndWaitFor :: HydraProcess IO MockTx -> ClientRequest MockTx -> ClientResponse -> IO ()
sendRequestAndWaitFor node req expected =
  sendRequest node req >> (wait1sForResponse node `shouldReturn` Just expected)

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m tx = HydraProcess
  { nodeId :: Natural
  , stopHydraNode :: m ()
  , sendRequest :: ClientRequest tx -> m ()
  , wait1sForResponse :: m (Maybe ClientResponse)
  , waitForLedgerState :: Maybe (LedgerState tx) -> m ()
  , queryNodeState :: m NodeState
  }

data Connections = Connections {chain :: OnChain IO, network :: HydraNetwork MockTx IO}

-- | Creates a simulated chain by returning a function to create the chain
-- client interface for a node. This is necessary, to get to know all nodes
-- which use this function and simulate an 'OnChainTx' happening.
--
-- NOTE: This implementation currently ensures that no two equal 'OnChainTx' can
-- be posted on chain assuming the construction of the real transaction is
-- referentially transparent.
simulatedChainAndNetwork :: IO (HydraNode MockTx IO -> IO Connections)
simulatedChainAndNetwork = do
  refHistory <- newIORef []
  nodes <- newTVarIO []
  pure $ \n -> do
    atomically $ modifyTVar nodes (n :)
    pure $ Connections OnChain{postTx = postTx nodes refHistory} HydraNetwork{broadcast = broadcast nodes}
 where
  postTx nodes refHistory tx = do
    h <- readIORef refHistory
    unless (tx `elem` h) $ do
      modifyIORef' refHistory (tx :)
      readTVarIO nodes >>= mapM_ (`handleChainTx` tx)

  broadcast nodes msg = readTVarIO nodes >>= mapM_ (`handleMessage` msg)

startHydraNode :: Natural -> (HydraNode MockTx IO -> IO Connections) -> IO (HydraProcess IO MockTx)
startHydraNode nodeId connectToChain = do
  response <- newEmptyMVar
  node <- createHydraNode (Party nodeId) mockLedger (putMVar response)
  Connections cc hn <- connectToChain node
  let testNode = node{oc = cc, hn}
  nodeThread <- async $ runHydraNode testNode
  link nodeThread
  pure
    HydraProcess
      { stopHydraNode = cancel nodeThread
      , queryNodeState =
          poll nodeThread >>= \case
            Nothing -> pure Ready
            Just _ -> pure NotReady
      , sendRequest = handleClientRequest node
      , wait1sForResponse =
          timeout 1_000_000 $ takeMVar response
      , waitForLedgerState =
          \st -> do
            result <-
              timeout
                1_000_000
                ( atomically $ do
                    st' <- queryLedgerState node
                    check (st == st')
                )
            when (isNothing result) $ expectationFailure ("Expected ledger state of node " <> show nodeId <> " to be " <> show st)
      , nodeId
      }

data MockTx = ValidTx Integer | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = [MockTx]

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx _ -> Valid
        InvalidTx -> Invalid ValidationError
    , applyTransaction = \st tx -> pure (tx : st)
    , initLedgerState = []
    }
