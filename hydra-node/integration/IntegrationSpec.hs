{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude
import Control.Concurrent.STM (modifyTVar, newTVarIO, readTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hydra.Ledger (Ledger (..), LedgerState, ValidationError (..), ValidationResult (Invalid, Valid))
import Hydra.Logic (ClientRequest (..), ClientResponse (..), Party (..))
import Hydra.Node (ClientSide (..), HydraNode (..), OnChain (..), createHydraNode, handleChainTx, handleClientRequest, runHydraNode)
import System.Timeout (timeout)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldNotBe,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integrating one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $ do
    it "is Ready when started" $ do
      n <- simulatedChain >>= startHydraNode 1
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- simulatedChain >>= startHydraNode 1
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Hydra node integration" $ do
    it "accepts Init command" $ do
      n <- simulatedChain >>= startHydraNode 1
      sendRequest n (Init [1]) `shouldReturn` ()

    it "accepts Commit after successful Init" $ do
      n <- simulatedChain >>= startHydraNode 1
      sendRequest n (Init [1])
      sendRequest n Commit

    it "accepts a tx after the head was opened between two nodes" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 (Init [1, 2])
      wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      wait1sForResponse n2 `shouldReturn` Just HeadIsOpen
      sendRequest n2 (NewTx $ ValidTx 1)

    it "not accepts commits when the head is open" $ do
      n1 <- simulatedChain >>= startHydraNode 1
      sendRequest n1 (Init [1])
      wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Commit
      wait1sForResponse n1 `shouldReturn` Just CommandFailed

    it "can close an open head" $ do
      n1 <- simulatedChain >>= startHydraNode 1
      sendRequest n1 (Init [1])
      wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Close
      wait1sForResponse n1 `shouldReturn` Just HeadIsClosed

    it "sees the head closed by other nodes" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 (Init [1, 2])
      wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      wait1sForResponse n2 `shouldReturn` Just HeadIsOpen

      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Close

      wait1sForResponse n2 `shouldReturn` Just HeadIsClosed

    it "only opens the head after all nodes committed" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 (Init [1, 2])
      wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n1 >>= (`shouldNotBe` Just HeadIsOpen)

      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      wait1sForResponse n2 `shouldReturn` Just HeadIsOpen

      -- Only now the head should be open for node 1
      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen

    it "valid new transaction in open head is stored in ledger" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequestAndWaitFor n1 (Init [1, 2]) ReadyToCommit
      sendRequest n1 Commit
      wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequestAndWaitFor n2 Commit HeadIsOpen
      wait1sForResponse n1 `shouldReturn` Just HeadIsOpen

      sendRequest n1 (NewTx $ ValidTx 1)
      waitForLedgerState n1 `shouldReturn` Just [ValidTx 1]
      waitForLedgerState n2 `shouldReturn` Just [ValidTx 1]

waitForLedgerState :: HydraProcess IO -> IO (Maybe [MockTx])
waitForLedgerState = panic "not implemented"

sendRequestAndWaitFor :: HydraProcess IO -> ClientRequest MockTx -> ClientResponse -> IO ()
sendRequestAndWaitFor node req expected =
  sendRequest node req >> (wait1sForResponse node `shouldReturn` Just expected)

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m = HydraProcess
  { nodeId :: Natural
  , stopHydraNode :: m ()
  , sendRequest :: ClientRequest MockTx -> m ()
  , wait1sForResponse :: m (Maybe ClientResponse)
  , queryNodeState :: m NodeState
  }

-- | Creates a simulated chain by returning a function to create the chain
-- client interface for a node. This is necessary, to get to know all nodes
-- which use this function and simulate an 'OnChainTx' happening.
--
-- NOTE: This implementation currently ensures that no two equal 'OnChainTx' can
-- be posted on chain assuming the construction of the real transaction is
-- referentially transparent.
simulatedChain :: IO (HydraNode MockTx IO -> IO (OnChain IO))
simulatedChain = do
  refHistory <- newIORef []
  nodes <- newTVarIO []
  pure $ \n -> do
    atomically $ modifyTVar nodes (n :)
    pure $ OnChain{postTx = postTx nodes refHistory}
 where
  postTx nodes refHistory tx = do
    h <- readIORef refHistory
    unless (tx `elem` h) $ do
      modifyIORef' refHistory (tx :)
      readTVarIO nodes >>= mapM_ (`handleChainTx` tx)

startHydraNode :: Natural -> (HydraNode MockTx IO -> IO (OnChain IO)) -> IO (HydraProcess IO)
startHydraNode nodeId connectToChain = do
  node <- testHydraNode
  cc <- connectToChain node
  response <- newEmptyMVar
  let testNode = node{oc = cc, cs = ClientSide{sendResponse = putMVar response}}
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
      , nodeId
      }
 where
  testHydraNode :: IO (HydraNode MockTx IO)
  testHydraNode = createHydraNode (Party nodeId) mockLedger

data MockTx = ValidTx Integer | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = [MockTx]

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx _ -> Valid
        InvalidTx -> Invalid ValidationError
    , initLedgerState = []
    }
