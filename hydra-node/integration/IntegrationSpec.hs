{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude
import Control.Concurrent.STM (modifyTVar, newTVarIO, readTVarIO)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Hydra.Ledger (Ledger (..), LedgerState, ValidationError (..), ValidationResult (Invalid, Valid))
import Hydra.Logic (ClientRequest (..), ClientResponse (..))
import Hydra.Node (ClientSide (..), HydraNode (..), OnChain (..), createHydraNode, handleChainTx, handleClientRequest, runHydraNode)
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
      n <- simulatedChain >>= startHydraNode 1
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- simulatedChain >>= startHydraNode 1
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Hydra node integration" $ do
    it "accepts Init command" $ do
      n <- simulatedChain >>= startHydraNode 1
      sendRequest n Init `shouldReturn` ()

    it "accepts Commit after successful Init" $ do
      n <- simulatedChain >>= startHydraNode 1
      sendRequest n Init
      sendRequest n Commit

    it "accepts a tx after the head was opened between two nodes" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 Init
      waitForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit

      waitForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      waitForResponse n2 `shouldReturn` Just HeadIsOpen
      sendRequest n2 (NewTx ValidTx)

    it "not accepts commits when the head is open" $ do
      n1 <- simulatedChain >>= startHydraNode 1
      sendRequest n1 Init
      waitForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      waitForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Commit
      waitForResponse n1 `shouldReturn` Just CommandFailed

    it "can close an open head" $ do
      n1 <- simulatedChain >>= startHydraNode 1
      sendRequest n1 Init
      waitForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      waitForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Close
      waitForResponse n1 `shouldReturn` Just HeadIsClosed

    it "sees the head closed by other nodes" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 Init
      waitForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit

      waitForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      waitForResponse n2 `shouldReturn` Just HeadIsOpen

      waitForResponse n1 `shouldReturn` Just HeadIsOpen
      sendRequest n1 Close

      waitForResponse n2 `shouldReturn` Just HeadIsClosed

    it "only opens the head after all nodes committed" $ do
      chain <- simulatedChain
      n1 <- startHydraNode 1 chain
      n2 <- startHydraNode 2 chain

      sendRequest n1 Init
      waitForResponse n1 `shouldReturn` Just ReadyToCommit
      sendRequest n1 Commit
      waitForResponse n1 >>= (`shouldNotBe` Just HeadIsOpen)

      waitForResponse n2 `shouldReturn` Just ReadyToCommit
      sendRequest n2 Commit
      waitForResponse n2 `shouldReturn` Just HeadIsOpen

      -- Only now the head should be open for node 1
      waitForResponse n1 `shouldReturn` Just HeadIsOpen

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m = HydraProcess
  { nodeId :: Integer
  , stopHydraNode :: m ()
  , sendRequest :: ClientRequest MockTx -> m ()
  , -- | Waits for one second max.
    waitForResponse :: m (Maybe ClientResponse)
  , queryNodeState :: m NodeState
  }

simulatedChain :: IO (HydraNode MockTx IO -> IO (OnChain IO))
simulatedChain = do
  nodes <- newTVarIO []
  pure $ \n -> do
    refHistory <- newIORef []
    atomically $ modifyTVar nodes (n :)
    pure $ OnChain{postTx = postTx nodes refHistory}
 where
  postTx nodes refHistory tx = do
    h <- readIORef refHistory
    when (tx `elem` h) $ expectationFailure ("cannot post the same transaction " <> show tx <> " twice")
    modifyIORef' refHistory (tx :)
    readTVarIO nodes >>= mapM_ (`handleChainTx` tx)

startHydraNode :: Integer -> (HydraNode MockTx IO -> IO (OnChain IO)) -> IO (HydraProcess IO)
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
      , waitForResponse =
          timeout 1_000_000 $ takeMVar response
      , nodeId
      }
 where
  testHydraNode :: IO (HydraNode MockTx IO)
  testHydraNode = createHydraNode mockLedger

data MockTx = ValidTx | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = ()

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx -> Valid
        InvalidTx -> Invalid ValidationError
    , initLedgerState = ()
    }
