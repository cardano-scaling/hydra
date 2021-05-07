{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude
import Control.Concurrent.STM (modifyTVar, newTVarIO, readTVarIO)
import Hydra.Ledger (Ledger (..), LedgerState, ValidationError (..), ValidationResult (Invalid, Valid))
import Hydra.Logic (ClientRequest (..), ClientResponse (..), Event (ClientEvent, OnChainEvent))
import Hydra.Node (ClientSide (..), EventQueue (..), HydraNode (..), OnChain (..), createHydraNode, runHydraNode)
import System.Timeout (timeout)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldBe,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integrating one ore more hydra-nodes" $ do
  describe "Sanity tests of test suite" $ do
    it "is Ready when started" $ do
      n <- simulatedChain >>= startHydraNode
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- simulatedChain >>= startHydraNode
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Hydra node integration" $ do
    it "does accept Init command" $ do
      n <- simulatedChain >>= startHydraNode
      sendCommand n Init `shouldReturn` ()

    it "does accept commits after successful Init" $ do
      n <- simulatedChain >>= startHydraNode
      sendCommand n Init
      sendCommand n Commit

    it "does accept a tx after the head is opened between many nodes" $ do
      chain <- simulatedChain
      n1 <- startHydraNode chain
      n2 <- startHydraNode chain
      sendCommand n1 Init
      failAfter (Seconds 1) $ waitForResponse n1 ReadyToCommit
      sendCommand n1 Commit
      waitForResponse n2 ReadyToCommit
      sendCommand n2 Commit
      waitForResponse n2 AcceptingTx
      sendCommand n2 (NewTx ValidTx)

newtype Seconds = Seconds Natural

failAfter :: Seconds -> IO a -> IO a
failAfter (Seconds seconds) action =
  timeout (fromIntegral seconds * 1_000_000) action >>= maybe (panic $ "timed out after " <> show seconds <> "s") pure

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m = HydraProcess
  { stopHydraNode :: m ()
  , sendCommand :: ClientRequest MockTx -> m ()
  , waitForResponse :: ClientResponse -> m ()
  , queryNodeState :: m NodeState
  }

simulatedChain :: IO (HydraNode MockTx IO -> IO (OnChain IO))
simulatedChain = do
  queues <- newTVarIO []
  pure $ \HydraNode{eq} -> do
    atomically $ modifyTVar queues (eq :)
    pure $ OnChain{postTx = \tx -> readTVarIO queues >>= mapM_ (`putEvent` OnChainEvent tx)}

startHydraNode :: (HydraNode MockTx IO -> IO (OnChain IO)) -> IO (HydraProcess IO)
startHydraNode connectToChain = do
  node@HydraNode{eq} <- testHydraNode
  cc <- connectToChain node
  response <- newEmptyMVar
  let testNode = node{oc = cc, cs = ClientSide{showInstruction = putMVar response}}
  nodeThread <- async $ forever $ runHydraNode testNode
  pure
    HydraProcess
      { stopHydraNode = cancel nodeThread
      , queryNodeState =
          poll nodeThread >>= \case
            Nothing -> pure Ready
            Just _ -> pure NotReady
      , sendCommand = putEvent eq . ClientEvent
      , waitForResponse =
          \expected -> takeMVar response >>= \actual -> actual `shouldBe` expected
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
