{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude
import Hydra.Node (HydraNode, createHydraNode)

import Hydra.Ledger (Ledger (..), LedgerState, ValidationError (..), ValidationResult (Invalid, Valid))
import Hydra.Logic (ClientRequest (Init))
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integration tests" $ do
  describe "Sanity tests of test suite" $ do
    it "is Ready when started" $ do
      n <- startHydraNode
      queryNodeState n `shouldReturn` Ready

    it "is NotReady when stopped" $ do
      n <- startHydraNode
      stopHydraNode n
      queryNodeState n `shouldReturn` NotReady

  describe "Hydra node integration" $ do
    it "does accept Init command" $ do
      n <- startHydraNode
      sendCommand n Init

data NodeState = NotReady | Ready
  deriving (Eq, Show)

data HydraProcess m = HydraProcess
  { stopHydraNode :: m ()
  , sendCommand :: ClientRequest MockTx -> m ()
  , queryNodeState :: m NodeState
  }

startHydraNode :: IO (HydraProcess IO)
startHydraNode = do
  nodeThread <- async $ forever $ threadDelay 1_000_000
  pure
    HydraProcess
      { stopHydraNode = cancel nodeThread
      , queryNodeState =
          poll nodeThread >>= \case
            Nothing -> pure Ready
            Just _ -> pure NotReady
      , sendCommand = panic "not implemented"
      }

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
