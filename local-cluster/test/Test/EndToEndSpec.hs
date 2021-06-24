{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import qualified Data.ByteString as BS
import HydraNode (
  failAfter,
  getMetrics,
  hydraNodeProcess,
  readCreateProcess,
  sendRequest,
  waitForNodesConnected,
  waitForResponse,
  withHydraNode,
  withMockChain,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  pendingWith,
  shouldSatisfy,
 )
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = 11
bobVk = 21
carolVk = 31

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  describe "three hydra nodes scenario" $ do
    it "inits and closes a head with a single mock transaction" $ do
      failAfter 30 $
        withMockChain $
          withHydraNode 1 aliceSk [bobVk, carolVk] $ \n1 ->
            withHydraNode 2 bobSk [aliceVk, carolVk] $ \n2 ->
              withHydraNode 3 carolSk [aliceVk, bobVk] $ \n3 -> do
                waitForNodesConnected [n1, n2, n3]
                let contestationPeriod = 3 -- TODO: Should be part of init
                sendRequest n1 "Init"
                waitForResponse 3 [n1, n2, n3] "ReadyToCommit [VerKeyMockDSIGN 11, VerKeyMockDSIGN 21, VerKeyMockDSIGN 31]"
                sendRequest n1 "Commit (fromList [1])"
                sendRequest n2 "Commit (fromList [2])"
                sendRequest n3 "Commit (fromList [3])"

                waitForResponse 3 [n1, n2, n3] "HeadIsOpen (fromList [1,2,3])"
                sendRequest n1 "NewTx (SimpleTx {txId = 42, txInputs = fromList [1], txOutputs = fromList [4]})"
                waitForResponse 10 [n1, n2, n3] "TxConfirmed (SimpleTx {txId = 42, txInputs = fromList [1], txOutputs = fromList [4]})"
                sendRequest n1 "Close"
                waitForResponse 3 [n1] "HeadIsClosed 3s (Snapshot {number = 0, utxo = fromList [1,2,3], confirmed = []}) [SimpleTx {txId = 42, txInputs = fromList [1], txOutputs = fromList [4]}]"
                waitForResponse (contestationPeriod + 3) [n1] "HeadIsFinalized (fromList [2,3,4])"

  describe "Monitoring" $ do
    it "Node exposes Prometheus metrics on port 6001" $ do
      failAfter 20 $
        withMockChain $
          withHydraNode 1 aliceSk [bobVk, carolVk] $ \n1 ->
            withHydraNode 2 bobSk [aliceVk, carolVk] $ \_n2 ->
              withHydraNode 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                waitForNodesConnected [n1]
                sendRequest n1 "Init"
                waitForResponse 3 [n1] "ReadyToCommit"

                metrics <- getMetrics n1
                metrics `shouldSatisfy` ("hydra_head_events  5" `BS.isInfixOf`)

  describe "hydra-node executable" $ do
    it "display proper semantic version given it is passed --version argument" $ do
      failAfter 5 $ do
        version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
        version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))
