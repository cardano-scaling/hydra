{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.EndToEndSpec where

import Hydra.Prelude

import Cardano.Crypto.DSIGN (
  DSIGNAlgorithm (deriveVerKeyDSIGN),
  MockDSIGN,
  SignKeyDSIGN,
  VerKeyDSIGN,
 )
import Data.Aeson (object, (.=))
import qualified Data.ByteString as BS
import HydraNode (
  failAfter,
  getMetrics,
  hydraNodeProcess,
  readCreateProcess,
  sendInput,
  waitForNodesConnected,
  waitForOutput,
  withHydraNode,
  withMockChain,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldSatisfy,
 )
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

aliceSk, bobSk, carolSk :: SignKeyDSIGN MockDSIGN
aliceSk = 10
bobSk = 20
carolSk = 30

aliceVk, bobVk, carolVk :: VerKeyDSIGN MockDSIGN
aliceVk = deriveVerKeyDSIGN aliceSk
bobVk = deriveVerKeyDSIGN bobSk
carolVk = deriveVerKeyDSIGN carolSk

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
                sendInput n1 "init" []
                waitForOutput 3 [n1, n2, n3] "readyToCommit" ["parties" .= [int 10, 20, 30]]
                sendInput n1 "commit" ["utxo" .= [int 1]]
                sendInput n2 "commit" ["utxo" .= [int 2]]
                sendInput n3 "commit" ["utxo" .= [int 3]]

                waitForOutput 3 [n1, n2, n3] "headIsOpen" ["utxo" .= [int 1, 2, 3]]

                let tx = object ["id" .= int 42, "inputs" .= [int 1], "outputs" .= [int 4]]
                sendInput n1 "newTransaction" ["transaction" .= tx]

                waitForOutput 10 [n1, n2, n3] "transactionSeen" ["transaction" .= tx]
                waitForOutput 10 [n1, n2, n3] "snapshotConfirmed" ["snapshotNumber" .= int 1]
                sendInput n1 "close" []
                waitForOutput
                  3
                  [n1]
                  "headIsClosed"
                  [ "contestationPeriod" .= contestationPeriod
                  , "latestSnapshot"
                      .= object
                        [ "snapshotNumber" .= int 1
                        , "utxo" .= [int 2, 3, 4]
                        , "confirmedTransactions" .= [tx]
                        ]
                  ]
                waitForOutput (contestationPeriod + 3) [n1] "headIsFinalized" ["utxo" .= [int 2, 3, 4]]

  describe "Monitoring" $ do
    it "Node exposes Prometheus metrics on port 6001" $ do
      failAfter 20 $
        withMockChain $
          withHydraNode 1 aliceSk [bobVk, carolVk] $ \n1 ->
            withHydraNode 2 bobSk [aliceVk, carolVk] $ \_n2 ->
              withHydraNode 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                waitForNodesConnected [n1]
                sendInput n1 "init" []
                waitForOutput 3 [n1] "readyToCommit" ["parties" .= [int 10, 20, 30]]
                metrics <- getMetrics n1
                metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

  describe "hydra-node executable" $ do
    it "display proper semantic version given it is passed --version argument" $ do
      failAfter 5 $ do
        version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
        version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Helpers
--

int :: Int -> Int
int = id
