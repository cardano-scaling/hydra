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
import Hydra.Logging (showLogsOnFailure)
import HydraNode (
  failAfter,
  getMetrics,
  hydraNodeProcess,
  input,
  output,
  readCreateProcess,
  send,
  waitFor,
  waitForNodesConnected,
  withHydraNode,
  withMockChain,
 )
import Test.Hspec (
  Spec,
  around,
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
spec = around showLogsOnFailure $
  describe "End-to-end test using a mocked chain though" $ do
    describe "three hydra nodes scenario" $ do
      it "inits and closes a head with a single mock transaction" $ \tracer -> do
        failAfter 30 $
          withMockChain $ \chainPorts ->
            withHydraNode tracer chainPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
              withHydraNode tracer chainPorts 2 bobSk [aliceVk, carolVk] $ \n2 ->
                withHydraNode tracer chainPorts 3 carolSk [aliceVk, bobVk] $ \n3 -> do
                  waitForNodesConnected tracer [n1, n2, n3]
                  let contestationPeriod = 10 -- TODO: Should be part of init
                  send n1 $ input "init" []
                  waitFor tracer 3 [n1, n2, n3] $
                    output "readyToCommit" ["parties" .= [int 10, 20, 30]]
                  send n1 $ input "commit" ["utxo" .= [int 1]]
                  send n2 $ input "commit" ["utxo" .= [int 2]]
                  send n3 $ input "commit" ["utxo" .= [int 3]]

                  waitFor tracer 3 [n1, n2, n3] $ output "headIsOpen" ["utxo" .= [int 1, 2, 3]]

                  let tx = object ["id" .= int 42, "inputs" .= [int 1], "outputs" .= [int 4]]
                  send n1 $ input "newTransaction" ["transaction" .= tx]

                  waitFor tracer 10 [n1, n2, n3] $ output "transactionSeen" ["transaction" .= tx]
                  waitFor tracer 10 [n1, n2, n3] $
                    output
                      "snapshotConfirmed"
                      [ "snapshot"
                          .= object
                            [ "confirmedTransactions" .= [tx]
                            , "snapshotNumber" .= int 1
                            , "utxo" .= [int 2, 3, 4]
                            ]
                      ]

                  send n1 $ input "getUtxo" []
                  waitFor tracer 10 [n1] $ output "utxo" ["utxo" .= [int 2, 3, 4]]

                  send n1 $ input "close" []
                  waitFor tracer 3 [n1] $
                    output
                      "headIsClosed"
                      [ "contestationPeriod" .= contestationPeriod
                      , "latestSnapshot"
                          .= object
                            [ "snapshotNumber" .= int 1
                            , "utxo" .= [int 2, 3, 4]
                            , "confirmedTransactions" .= [tx]
                            ]
                      ]
                  waitFor tracer (contestationPeriod + 3) [n1] $ output "headIsFinalized" ["utxo" .= [int 2, 3, 4]]

    describe "Monitoring" $ do
      it "Node exposes Prometheus metrics on port 6001" $ \tracer -> do
        failAfter 20 $
          withMockChain $ \mockPorts ->
            withHydraNode tracer mockPorts 1 aliceSk [bobVk, carolVk] $ \n1 ->
              withHydraNode tracer mockPorts 2 bobSk [aliceVk, carolVk] $ \_n2 ->
                withHydraNode tracer mockPorts 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                  waitForNodesConnected tracer [n1]
                  send n1 $ input "init" []
                  waitFor tracer 3 [n1] $ output "readyToCommit" ["parties" .= [int 10, 20, 30]]
                  metrics <- getMetrics n1
                  metrics `shouldSatisfy` ("hydra_head_events  4" `BS.isInfixOf`)

    describe "hydra-node executable" $ do
      it "display proper semantic version given it is passed --version argument" $ \_ -> do
        failAfter 5 $ do
          version <- readCreateProcess (hydraNodeProcess ["--version"]) ""
          version `shouldSatisfy` (=~ ("[0-9]+\\.[0-9]+\\.[0-9]+(-[a-zA-Z0-9]+)?" :: String))

--
-- Helpers
--

int :: Int -> Int
int = id
