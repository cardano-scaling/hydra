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
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
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
                sendRequest n1 $
                  request "init" []
                waitForResponse 3 [n1, n2, n3] $
                  response "readyToCommit" ["parties" .= [10, 20, 30 :: Int]]
                sendRequest n1 $
                  request "commit" ["utxo" .= [1 :: Int]]
                sendRequest n2 $
                  request "commit" ["utxo" .= [2 :: Int]]
                sendRequest n3 $
                  request "commit" ["utxo" .= [3 :: Int]]

                waitForResponse 3 [n1, n2, n3] $
                  response "headIsOpen" ["utxo" .= [1, 2, 3 :: Int]]

                let tx = object ["id" .= (42 :: Int), "inputs" .= [1 :: Int], "outputs" .= [4 :: Int]]
                sendRequest n1 $
                  request "newTransaction" ["transaction" .= tx]

                waitForResponse 10 [n1, n2, n3] $
                  response "transactionSeen" ["transaction" .= tx]
                waitForResponse 10 [n1, n2, n3] $
                  response "snapshotConfirmed" ["snapshotNumber" .= (1 :: Int)]
                sendRequest n1 $
                  request "close" []
                waitForResponse 3 [n1] $
                  response
                    "headIsClosed"
                    [ "contestationPeriod" .= contestationPeriod
                    , "latestSnapshot"
                        .= object
                          [ "snapshotNumber" .= (1 :: Int)
                          , "utxo" .= [2, 3, 4 :: Int]
                          , "confirmedTransactions" .= [tx]
                          ]
                    ]
                waitForResponse (contestationPeriod + 3) [n1] $
                  response "headIsFinalized" ["utxo" .= [2, 3, 4 :: Int]]

  describe "Monitoring" $ do
    it "Node exposes Prometheus metrics on port 6001" $ do
      failAfter 20 $
        withMockChain $
          withHydraNode 1 aliceSk [bobVk, carolVk] $ \n1 ->
            withHydraNode 2 bobSk [aliceVk, carolVk] $ \_n2 ->
              withHydraNode 3 carolSk [aliceVk, bobVk] $ \_n3 -> do
                waitForNodesConnected [n1]
                sendRequest n1 $ request "init" []
                waitForResponse 3 [n1] $ response "readyToCommit" ["parties" .= [10, 20, 30 :: Int]]
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

request :: Text -> [Pair] -> Value
request tag pairs = object $ ("request" .= tag) : pairs

response :: Text -> [Pair] -> Value
response tag pairs = object $ ("response" .= tag) : pairs
