{-# LANGUAGE TypeApplications #-}

module Test.EndToEndSpec where

import Cardano.Prelude
import HydraNode (
  failAfter,
  getEventsCountMetric,
  sendRequest,
  wait3sForResponse,
  withHydraNode,
  withMockChain,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  describe "three hydra nodes scenario" $ do
    it "inits a head and accepts a valid tx" $ do
      failAfter 10 $
        withMockChain $
          withHydraNode 1 $ \n1 ->
            withHydraNode 2 $ \n2 ->
              withHydraNode 3 $ \n3 -> do
                sendRequest n1 "Init [1, 2, 3]"
                wait3sForResponse [n1, n2, n3] "ReadyToCommit"
                sendRequest n1 "Commit 10"
                sendRequest n2 "Commit 20"
                sendRequest n3 "Commit 5"
                wait3sForResponse [n1, n2, n3] "HeadIsOpen"
                sendRequest n1 "NewTx (ValidTx 1)"
                wait3sForResponse [n2] "TxReceived (ValidTx 1)"

    -- NOTE(SN): This is likely too detailed and should move to a lower-level
    -- integration test
    it "init a head and reject too expensive tx" $ do
      failAfter 10 $
        withMockChain $
          withHydraNode 1 $ \n1 ->
            withHydraNode 2 $ \n2 ->
              withHydraNode 3 $ \n3 -> do
                sendRequest n1 "Init [1, 2, 3]"
                wait3sForResponse [n1, n2, n3] "ReadyToCommit"
                sendRequest n1 "Commit 10"
                sendRequest n2 "Commit 20"
                sendRequest n3 "Commit 5"
                wait3sForResponse [n1, n2, n3] "HeadIsOpen"
                -- NOTE(SN): Everything above this boilerplate
                sendRequest n1 "NewTx InvalidTx"
                wait3sForResponse [n1] "TxInvalid InvalidTx"

  describe "Monitoring" $ do
    it "Node exposes JSON metrics on port 6001" $ do
      failAfter 10 $
        withMockChain $
          withHydraNode 1 $ \n1 -> do
            sendRequest n1 "Init [1, 2, 3]"
            wait3sForResponse [n1] "ReadyToCommit"

            getEventsCountMetric n1 `shouldReturn` Just 2
