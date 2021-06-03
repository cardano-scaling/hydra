{-# LANGUAGE TypeApplications #-}

module Test.EndToEndSpec where

import Cardano.Prelude
import qualified Data.ByteString as BS
import HydraNode (
  failAfter,
  getMetrics,
  sendRequest,
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

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  describe "three hydra nodes scenario" $ do
    it "inits a head and accepts a valid tx" $ do
      failAfter 30 $
        withMockChain $
          withHydraNode 1 $ \n1 ->
            withHydraNode 2 $ \n2 ->
              withHydraNode 3 $ \n3 -> do
                waitForResponse 10 [n1, n2, n3] "NodeConnectedToNetwork"
                sendRequest n1 "Init [1, 2, 3]"
                waitForResponse 3 [n1, n2, n3] "ReadyToCommit"
                sendRequest n1 "Commit 10"
                sendRequest n2 "Commit 20"
                sendRequest n3 "Commit 5"
                waitForResponse 3 [n1, n2, n3] "HeadIsOpen"
                sendRequest n1 "NewTx (ValidTx 1)"
                waitForResponse 3 [n2] "ReqTxReceived (ValidTx 1)"
                -- NOTE(SN): Here any number of head interactions would take
                -- place. For now we do only have this requested, but not
                -- confirmed transaction.
                sendRequest n1 "Close"
                waitForResponse 3 [n1] "HeadIsClosing 3600"
                waitForResponse 3 [n1] "NotContested 2"
                waitForResponse 3 [n1] "NotContested 3"
                waitForResponse 3 [n1] "HeadIsClosed []"

    -- NOTE(SN): This is likely too detailed and should move to a lower-level
    -- integration test
    it "init a head and reject too expensive tx" $ do
      failAfter 30 $
        withMockChain $
          withHydraNode 1 $ \n1 ->
            withHydraNode 2 $ \n2 ->
              withHydraNode 3 $ \n3 -> do
                waitForResponse 10 [n1, n2, n3] "NodeConnectedToNetwork"
                sendRequest n1 "Init [1, 2, 3]"
                waitForResponse 3 [n1, n2, n3] "ReadyToCommit"
                sendRequest n1 "Commit 10"
                sendRequest n2 "Commit 20"
                sendRequest n3 "Commit 5"
                waitForResponse 3 [n1, n2, n3] "HeadIsOpen"
                -- NOTE(SN): Everything above this boilerplate
                sendRequest n1 "NewTx InvalidTx"

                waitForResponse 3 [n1] "TxInvalid InvalidTx"

  describe "Monitoring" $ do
    it "Node exposes Prometheus metrics on port 6001" $ do
      failAfter 20 $
        withMockChain $
          withHydraNode 1 $ \n1 -> do
            withHydraNode 2 $ \_ ->
              withHydraNode 3 $ \_ -> do
                waitForResponse 10 [n1] "NodeConnectedToNetwork"
                sendRequest n1 "Init [1, 2, 3]"
                waitForResponse 3 [n1] "ReadyToCommit"

                metrics <- getMetrics n1
                metrics `shouldSatisfy` ("hydra_head_events  3" `BS.isInfixOf`)
