{-# LANGUAGE TypeApplications #-}

module Test.EndToEndSpec where

import Cardano.Prelude (
  ($),
 )
import HydraNode (
  Request (Commit, Init, NewTx),
  Response (..),
  sendRequest,
  wait3sForResponse,
  withHydraNode,
  withMockChain,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
 )

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  it "can init a head between three hydra nodes" $ do
    withMockChain $
      withHydraNode 1 $ \n1 ->
        withHydraNode 2 $ \n2 ->
          withHydraNode 3 $ \n3 -> do
            sendRequest n1 (Init [1, 2, 3])
            wait3sForResponse [n1] ReadyToCommit
            sendRequest n1 Commit
            wait3sForResponse [n2] ReadyToCommit
            sendRequest n2 Commit
            wait3sForResponse [n3] ReadyToCommit
            sendRequest n3 Commit

            wait3sForResponse [n1, n2, n3] HeadIsOpen

            sendRequest n1 (NewTx 1)
            -- NOTE(SN) we could wait for 'TxReceived 1' explicitly and ignore other responses
            wait3sForResponse [n2] (TxReceived 1)
