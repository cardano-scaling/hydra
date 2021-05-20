{-# LANGUAGE TypeApplications #-}

module Test.EndToEndSpec where

import Cardano.Prelude (
  Either (..),
  ($),
 )
import HydraNode (
  Request (Init),
  Response (..),
  sendRequest,
  wait3sForResponse,
  withHydraNode,
 )
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )

spec :: Spec
spec = describe "End-to-end test using a mocked chain though" $ do
  it "can init a head between three hydra nodes" $ do
    withHydraNode 1 $ \n1 ->
      withHydraNode 2 $ \n2 ->
        withHydraNode 3 $ \n3 -> do
          sendRequest n1 (Init [1, 2, 3])
          wait3sForResponse n1 `shouldReturn` Right ReadyToCommit
          wait3sForResponse n2 `shouldReturn` Right ReadyToCommit
          wait3sForResponse n3 `shouldReturn` Right ReadyToCommit
