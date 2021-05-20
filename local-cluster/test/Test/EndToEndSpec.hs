{-# LANGUAGE TypeApplications #-}

module Test.EndToEndSpec where

import Cardano.Prelude
import HydraNode
import Safe (
  readMay,
 )
import System.IO (
  hGetLine,
 )
import System.Timeout (
  timeout,
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
          wait1sForResponse n1 `shouldReturn` Just ReadyToCommit
          wait1sForResponse n2 `shouldReturn` Just ReadyToCommit
          wait1sForResponse n3 `shouldReturn` Just ReadyToCommit

wait1sForResponse :: HydraNode -> IO (Maybe Response)
wait1sForResponse HydraNode{outputStream} = do
  result <- timeout 1 $ hGetLine outputStream
  pure $ result >>= readMay
