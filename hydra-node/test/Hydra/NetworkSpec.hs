{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude hiding (threadDelay)

import Control.Monad.Class.MonadTimer (threadDelay, timeout)
import Hydra.Logic (HydraMessage (ReqTx))
import Hydra.Network.Ouroboros (broadcast, withOuroborosHydraNetwork)
import Hydra.Network.ZeroMQ (withZeroMQHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn, xit)

type MockTx = ()

spec :: Spec
spec = describe "Networking layer" $ do
  describe "Ouroboros Network" $ do
    xit "broadcasts messages to single connected peer" $ do
      received <- newEmptyMVar
      failAfter2Seconds $ do
        withOuroborosHydraNetwork ("127.0.0.1", "45678") [("127.0.0.1", "45679")] (const $ pure ()) $ \hn1 ->
          withOuroborosHydraNetwork ("127.0.0.1", "45679") [("127.0.0.1", "45678")] (putMVar received) $ \_ -> do
            broadcast hn1 ReqTx
            takeMVar received `shouldReturn` ReqTx

  describe "0MQ Network" $ do
    it "broadcasts messages to 2 connected peers" $ do
      node2received <- newEmptyMVar
      node3received <- newEmptyMVar
      failAfter2Seconds $ do
        withZeroMQHydraNetwork ("127.0.0.1", "55677") [("127.0.0.1", "55678"), ("127.0.0.1", "55679")] (const $ pure ()) $ \hn1 ->
          withZeroMQHydraNetwork ("127.0.0.1", "55678") [("127.0.0.1", "55677"), ("127.0.0.1", "55679")] (putMVar node2received) $ \_ ->
            withZeroMQHydraNetwork ("127.0.0.1", "55679") [("127.0.0.1", "55677"), ("127.0.0.1", "55678")] (putMVar node3received) $ \_ -> do
              threadDelay 1 -- This is needed to wait for all nodes to be up
              broadcast hn1 ReqTx
              takeMVar node2received `shouldReturn` ReqTx
              takeMVar node3received `shouldReturn` ReqTx

failAfter2Seconds :: IO () -> IO ()
failAfter2Seconds action =
  timeout 2 action >>= \case
    Nothing -> expectationFailure "Test timed out after 2 seconds"
    Just _ -> pure ()
