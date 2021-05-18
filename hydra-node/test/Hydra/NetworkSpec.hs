{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude

import Control.Monad.Class.MonadTimer (timeout)
import Hydra.Logic (HydraMessage (ReqTx))
import Hydra.Network (HydraNetwork (broadcast), withOuroborosHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)

type MockTx = ()

spec :: Spec
spec = describe "Ouroboros networking layer" $ do
  it "broadcasts messages to single connected peer" $ do
    received <- newEmptyMVar
    failAfter2Seconds $ do
      withOuroborosHydraNetwork ("127.0.0.1", "45678") [("127.0.0.1", "45679")] (const $ pure ()) $ \hn1 ->
        withOuroborosHydraNetwork ("127.0.0.1", "45679") [("127.0.0.1", "45678")] (putMVar received) $ \_ -> do
          broadcast hn1 ReqTx
          takeMVar received `shouldReturn` ReqTx

failAfter2Seconds :: IO () -> IO ()
failAfter2Seconds action =
  timeout 2 action >>= \case
    Nothing -> expectationFailure "Test timed out after 2 seconds"
    Just _ -> pure ()
