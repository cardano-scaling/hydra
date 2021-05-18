{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude

import Control.Monad.Class.MonadTimer (timeout)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Hydra.Logic (Event (NetworkEvent), HydraMessage (ReqTx))
import Hydra.Network (HydraNetwork (broadcast), withOuroborosHydraNetwork)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldReturn)

type MockTx = ()

spec :: Spec
spec = describe "Ouroboros networking layer" $ do
  it "broadcasts messages to single connected peer" $ do
    queue2 <- newIORef []
    let callback2 msg = atomicModifyIORef' queue2 $ \msgs -> (NetworkEvent msg : msgs, ())

    failAfter2Seconds $ do
      withOuroborosHydraNetwork ("127.0.0.1", "45678") [("127.0.0.1", "45679")] (const $ pure ()) $ \hn1 ->
        withOuroborosHydraNetwork ("127.0.0.1", "45679") [("127.0.0.1", "45678")] callback2 $ \_ -> do
          broadcast hn1 ReqTx
          readIORef queue2 `shouldReturn` ([NetworkEvent ReqTx] :: [Event MockTx])

failAfter2Seconds :: IO () -> IO ()
failAfter2Seconds action =
  timeout 2 action >>= \case
    Nothing -> expectationFailure "Test timed out after 2 seconds"
    Just _ -> pure ()
