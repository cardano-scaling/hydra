{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Hydra.Logic (Event (NetworkEvent), HydraMessage (ReqTx))
import Hydra.Network (HydraNetwork (broadcast), createOuroborosHydraNetwork)
import Test.Hspec (Spec, describe, it, shouldReturn)

type MockTx = ()

spec :: Spec
spec = describe "Ouroboros networking layer" $ do
  it "broadcasts messages to single connected peer" $ do
    queue <- newIORef []

    let callback msg = atomicModifyIORef' queue $ \msgs -> (NetworkEvent msg : msgs, ())

    hn1 <- createOuroborosHydraNetwork "127.0.0.1:45678" ["127.0.0.1:45679"] (const $ pure ())
    _ <- createOuroborosHydraNetwork "127.0.0.1:45679" ["127.0.0.1:45678"] callback

    broadcast hn1 ReqTx
    readIORef queue `shouldReturn` ([NetworkEvent ReqTx] :: [Event MockTx])
