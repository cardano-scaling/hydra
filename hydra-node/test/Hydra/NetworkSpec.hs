-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude

import Data.IORef (newIORef, readIORef)
import Hydra.Logic (Event (NetworkEvent), HydraMessage (ReqTx))
import Hydra.Network (HydraNetwork (broadcast), createOuroborosHydraNetwork)
import Test.Hspec (Spec, describe, it, shouldReturn)

type MockTx = ()

spec :: Spec
spec = describe "the networking layer" $ do
  it "broadcasts messages to a single other host" $ do
    queue1 <- newIORef []
    _queue2 <- newIORef []

    let _callback1 = panic "TODO"
        _callback2 = panic "TODO"

    hn1 <- createOuroborosHydraNetwork ["hn2"] _callback1
    _ <- createOuroborosHydraNetwork ["hn1"] _callback2

    broadcast hn1 ReqTx
    readIORef queue1 `shouldReturn` ([NetworkEvent ReqTx] :: [Event MockTx])
