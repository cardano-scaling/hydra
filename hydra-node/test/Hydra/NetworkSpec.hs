-- | Test the real networking layer
module Hydra.NetworkSpec where

import Cardano.Prelude

import Hydra.Logic (Event (NetworkEvent), HydraMessage (ReqTx))
import Hydra.Network (HydraNetwork (broadcast), createSimulatedHydraNetwork)
import Hydra.Node (EventQueue (nextEvent), createEventQueue)
import Test.Hspec (Spec, describe, it, shouldReturn)

type MockTx = ()

spec :: Spec
spec = describe "the networking layer" $ do
  it "broadcasts messages to a single other host" $ do
    eq1 <- createEventQueue -- TODO(SN): use callbacks instead?
    hn1 <- createSimulatedHydraNetwork ["hn2"] eq1

    eq2 <- createEventQueue -- TODO(SN): use callbacks instead?
    _ <- createSimulatedHydraNetwork ["hn1"] eq2

    broadcast hn1 ReqTx
    nextEvent eq2 `shouldReturn` (NetworkEvent ReqTx :: Event MockTx)
