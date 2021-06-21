module Hydra.NodeSpec where

import Hydra.Prelude

import Test.Hspec (
  Spec,
  describe,
  it,
  pending,
 )

spec :: Spec
spec = describe "Hydra Node business logic" $ do
  it "does initialize a Head" $ pending

  it "does send transactions received from client onto the network" $ pending

  it "does not forward invalid transactions received from client" $ pending

  it "does not broadcast reqTx given new transaction is invalid" $ pending
