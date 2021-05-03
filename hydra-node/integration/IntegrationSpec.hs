{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude

import Hydra.Node (NodeState (Ready), queryNodeState, startHydraNode, stopHydraNode)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integration tests" $ do
  it "can be started and stopped" $ do
    n <- startHydraNode @IO
    stopHydraNode n

  it "is Ready when started" $ do
    n <- startHydraNode @IO
    queryNodeState n `shouldReturn` Ready
