{-# LANGUAGE TypeApplications #-}

module IntegrationSpec where

import Cardano.Prelude

import Hydra.Node (NodeState (..), queryNodeState, startHydraNode, stopHydraNode)
import Test.Hspec (
  Spec,
  describe,
  it,
  shouldReturn,
 )

spec :: Spec
spec = describe "Integration tests" $ do
  it "is Ready when started" $ do
    n <- startHydraNode @IO
    queryNodeState n `shouldReturn` Ready

  it "is NotReady when stopped" $ do
    n <- startHydraNode @IO
    stopHydraNode n
    queryNodeState n `shouldReturn` NotReady
