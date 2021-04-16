module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.String (String)
import Hydra.Logic (HeadState (InitState))
import System.Timeout (timeout)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldReturn)

spec :: Spec
spec = describe "Hydra Node" $ do
  it "does nothing for a second without events" $ do
    q <- createEventQueue
    hh <- createHydraHead InitState
    res <- timeout 1000000 $ runHydra q mockNetwork mockChain hh
    res `shouldBe` Nothing
    queryHeadState hh `shouldReturn` InitState

mockNetwork :: HydraNetwork IO
mockNetwork =
  HydraNetwork
    { broadcast = \x -> shouldNotBeCalled $ "broadcast(" <> show x <> ")"
    }

mockChain :: OnChain IO
mockChain =
  OnChain
    { postTx = \x -> shouldNotBeCalled $ "postTx(" <> show x <> ")"
    }

shouldNotBeCalled :: String -> Expectation
shouldNotBeCalled name = expectationFailure $ name <> " should not have been called"
