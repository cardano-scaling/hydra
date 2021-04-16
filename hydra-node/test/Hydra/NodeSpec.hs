module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.String (String)
import Hydra.Logic (ClientCommand (Close, Init, NewTx), Event (ClientEvent), HeadState (ClosedState, InitState))
import System.Timeout (timeout)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldReturn)

spec :: Spec
spec = describe "Hydra Node" $ do
  it "does nothing for a second without events" $ do
    q <- createEventQueue
    hh <- createHydraHead InitState
    res <- timeout 1000000 $ runHydra q mockNetwork mockChain hh
    res `shouldBe` Nothing
    queryHeadState hh `shouldReturn` InitState
  it "does something" $ do
    q <- primedEventQueue [ClientEvent Init, ClientEvent NewTx, ClientEvent Close]
    hh <- createHydraHead InitState
    n <- createHydraNetwork q
    runHydra q n mockChain hh
    queryHeadState hh >>= shouldNotBe InitState
    runHydra q n mockChain hh
    runHydra q n mockChain hh
    queryHeadState hh >>= shouldBe ClosedState

primedEventQueue :: [Event] -> IO (EventQueue IO)
primedEventQueue es = do
  q@EventQueue{putEvent} <- createEventQueue
  mapM_ putEvent es
  pure q

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
