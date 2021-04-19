module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.String (String)
import Hydra.Logic (
  ClientInstruction (..),
  Event,
  HeadState (..),
  OnChainTx (..),
 )
import System.Timeout (timeout)
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
  shouldNotBe,
  shouldReturn,
 )

spec :: Spec
spec = describe "Hydra Node" $ do
  it "does nothing for a second without events" $ do
    q <- createEventQueue
    hh <- createHydraHead InitState
    res <- timeout 1000000 $ runHydra q mockNetwork mockChain mockClientSide hh
    res `shouldBe` Nothing
    queryHeadState hh `shouldReturn` InitState
  it "does something" $ do
    hh <- createHydraHead InitState
    init (expectOnChain InitTx) hh (expectClientSide AcceptingTx)
    queryHeadState hh >>= shouldNotBe InitState

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

expectOnChain :: OnChainTx -> OnChain IO
expectOnChain expected =
  OnChain
    { postTx = (`shouldBe` expected)
    }

mockClientSide :: ClientSide IO
mockClientSide =
  ClientSide
    { showInstruction = \x -> shouldNotBeCalled $ "showInstruction(" <> show x <> ")"
    }

expectClientSide :: ClientInstruction -> ClientSide IO
expectClientSide ins =
  ClientSide
    { showInstruction = (`shouldBe` ins)
    }

shouldNotBeCalled :: String -> Expectation
shouldNotBeCalled name = expectationFailure $ name <> " should not have been called"
