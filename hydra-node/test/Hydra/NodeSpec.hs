{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.String (String)
import Hydra.Ledger (Ledger (Ledger, canApply), ValidationError (ValidationError), ValidationResult (..))
import Hydra.Logic (
  ClientInstruction (..),
  HeadState (..),
  HydraMessage (ReqTx),
  OnChainTx (..),
 )
import qualified Hydra.Logic.SimpleHead as SimpleHead
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
  shouldSatisfy,
 )

spec :: Spec
spec = describe "Hydra Node" $ do
  it "does nothing for a second without events" $ do
    q <- createEventQueue
    hh <- createHydraHead InitState mockLedger
    res <- timeout 1000000 $ handleNextEvent q mockNetwork mockChain mockClientSide hh
    res `shouldBe` Nothing
    queryHeadState hh `shouldReturn` InitState

  it "does something" $ do
    hh <- createHydraHead InitState mockLedger
    res <- init (expectOnChain InitTx) hh (expectClientSide AcceptingTx)
    res `shouldBe` Right ()
    queryHeadState hh >>= shouldNotBe InitState

  it "does send transactions received from client onto the network" $ do
    hh <- createHydraHead (OpenState SimpleHead.mkState) mockLedger
    (n, queryNetworkMsgs) <- recordNetwork
    void $ newTx hh n ValidTx
    queryHeadState hh >>= flip shouldSatisfy isOpen
    queryNetworkMsgs `shouldReturn` [ReqTx]

  it "does not forward invalid transactions received from client" $ do
    hh <- createHydraHead (OpenState SimpleHead.mkState) mockLedger
    newTx hh mockNetwork InvalidTx `shouldReturn` Invalid ValidationError
    queryHeadState hh >>= flip shouldSatisfy isOpen

data MockTx = ValidTx | InvalidTx
  deriving (Eq, Show)

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \_ -> \case
        ValidTx -> Valid
        InvalidTx -> Invalid ValidationError
    }

isOpen :: HeadState tx -> Bool
isOpen OpenState{} = True
isOpen _ = False

recordNetwork :: IO (HydraNetwork IO, IO [HydraMessage])
recordNetwork = do
  ref <- newIORef []
  pure (HydraNetwork{broadcast = recordMsg ref}, queryMsgs ref)
 where
  recordMsg ref x = atomicModifyIORef' ref $ \old -> (old <> [x], ())

  queryMsgs = readIORef

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
