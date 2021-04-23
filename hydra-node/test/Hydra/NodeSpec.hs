{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.String (String)
import Hydra.Ledger (Ledger (Ledger, canApply, initLedgerState), LedgerState, ValidationError (ValidationError), ValidationResult (..), cardanoLedger)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.LedgerSpec as LedgerSpec
import Hydra.Logic (
  ClientInstruction (..),
  ClientRequest (..),
  Event (..),
  HeadState (..),
  HydraMessage (..),
  LogicError (..),
  OnChainTx (..),
 )
import qualified Hydra.Logic.SimpleHead as SimpleHead
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
  it "does something" $ do
    hh <- createHydraHead InitState mockLedger
    res <- init (expectOnChain InitTx) hh (expectClientSide AcceptingTx)
    res `shouldBe` Right ()
    queryHeadState hh >>= shouldNotBe InitState

  it "does send transactions received from client onto the network" $ do
    hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
    (n, queryNetworkMsgs) <- recordNetwork
    void $ newTx hh n ValidTx
    queryHeadState hh >>= flip shouldSatisfy isOpen
    queryNetworkMsgs `shouldReturn` [ReqTx]

  it "does not forward invalid transactions received from client" $ do
    hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
    newTx hh mockNetwork InvalidTx `shouldReturn` Invalid ValidationError
    queryHeadState hh >>= flip shouldSatisfy isOpen

  describe "Hydra Node Client" $ do
    it "does not broadcast reqTx given new transaction is invalid" $ do
      hh <- createHydraHead (OpenState $ SimpleHead.mkState MaryTest.mkLedgerState) (cardanoLedger MaryTest.mkLedgerEnv)
      handleNextEvent mockNetwork mockChain mockClientSide hh (ClientEvent $ NewTx LedgerSpec.txInvalid)
        `shouldReturn` Just (LedgerError ValidationError)
      queryHeadState hh >>= flip shouldSatisfy isOpen

data MockTx = ValidTx | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = ()

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx -> Valid
        InvalidTx -> Invalid ValidationError
    , initLedgerState = ()
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
