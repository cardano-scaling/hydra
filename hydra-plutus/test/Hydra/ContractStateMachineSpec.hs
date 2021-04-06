{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractStateMachineSpec where

import Hydra.ContractStateMachine

import Cardano.Prelude
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.String (String)
import Hydra.Contract.Types (HydraState (Closed), toDatumHash)
import Hydra.Utils (datumAtAddress)
import Ledger (Slot (Slot))
import qualified Ledger.Ada as Ada
import Plutus.Contract hiding (runError)
import Plutus.Contract.StateMachine (SMContractError)
import Plutus.Contract.Test
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import qualified Plutus.Trace.Emulator as Trace
import Test.Tasty

w1 :: Wallet
w1 = Wallet 1

theContract :: Contract () Schema SMContractError ()
theContract = hydraHead

{- ORMOLU_DISABLE -}
-- compiledScript :: PlutusTx.CompiledCode (HydraState -> HydraInput -> ValidatorCtx -> Bool)
-- compiledScript = $$(PlutusTx.compile [|| validatorSM ||])
{- ORMOLU_ENABLE -}

tests :: TestTree
tests =
  testGroup
    "StateMachine Contract Behaviour"
    [ --checkCompiledContractPIR "test/Hydra/ContractStateMachine.pir" compiledScript
      checkPredicate
        "Expose 'collectCom' and 'close' endpoints"
        ( endpointAvailable @"collectCom" theContract (Trace.walletInstanceTag w1)
            .&&. endpointAvailable @"close" theContract (Trace.walletInstanceTag w1)
        )
        $ void (Trace.activateContractWallet w1 theContract)
    , checkPredicate
        "Close state after CollectCom"
        (assertNoFailedTransactions .&&. assertStateIsClosed)
        collectAndClose
    ]

assertStateIsClosed :: TracePredicate
assertStateIsClosed =
  datumAtAddress contractAddress (toDatumHash Closed)

collectAndClose :: Eff '[RunContract, Waiting, EmulatorControl, EmulatedWalletAPI, LogMsg String] ()
collectAndClose = do
  callCollectCom
  void $ Trace.waitUntilSlot (Slot 10)
  callClose
  void $ Trace.waitUntilSlot (Slot 20)

callCollectCom :: Trace.EmulatorTrace ()
callCollectCom = do
  contractHandle <- Trace.activateContractWallet w1 theContract
  Trace.callEndpoint @"collectCom" contractHandle (CollectComParams $ Ada.lovelaceValueOf 42)

callClose :: Trace.EmulatorTrace ()
callClose = do
  contractHandle <- Trace.activateContractWallet w1 theContract
  Trace.callEndpoint @"close" contractHandle ()
