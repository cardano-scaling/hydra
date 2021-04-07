{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractStateMachineSpec where

import Hydra.ContractStateMachine

import Cardano.Prelude
import Hydra.Contract.Types (HydraInput, HydraState (..), toDatumHash)
import Hydra.Utils (checkCompiledContractPIR, datumAtAddress)
import Ledger (Slot (Slot), ValidatorCtx)
import qualified Ledger.Ada as Ada
import Plutus.Contract hiding (runError)
import Plutus.Contract.StateMachine (SMContractError)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Test.Tasty

w1 :: Wallet
w1 = Wallet 1

theContract :: Contract () Schema SMContractError ()
theContract = contract

{- ORMOLU_DISABLE -}
compiledScript :: PlutusTx.CompiledCode (HydraState -> HydraInput -> ValidatorCtx -> Bool)
compiledScript = $$(PlutusTx.compile [|| validatorSM ||])
{- ORMOLU_ENABLE -}

tests :: TestTree
tests =
  testGroup
    "StateMachine Contract Behaviour"
    [ checkCompiledContractPIR "test/Hydra/ContractStateMachine.pir" compiledScript
    , checkPredicate
        "Expose 'collectCom' and 'close' endpoints"
        ( endpointAvailable @"collectCom" theContract (Trace.walletInstanceTag w1)
            .&&. endpointAvailable @"close" theContract (Trace.walletInstanceTag w1)
        )
        $ void (Trace.activateContractWallet w1 theContract)
    , checkPredicate
        "Close state after CollectCom"
        (assertNoFailedTransactions .&&. assertStateIsClosed)
        collectAndClose
    , checkPredicate
        "Closed state after init > collectCom > close"
        (assertNoFailedTransactions .&&. assertStateIsClosed)
        initCollectAndClose
    ]

assertState :: HydraState -> TracePredicate
assertState = datumAtAddress contractAddress . toDatumHash

assertStateIsClosed :: TracePredicate
assertStateIsClosed =
  datumAtAddress contractAddress (toDatumHash Closed)

collectAndClose :: Trace.EmulatorTrace ()
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

initCollectAndClose :: Trace.EmulatorTrace ()
initCollectAndClose = do
  contractHandle <- Trace.activateContractWallet w1 theContract
  Trace.callEndpoint @"init" contractHandle ()
  void $ Trace.waitUntilSlot (Slot 10)
  callCollectCom
  void $ Trace.waitUntilSlot (Slot 20)
  callClose
  void $ Trace.waitUntilSlot (Slot 30)
