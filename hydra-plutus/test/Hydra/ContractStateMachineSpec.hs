{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractStateMachineSpec where

import Hydra.ContractStateMachine

import Cardano.Prelude
import Hydra.Contract.Types (HeadParameters (..), HydraInput, HydraState (..), toDatumHash)
import Hydra.Utils (checkCompiledContractPIR, datumAtAddress)
import Ledger (PubKeyHash (..), Slot (Slot), Tx (..), TxOut, ValidatorCtx, txOutValue)
import qualified Ledger.Ada as Ada
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Value (flattenValue)
import Plutus.Contract hiding (runError)
import Plutus.Contract.StateMachine (SMContractError)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Test.Tasty
import Hydra.MonetaryPolicy(hydraCurrencySymbol)

w1 :: Wallet
w1 = Wallet 1

theContract :: Contract () Schema SMContractError ()
theContract = contract headParameters

headParameters :: HeadParameters
headParameters =
  HeadParameters
    { verificationKeys = [PubKeyHash "party1pubkeyhash", PubKeyHash "party2pubkeyhash"]
    , currencyId = hydraCurrencySymbol 14
    }

{- ORMOLU_DISABLE -}
compiledScript :: PlutusTx.CompiledCode (HydraState -> HydraInput -> ValidatorCtx -> Bool)
compiledScript = $$(PlutusTx.compile [|| validatorSM ||])
{- ORMOLU_ENABLE -}

tests :: TestTree
tests =
  testGroup
    "Contract StateMachine"
    [ testGroup
        "StateMachine Contract Behaviour"
        [ checkCompiledContractPIR "test/Hydra/ContractStateMachine.pir" compiledScript
        , checkPredicate
            "Expose 'collectCom' and 'close' endpoints"
            ( endpointAvailable @"collectCom" theContract (Trace.walletInstanceTag w1)
                .&&. endpointAvailable @"close" theContract (Trace.walletInstanceTag w1)
            )
            $ void (Trace.activateContractWallet w1 theContract)
        , checkPredicate
            "Closed state after setup > init > collectCom > close"
            (assertNoFailedTransactions .&&. assertStateIsClosed)
            setupInitCollectAndClose
        ]
    , testGroup
        "Init Transaction has the right shape"
        [ checkPredicate
            "has right number of outputs"
            (tx theContract (Trace.walletInstanceTag w1) (assertInitTxShape headParameters) "right shape")
            ( do
                contractHandle <- Trace.activateContractWallet w1 theContract
                Trace.callEndpoint @"setup" contractHandle ()
                Trace.callEndpoint @"init" contractHandle ()
            )
        ]
    ]

assertState :: HydraState -> TracePredicate
assertState = datumAtAddress contractAddress . toDatumHash

assertStateIsClosed :: TracePredicate
assertStateIsClosed =
  datumAtAddress contractAddress (toDatumHash Closed)

assertInitTxShape :: HeadParameters -> UnbalancedTx -> Bool
assertInitTxShape HeadParameters{verificationKeys} unbalancedTx =
  let outputs = txOutputs (unBalancedTxTx unbalancedTx)
      numberOfParticipants = length verificationKeys
   in length outputs == numberOfParticipants + 1
        && participationTokensAreUnique outputs numberOfParticipants

participationTokensAreUnique :: [TxOut] -> Int -> Bool
participationTokensAreUnique outputs numberOfParticipants =
  length (filter (hasParticipationToken 1) outputs) == numberOfParticipants

--    && Set.size (assetNames policyId tx) == numberOfParticipants

hasParticipationToken :: Int -> TxOut -> Bool
hasParticipationToken numberOfTokens txOut =
  let tokens = filter (\(cur, _, _) -> cur /= Ada.adaSymbol) $ flattenValue $ txOutValue txOut
   in length tokens == numberOfTokens

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

setupInitCollectAndClose :: Trace.EmulatorTrace ()
setupInitCollectAndClose = do
  contractHandle <- Trace.activateContractWallet w1 theContract
  Trace.callEndpoint @"setup" contractHandle ()
  void $ Trace.waitUntilSlot (Slot 10)
  Trace.callEndpoint @"init" contractHandle ()
  void $ Trace.waitUntilSlot (Slot 20)
  Trace.callEndpoint @"collectCom" contractHandle (CollectComParams $ Ada.lovelaceValueOf 42)
  void $ Trace.waitUntilSlot (Slot 30)
  Trace.callEndpoint @"close" contractHandle ()
  void $ Trace.waitUntilSlot (Slot 40)
