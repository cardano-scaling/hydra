{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractStateMachineSpec where

import Hydra.ContractStateMachine

import Cardano.Prelude
import Hydra.Contract.Types (CollectingState (..), HeadParameters (..), HydraInput, HydraState (..), toDatumHash)
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.Utils (datumAtAddress)
import Ledger (PubKeyHash (..), Tx (..), TxOut, ValidatorCtx, txOutValue)
import qualified Ledger.Ada as Ada
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Value (flattenValue)
import Plutus.Contract hiding (runError)
import Plutus.Contract.StateMachine (SMContractError)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import qualified PlutusTx
import Test.Tasty

w1 :: Wallet
w1 = Wallet 1

w2 :: Wallet
w2 = Wallet 2

theContract :: Contract () Schema SMContractError ()
theContract = contract headParameters

pubKey1 :: PubKeyHash
pubKey1 = PubKeyHash "party1pubkeyhash"

pubKey2 :: PubKeyHash
pubKey2 = PubKeyHash "party2pubkeyhash"

headParameters :: HeadParameters
headParameters =
  HeadParameters
    { verificationKeys = [pubKey1, pubKey2]
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
        [ -- checkCompiledContractPIR "test/Hydra/ContractStateMachine.pir" compiledScript
          checkPredicate
            "Expose 'collectCom' and 'close' endpoints"
            ( endpointAvailable @"collectCom" theContract (Trace.walletInstanceTag w1)
                .&&. endpointAvailable @"close" theContract (Trace.walletInstanceTag w1)
            )
            $ void (Trace.activateContractWallet w1 theContract)
        , checkPredicate
            "Closed state after setup > init > collectCom > close"
            (assertNoFailedTransactions .&&. assertStateIsClosed)
            setupInitCollectAndClose
        , checkPredicate
            "Collecting holds all keys after init"
            (assertNoFailedTransactions .&&. assertState (Collecting $ CollectingState $ verificationKeys headParameters))
            $ do
              contractHandle <- Trace.activateContractWallet w1 theContract
              Trace.callEndpoint @"setup" contractHandle ()
              void $ Trace.nextSlot
              Trace.callEndpoint @"init" contractHandle ()
        , checkPredicate
            "Single commit is acknowledged"
            (assertNoFailedTransactions .&&. assertState (Collecting $ CollectingState [pubKey2]))
            $ do
              contractHandle <- Trace.activateContractWallet w1 theContract
              Trace.callEndpoint @"setup" contractHandle ()
              void $ Trace.nextSlot
              Trace.callEndpoint @"init" contractHandle ()
              void $ Trace.nextSlot
              Trace.callEndpoint @"commit" contractHandle pubKey1
        , checkPredicate
            "Committing from all parties is acknowledged"
            (assertNoFailedTransactions .&&. assertState (Collecting $ CollectingState []))
            $ do
              contractHandle <- Trace.activateContractWallet w1 theContract
              Trace.callEndpoint @"setup" contractHandle ()
              void $ Trace.nextSlot
              Trace.callEndpoint @"init" contractHandle ()
              void $ Trace.nextSlot
              Trace.callEndpoint @"commit" contractHandle pubKey1
              void $ Trace.nextSlot
              Trace.callEndpoint @"commit" contractHandle pubKey2
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
  void $ Trace.nextSlot
  callClose
  void $ Trace.nextSlot

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
  alice <- Trace.activateContractWallet w1 theContract
  bob <- Trace.activateContractWallet w2 theContract
  Trace.callEndpoint @"setup" alice ()
  void $ Trace.nextSlot
  Trace.callEndpoint @"init" alice ()
  void $ Trace.nextSlot
  Trace.callEndpoint @"commit" alice pubKey1
  void $ Trace.nextSlot
  Trace.callEndpoint @"commit" bob pubKey2
  void $ Trace.nextSlot
  Trace.callEndpoint @"collectCom" alice (CollectComParams $ Ada.lovelaceValueOf 42)
  void $ Trace.nextSlot
  Trace.callEndpoint @"close" alice ()
  void $ Trace.nextSlot
