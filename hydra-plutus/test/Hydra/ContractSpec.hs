{-# LANGUAGE TypeApplications #-}

module Hydra.ContractSpec where

import Cardano.Prelude

import Ledger

import Hydra.Test.Utils (assertFinalState, prettyUtxo, utxoOf, vk)
import Plutus.Contract (Contract)
import Test.Tasty (TestTree, testGroup)
import Wallet.Types (ContractError)

import Plutus.Contract.Test (
  Wallet (..),
  assertNoFailedTransactions,
  checkPredicate,
  (.&&.),
 )

import qualified Control.Monad.Freer.Extras.Log as Trace
import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OffChain as OffChain
import qualified Hydra.Contract.OnChain as OnChain
import qualified Plutus.Trace.Emulator as Trace
import qualified Prelude

--
-- Fixture
--

alice :: Wallet
alice = Wallet 1

bob :: Wallet
bob = Wallet 2

testPolicy :: MonetaryPolicy
testPolicy = OnChain.hydraMonetaryPolicy 42

testPolicyId :: MonetaryPolicyHash
testPolicyId = monetaryPolicyHash testPolicy

contract :: Contract [OnChain.HydraState] OffChain.Schema ContractError ()
contract = OffChain.contract testPolicy [vk alice, vk bob]

--
-- Helpers
--

--
-- Test
--

tests :: TestTree
tests =
  testGroup
    "Simple Scenario"
    [ checkPredicate
        "Init > Commit > Commit > CollectCom"
        ( assertNoFailedTransactions .&&. assertFinalState contract alice stateIsOpen
        )
        fullScenarioSimple
    ]

stateIsOpen :: OnChain.HydraState -> Bool
stateIsOpen = \case
  OnChain.Open{} -> True
  _ -> False

fullScenarioSimple :: Trace.EmulatorTrace ()
fullScenarioSimple = do
  aliceH <- setupWallet alice
  bobH <- setupWallet bob

  Trace.callEndpoint @"init" aliceH ()
  void Trace.nextSlot

  utxoAlice <- utxoOf alice
  Trace.logInfo ("Alice's UTxO: " <> prettyUtxo utxoAlice)
  Trace.callEndpoint @"commit" aliceH (vk alice, selectOne utxoAlice)
  void Trace.nextSlot

  utxoBob <- utxoOf bob
  Trace.logInfo ("Bob's UTxO: " <> prettyUtxo utxoBob)
  Trace.callEndpoint @"commit" bobH (vk bob, selectOne utxoBob)
  void Trace.nextSlot

  Trace.callEndpoint @"collectCom" aliceH (vk alice)
  void Trace.nextSlot
 where
  selectOne = Prelude.last . Map.toList

  setupWallet ::
    Wallet ->
    Trace.EmulatorTrace (Trace.ContractHandle [OnChain.HydraState] OffChain.Schema ContractError)
  setupWallet user = do
    h <- Trace.activateContractWallet user contract
    Trace.callEndpoint @"setupForTesting" h (vk user)
    void Trace.nextSlot
    return h
