{-# LANGUAGE TypeApplications #-}

module Hydra.ContractSpec where

import Cardano.Prelude

import Ledger

import Data.String (String)
import Hydra.Test.Utils (prettyUtxo, utxoOf, vk)
import Plutus.Contract (Contract)
import Plutus.Trace.Emulator.Types (walletInstanceTag)
import Test.Tasty (TestTree, testGroup)
import Wallet.Types (ContractError)

import Plutus.Contract.Test (
  Wallet (..),
  assertAccumState,
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
        ( assertNoFailedTransactions
            .&&. assertAccumState contract (walletInstanceTag alice) stateIsOpen "state is Open"
        )
        fullScenarioSimple
    ]

stateIsOpen :: [OnChain.HydraState] -> Bool
stateIsOpen = \case
  [OnChain.Open{}] -> True
  _ -> False

fullScenarioSimple :: Trace.EmulatorTrace ()
fullScenarioSimple = do
  aliceH <- Trace.activateContractWallet alice contract
  bobH <- Trace.activateContractWallet bob contract

  Trace.callEndpoint @"init" aliceH ()
  void Trace.nextSlot

  utxoBob <- utxoOf bob
  Trace.logInfo ("Bob's UTxO: " <> show @_ @String utxoBob)
  Trace.callEndpoint @"commit" bobH (vk bob, commitFirst utxoBob)
  void Trace.nextSlot --
  utxoAlice <- utxoOf alice
  Trace.logInfo ("Alice's UTxO: " <> prettyUtxo utxoAlice)
  Trace.callEndpoint @"commit" aliceH (vk alice, commitFirst utxoAlice)
  void Trace.nextSlot

  Trace.callEndpoint @"collectCom" aliceH (vk alice)
  void Trace.nextSlot
 where
  -- Trace.callEndpoint @"close" aliceH (vk alice)
  -- void Trace.nextSlot

  commitFirst = Prelude.head . Map.toList
