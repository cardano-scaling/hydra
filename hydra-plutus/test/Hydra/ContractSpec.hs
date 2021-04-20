module Hydra.ContractSpec where

import Cardano.Prelude

import Ledger

import Ledger.AddressMap (UtxoMap, fundsAt)
import Wallet.Types(ContractError)
import Wallet.Emulator.Chain(chainNewestFirst)
import Wallet.Emulator.Wallet(walletAddress)
import Wallet.Emulator.MultiAgent (emulatorState, chainUtxo)
import Control.Lens(view, (^.))
import Test.Tasty(TestTree, testGroup)
import Plutus.Contract (Contract)
import Hydra.Utils (datumAtAddress)
import Data.String (String)

import Plutus.Contract.Test (
  Wallet(..),
  (.&&.),
  TracePredicate,
  walletPubKey,
  assertNoFailedTransactions,
  checkPredicate
 )

import qualified Hydra.Contract.OnChain as OnChain
import qualified Hydra.Contract.OffChain as OffChain

import qualified Plutus.Trace.Emulator as Trace
import qualified Control.Monad.Freer.Extras.Log as Trace

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

contract :: Contract () OffChain.Schema ContractError ()
contract = OffChain.contract testPolicy

--
-- Helpers
--

vk :: Wallet -> PubKeyHash
vk = pubKeyHash . walletPubKey

utxoOf :: Wallet -> Trace.EmulatorTrace UtxoMap
utxoOf w = do
  st <- emulatorState . view chainNewestFirst <$> Trace.chainState
  return $ st ^. chainUtxo . fundsAt (walletAddress w)

--
-- Test
--

tests :: TestTree
tests =
  testGroup
    "Simple Scenario"
    [ checkPredicate
        "Init > Commit > Commit"
        (assertNoFailedTransactions .&&. assertStateIs OnChain.Open)
        fullScenarioSimple
    ]

assertStateIs :: OnChain.HydraState -> TracePredicate
assertStateIs =
  datumAtAddress (OnChain.νHydraAddress testPolicyId)

fullScenarioSimple :: Trace.EmulatorTrace ()
fullScenarioSimple = do
  aliceH <- Trace.activateContractWallet alice contract
  bobH <- Trace.activateContractWallet bob contract

  Trace.callEndpoint @"init" aliceH [vk alice, vk bob]
  void Trace.nextSlot

  utxoAlice <- utxoOf alice
  Trace.logInfo ("Alice's UTxO: " <> show @_ @String utxoAlice)
  Trace.callEndpoint @"commit" aliceH (vk alice, utxoAlice)
  void Trace.nextSlot

  -- utxoBob <- utxoOf bob
  -- Trace.logInfo ("Bob's UTxO: " <> show @_ @String utxoBob)
  -- Trace.callEndpoint @"commit" bobH (vk bob, utxoBob)
  -- void Trace.nextSlot                              --

  -- Trace.callEndpoint @"collectCom" alice ()
  -- void Trace.nextSlot
  -- Trace.callEndpoint @"close" alice ()
  -- void Trace.nextSlot
