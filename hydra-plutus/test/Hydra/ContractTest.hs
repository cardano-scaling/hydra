{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:defer-errors #-}

module Hydra.ContractTest where

import Hydra.Prelude

import Ledger

import Hydra.Test.Utils (
  assertFinalState,
  callEndpoint,
  prettyUtxo,
  utxoOf,
  vk,
 )
import Ledger.Ada (lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract (Contract)
import Plutus.Contract.Test (
  Wallet (..),
  assertContractError,
  assertNoFailedTransactions,
  checkPredicate,
  walletFundsChange,
  (.&&.),
 )
import Plutus.Trace.Emulator.Types (walletInstanceTag)
import PlutusTx.Monoid (inv)
import Test.Tasty (TestTree, testGroup)
import Wallet.Types (ContractError (..))

import qualified Control.Monad.Freer.Extras.Log as Trace
import qualified Data.Map.Strict as Map
import qualified Hydra.Depreciated.OffChain as OffChain
import qualified Hydra.Depreciated.OnChain as OnChain
import qualified Plutus.Trace.Emulator as Trace
import Test.Tasty.ExpectedFailure (expectFailBecause)
import qualified Prelude

--
-- Fixture
--

alice :: Wallet
alice = Wallet 1

bob :: Wallet
bob = Wallet 2

testPolicy :: MintingPolicy
testPolicy = OnChain.hydraMintingPolicy 42

testPolicyId :: MintingPolicyHash
testPolicyId = mintingPolicyHash testPolicy

contract :: Contract [OnChain.State] OffChain.Schema ContractError ()
contract = OffChain.contract headParameters
 where
  headParameters = OffChain.mkHeadParameters [vk alice, vk bob] testPolicy

--
-- Helpers
--

--
-- Test
--

tests :: TestTree
tests =
  expectFailBecause "We upgraded dependencies and things have changed " $
    testGroup
      "Hydra Scenarios"
      [ checkPredicate
          "Init > Commit > Commit > CollectCom: Can CollectCom when all parties have submitted"
          ( assertNoFailedTransactions
              .&&. assertFinalState contract alice stateIsOpen
              .&&. assertFinalState contract alice hasTwoTxOuts
              .&&. walletFundsChange alice (inv fixtureAmount)
              .&&. walletFundsChange bob (inv fixtureAmount)
          )
          $ do
            aliceH <- setupWallet alice
            bobH <- setupWallet bob

            callEndpoint @"init" aliceH ()

            utxoAlice <- utxoOf alice
            Trace.logInfo ("Alice's UTxO: " <> prettyUtxo utxoAlice)
            let aliceCommit = selectOne utxoAlice
            callEndpoint @"commit" aliceH (vk alice, aliceCommit)

            utxoBob <- utxoOf bob
            Trace.logInfo ("Bob's UTxO: " <> prettyUtxo utxoBob)
            let bobCommit = selectOne utxoBob
            callEndpoint @"commit" bobH (vk bob, bobCommit)

            callEndpoint @"collectCom" aliceH (vk alice, snd <$> [aliceCommit, bobCommit])
      , checkPredicate
          "Init > Abort: One can always abort before head is open"
          ( assertNoFailedTransactions
              .&&. assertFinalState contract alice stateIsFinal
              .&&. walletFundsChange alice (lovelaceValueOf 0)
          )
          $ do
            aliceH <- setupWallet alice
            callEndpoint @"init" aliceH ()
            callEndpoint @"abort" aliceH (vk alice, [])
      , checkPredicate
          "Init > Commit > Abort: One can always abort before head is open"
          ( assertNoFailedTransactions
              .&&. assertFinalState contract alice stateIsFinal
              .&&. walletFundsChange alice (lovelaceValueOf 0)
          )
          $ do
            aliceH <- setupWallet alice
            callEndpoint @"init" aliceH ()
            utxoAlice <- selectOne <$> utxoOf alice
            callEndpoint @"commit" aliceH (vk alice, utxoAlice)
            callEndpoint @"abort" aliceH (vk alice, [snd utxoAlice])
      , checkPredicate
          "Init > Commit > CollectCom: CollectCom is not allowed when not all parties have committed"
          ( assertFinalState contract alice stateIsInitial
              .&&. walletFundsChange alice (inv fixtureAmount)
              .&&. assertContractError
                contract
                (walletInstanceTag alice)
                ( \case
                    WalletError{} -> True
                    _ -> False
                )
                "expected collectCom to fail"
          )
          $ do
            aliceH <- setupWallet alice
            callEndpoint @"init" aliceH ()
            _ <- selectOne <$> utxoOf alice
            --callEndpoint @"commit" aliceH (vk alice, utxoAlice)
            callEndpoint @"collectCom" aliceH (vk alice, [])
      ]

fixtureAmount :: Value
fixtureAmount = lovelaceValueOf 1000

stateIsInitial :: OnChain.State -> Bool
stateIsInitial = \case
  OnChain.Initial{} -> True
  _ -> False

stateIsOpen :: OnChain.State -> Bool
stateIsOpen = \case
  OnChain.Open{} -> True
  _ -> False

hasTwoTxOuts :: OnChain.State -> Bool
hasTwoTxOuts = \case
  OnChain.Open committedOutputs -> length committedOutputs == 2
  _ -> False

stateIsFinal :: OnChain.State -> Bool
stateIsFinal = \case
  OnChain.Final{} -> True
  _ -> False

selectOne :: UtxoMap -> (TxOutRef, TxOut)
selectOne =
  Prelude.head
    . Map.toList
    . Map.filter ((== fixtureAmount) . txOutValue)
    . Map.map txOutTxOut

setupWallet ::
  Wallet ->
  Trace.EmulatorTrace (Trace.ContractHandle [OnChain.State] OffChain.Schema ContractError)
setupWallet user = do
  h <- Trace.activateContractWallet user contract
  Trace.callEndpoint @"setupForTesting" h (vk user, replicate 10 fixtureAmount)
  void Trace.nextSlot
  return h
