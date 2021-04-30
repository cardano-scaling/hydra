{-# LANGUAGE TypeApplications #-}

module Hydra.ContractSpec where

import Cardano.Prelude
import qualified Control.Monad.Freer.Extras.Log as Trace
import qualified Data.Map.Strict as Map
import qualified Hydra.Contract.OffChain as OffChain
import qualified Hydra.Contract.OnChain as OnChain
import Hydra.Test.Utils (
  assertFinalState,
  callEndpoint,
  prettyUtxo,
  utxoOf,
  vk,
 )
import Ledger
import Ledger.Ada (lovelaceValueOf)
import Ledger.AddressMap (UtxoMap)
import Plutus.Contract (Contract)
import Plutus.Contract.Test (
  Wallet (..),
  assertFailedTransaction,
  assertNoFailedTransactions,
  checkPredicate,
  walletFundsChange,
  (.&&.),
 )
import qualified Plutus.Trace.Emulator as Trace
import PlutusTx.Monoid (inv)
import Test.Tasty (TestTree, testGroup)
import Wallet.Types (ContractError)
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
    "Hydra Scenarios"
    [ checkPredicate
        "✓ | Init > Commit > Commit > CollectCom"
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

          callEndpoint @"collectCom" aliceH (vk alice, txOutTxOut . snd <$> [aliceCommit, bobCommit])
    , checkPredicate
        "✓ | Init > Abort"
        ( assertNoFailedTransactions
            .&&. assertFinalState contract alice stateIsFinal
            .&&. walletFundsChange alice (lovelaceValueOf 0)
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          callEndpoint @"abort" aliceH (vk alice, [])
    , checkPredicate
        "✓ | Init > Commit > Abort"
        ( assertNoFailedTransactions
            .&&. assertFinalState contract alice stateIsFinal
            .&&. walletFundsChange alice (lovelaceValueOf 0)
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          utxoAlice <- selectOne <$> utxoOf alice
          callEndpoint @"commit" aliceH (vk alice, utxoAlice)
          callEndpoint @"abort" aliceH (vk alice, [txOutTxOut $ snd utxoAlice])
    , checkPredicate
        "x | Init > Commit > CollectCom"
        ( assertFailedTransaction (\_ _ _ -> True)
            .&&. assertFinalState contract alice stateIsInitial
            .&&. walletFundsChange alice (inv fixtureAmount)
        )
        $ do
          aliceH <- setupWallet alice
          callEndpoint @"init" aliceH ()
          utxoAlice <- selectOne <$> utxoOf alice
          callEndpoint @"commit" aliceH (vk alice, utxoAlice)
          callEndpoint @"collectCom" aliceH (vk alice, [])
    ]

fixtureAmount :: Value
fixtureAmount = lovelaceValueOf 1000

stateIsInitial :: OnChain.HydraState -> Bool
stateIsInitial = \case
  OnChain.Initial{} -> True
  _ -> False

stateIsOpen :: OnChain.HydraState -> Bool
stateIsOpen = \case
  OnChain.Open{} -> True
  _ -> False

hasTwoTxOuts :: OnChain.HydraState -> Bool
hasTwoTxOuts = \case
  OnChain.Open committedOutputs -> length committedOutputs == 2
  _ -> False

stateIsFinal :: OnChain.HydraState -> Bool
stateIsFinal = \case
  OnChain.Final{} -> True
  _ -> False

selectOne :: UtxoMap -> (TxOutRef, TxOutTx)
selectOne =
  Prelude.head
    . Map.toList
    . Map.filter ((== fixtureAmount) . txOutValue . txOutTxOut)

setupWallet ::
  Wallet ->
  Trace.EmulatorTrace (Trace.ContractHandle [OnChain.HydraState] OffChain.Schema ContractError)
setupWallet user = do
  h <- Trace.activateContractWallet user contract
  Trace.callEndpoint @"setupForTesting" h (vk user, replicate 10 fixtureAmount)
  void Trace.nextSlot
  return h
