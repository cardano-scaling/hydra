{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import qualified Cardano.Api.UTxO as UTxO
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Cardano.Ledger.Alonzo.TxInfo (TxOutSource (TxOutFromOutput))
import Cardano.Ledger.Babbage.TxInfo (txInfoOutV2)
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import Hydra.Cardano.Api (
  UTxO,
  toLedgerTxOut,
 )
import Hydra.Chain.Direct.Contract.Abort (genAbortMutation, healthyAbortTx, propHasCommit, propHasInitial)
import Hydra.Chain.Direct.Contract.Close (genCloseMutation, healthyCloseTx)
import Hydra.Chain.Direct.Contract.CollectCom (genCollectComMutation, healthyCollectComTx)
import Hydra.Chain.Direct.Contract.Commit (genCommitMutation, healthyCommitTx)
import Hydra.Chain.Direct.Contract.Contest (genContestMutation, healthyContestTx)
import Hydra.Chain.Direct.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Chain.Direct.Contract.Init (genHealthyIdleSt, genInitMutation, genObserveInitMutation, healthyInitTx)
import Hydra.Chain.Direct.Contract.Mutation (
  propMutationOffChain,
  propMutationOnChain,
  propTransactionValidates,
 )
import Hydra.Chain.Direct.State (SomeOnChainHeadState (..))
import qualified Hydra.Contract.Commit as Commit
import Hydra.Contract.Head (
  verifyPartySignature,
  verifySnapshotSignature,
 )
import qualified Hydra.Contract.Head as OnChain
import Hydra.Crypto (aggregate, sign, toPlutusSignatures)
import qualified Hydra.Crypto as Hydra
import Hydra.Ledger (hashUTxO)
import qualified Hydra.Ledger as OffChain
import Hydra.Ledger.Cardano (
  Tx,
  genUTxOWithSimplifiedAddresses,
  shrinkUTxO,
 )
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (deriveParty, partyToChain)
import Hydra.Snapshot (Snapshot (..))
import Plutus.Orphans ()
import Plutus.V2.Ledger.Api (fromBuiltin, toBuiltin)
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  forAll,
  forAllShrink,
  (===),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = parallel $ do
  describe "Signature validator" $ do
    prop
      "verifies single signature produced off-chain"
      prop_verifyOffChainSignatures
    -- FIXME(AB): This property exists solely because our current multisignature implementation
    -- is just the aggregates of individual (mock) signatures and there is no point in doing some
    -- complicated shuffle logic to verify signatures given we'll end up verifying a single Ed25519
    -- signatures.
    prop
      "verifies snapshot multi-signature for list of parties and signatures"
      prop_verifySnapshotSignatures
  describe "TxOut hashing" $ do
    modifyMaxSuccess (const 20) $ do
      prop "OffChain.hashUTxO == OnChain.hashTxOuts" prop_consistentOnAndOffChainHashOfTxOuts
      prop "serialize' commutes with hashing" prop_serialiseTxOutCommutesWithHash

  describe "Init" $ do
    prop "is healthy" $
      propTransactionValidates healthyInitTx
    prop "does not survive random adversarial mutations (on-chain)" $
      propMutationOnChain healthyInitTx genInitMutation
    prop "does not survive random adversarial mutations (off-chain)" $
      propMutationOffChain healthyInitTx genObserveInitMutation (SomeOnChainHeadState <$> genHealthyIdleSt)

  describe "Abort" $ do
    prop "is healthy" $
      conjoin
        [ propTransactionValidates healthyAbortTx
        , propHasCommit healthyAbortTx
        , propHasInitial healthyAbortTx
        ]
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyAbortTx genAbortMutation
  describe "Commit" $ do
    prop "is healthy" $
      propTransactionValidates healthyCommitTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCommitTx genCommitMutation
  describe "CollectCom" $ do
    prop "is healthy" $
      propTransactionValidates healthyCollectComTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCollectComTx genCollectComMutation
  describe "Close" $ do
    prop "is healthy" $
      propTransactionValidates healthyCloseTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyCloseTx genCloseMutation
  describe "Contest" $ do
    prop "is healthy" $
      propTransactionValidates healthyContestTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyContestTx genContestMutation
  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionValidates healthyFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutationOnChain healthyFanoutTx genFanoutMutation

--
-- Properties
--

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let plutusTxOuts =
          rights $
            zipWith
              (\ix o -> txInfoOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
              [0 ..]
              txOuts
        txOuts = toList utxo
     in (OffChain.hashUTxO @Tx utxo === fromBuiltin (OnChain.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show txOuts)

prop_serialiseTxOutCommutesWithHash :: Property
prop_serialiseTxOutCommutesWithHash =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let serializedCommits = mapMaybe Commit.serializeCommit $ UTxO.pairs utxo
        serialisedUTxO = OnChain.hashPreSerializedCommits serializedCommits
     in fromBuiltin serialisedUTxO === hashUTxO @Tx utxo
          & counterexample ("Hashed CBOR: " <> decodeUtf8 (Base16.encode $ fromBuiltin serialisedUTxO))

prop_verifyOffChainSignatures :: Property
prop_verifyOffChainSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{number, utxo} :: Snapshot SimpleTx) ->
    forAll arbitrary $ \seed ->
      let sk = Hydra.generateSigningKey seed
          offChainSig = Hydra.sign sk snapshot
          onChainSig = List.head . toPlutusSignatures $ aggregate [offChainSig]
          onChainParty = partyToChain $ deriveParty sk
          snapshotNumber = toInteger number
          utxoHash = toBuiltin $ hashUTxO @SimpleTx utxo
       in verifyPartySignature snapshotNumber utxoHash onChainParty onChainSig
            & counterexample ("signed: " <> show onChainSig)
            & counterexample ("party: " <> show onChainParty)
            & counterexample ("message: " <> show (getSignableRepresentation snapshot))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{number, utxo} :: Snapshot SimpleTx) ->
    forAll arbitrary $ \sks ->
      let parties = deriveParty <$> sks
          onChainParties = partyToChain <$> parties
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger number
          utxoHash = toBuiltin $ hashUTxO @SimpleTx utxo
       in verifySnapshotSignature onChainParties snapshotNumber utxoHash signatures
