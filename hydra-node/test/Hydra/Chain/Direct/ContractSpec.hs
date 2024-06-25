{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.Chain.Direct.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (TxOutFromOutput))
import Cardano.Ledger.Babbage.TxInfo (transTxOutV2)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Hydra.Cardano.Api (
  UTxO,
  toLedgerTxOut,
  toPlutusTxOut,
 )
import Hydra.Cardano.Api.Network (networkIdToNetwork)
import Hydra.Chain.Direct.Contract.Abort (genAbortMutation, healthyAbortTx, propHasCommit, propHasInitial)
import Hydra.Chain.Direct.Contract.Close.CloseCurrent (genCloseCurrentMutation, healthyCloseCurrentTx)
import Hydra.Chain.Direct.Contract.Close.CloseInitial (genCloseInitialMutation, healthyCloseInitialTx)
import Hydra.Chain.Direct.Contract.Close.CloseOutdated (genCloseOutdatedMutation, healthyCloseOutdatedTx)
import Hydra.Chain.Direct.Contract.CollectCom (genCollectComMutation, healthyCollectComTx)
import Hydra.Chain.Direct.Contract.Commit (genCommitMutation, healthyCommitTx)
import Hydra.Chain.Direct.Contract.Contest (genContestMutation, healthyContestTx)
import Hydra.Chain.Direct.Contract.Decrement (genDecrementMutation, healthyDecrementTx)
import Hydra.Chain.Direct.Contract.FanOut (genFanoutMutation, healthyFanoutTx)
import Hydra.Chain.Direct.Contract.Init (genInitMutation, healthyInitTx)
import Hydra.Chain.Direct.Contract.Mutation (propMutation)
import Hydra.Chain.Direct.Fixture (testNetworkId)
import Hydra.Chain.Direct.Tx (headIdToCurrencySymbol)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head (
  verifyPartySignature,
  verifySnapshotSignature,
 )
import Hydra.Contract.Head qualified as OnChain
import Hydra.Crypto (aggregate, generateSigningKey, sign, toPlutusSignatures)
import Hydra.Ledger (hashUTxO)
import Hydra.Ledger qualified as OffChain
import Hydra.Ledger.Cardano (
  Tx,
  genUTxOSized,
  genUTxOWithSimplifiedAddresses,
  shrinkUTxO,
 )
import Hydra.Ledger.Cardano.Evaluate (propTransactionEvaluates)
import Hydra.Ledger.Simple (SimpleTx)
import Hydra.Party (deriveParty, partyToChain)
import Hydra.Plutus.Orphans ()
import Hydra.Snapshot (Snapshot (..))
import PlutusLedgerApi.V2 (fromBuiltin, toBuiltin)
import Test.QuickCheck (
  Property,
  conjoin,
  counterexample,
  forAll,
  forAllBlind,
  forAllShrink,
  property,
  shuffle,
  (=/=),
  (===),
  (==>),
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
      prop "OffChain.hashUTxO == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentOnAndOffChainHashOfTxOuts
      prop "OnChain.hashPreSerializedCommits == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentHashPreSerializedCommits
      prop "does care about ordering of TxOut" prop_hashingCaresAboutOrderingOfTxOuts

  describe "Serializing commits" $
    prop "deserializeCommit . serializeCommit === id" prop_serializingCommitRoundtrip

  describe "Init" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyInitTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyInitTx genInitMutation

  describe "Abort" $ do
    prop "is healthy" $
      conjoin
        [ propTransactionEvaluates healthyAbortTx
        , propHasCommit healthyAbortTx
        , propHasInitial healthyAbortTx
        ]
    prop "does not survive random adversarial mutations" $
      propMutation healthyAbortTx genAbortMutation
  describe "Commit" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCommitTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCommitTx genCommitMutation
  describe "CollectCom" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCollectComTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCollectComTx genCollectComMutation
  describe "Decrement" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyDecrementTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyDecrementTx genDecrementMutation
  describe "CloseInitial" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseInitialTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseInitialTx genCloseInitialMutation
  describe "CloseCurrent" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseCurrentTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseCurrentTx genCloseCurrentMutation
  describe "CloseOutdated" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseOutdatedTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseOutdatedTx genCloseOutdatedMutation
  describe "Contest" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyContestTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyContestTx genContestMutation
  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyFanoutTx genFanoutMutation

--
-- Properties
--

prop_serializingCommitRoundtrip :: Property
prop_serializingCommitRoundtrip =
  forAllBlind (List.head . UTxO.pairs <$> genUTxOSized 1) $ \singleUTxO ->
    let serialized = Commit.serializeCommit singleUTxO
        deserialized = serialized >>= Commit.deserializeCommit (networkIdToNetwork testNetworkId)
     in case deserialized of
          Just actual -> actual === singleUTxO
          Nothing ->
            property False
              & counterexample "roundtrip returned Nothing"
              & counterexample ("Serialized: " <> show serialized)
              & counterexample ("Deserialized: " <> show deserialized)

prop_consistentOnAndOffChainHashOfTxOuts :: Property
prop_consistentOnAndOffChainHashOfTxOuts =
  -- NOTE: We only generate shelley addressed txouts because they are left out
  -- of the plutus script context in 'txInfoOut'.
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let plutusTxOuts =
          rights $
            zipWith
              (\ix o -> transTxOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
              [0 ..]
              txOuts
        txOuts = map snd . sortOn fst $ UTxO.pairs utxo
     in (OffChain.hashUTxO @Tx utxo === fromBuiltin (OnChain.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show txOuts)

prop_consistentHashPreSerializedCommits :: Property
prop_consistentHashPreSerializedCommits =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let unsortedUTxOPairs = UTxO.pairs utxo
        toFanoutTxOuts = mapMaybe (toPlutusTxOut . snd) $ sortOn fst unsortedUTxOPairs
        serializedCommits = mapMaybe Commit.serializeCommit unsortedUTxOPairs
        hashedCommits = OnChain.hashPreSerializedCommits serializedCommits
        hashedTxOuts = OnChain.hashTxOuts toFanoutTxOuts
     in hashedCommits
          === hashedTxOuts
          & counterexample ("Hashed commits: " <> decodeUtf8 (Base16.encode $ fromBuiltin hashedCommits))
          & counterexample ("Hashed txOuts: " <> decodeUtf8 (Base16.encode $ fromBuiltin hashedTxOuts))
          & counterexample ("Serialized commits: " <> show serializedCommits)
          & counterexample ("To fanout txOuts: " <> show toFanoutTxOuts)

prop_hashingCaresAboutOrderingOfTxOuts :: Property
prop_hashingCaresAboutOrderingOfTxOuts =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    (length utxo > 1) ==>
      let plutusTxOuts =
            rights $
              zipWith
                (\ix o -> transTxOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
                [0 ..]
                txOuts
          txOuts = snd <$> UTxO.pairs utxo
       in forAll (shuffle plutusTxOuts) $ \shuffledTxOuts ->
            (shuffledTxOuts /= plutusTxOuts) ==>
              let hashed = OnChain.hashTxOuts plutusTxOuts
                  hashShuffled = OnChain.hashTxOuts shuffledTxOuts
               in (hashed =/= hashShuffled)
                    & counterexample ("Plutus: " <> show plutusTxOuts)
                    & counterexample ("Shuffled: " <> show shuffledTxOuts)

prop_verifyOffChainSignatures :: Property
prop_verifyOffChainSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{headId, number, utxo, utxoToDecommit, version} :: Snapshot SimpleTx) ->
    forAll arbitrary $ \seed ->
      let sk = generateSigningKey seed
          offChainSig = sign sk snapshot
          onChainSig = List.head . toPlutusSignatures $ aggregate [offChainSig]
          onChainParty = partyToChain $ deriveParty sk
          snapshotNumber = toInteger number
          snapshotVersion = toInteger version
          utxoHash = (toBuiltin $ hashUTxO @SimpleTx utxo)
          utxoToDecommitHash = (toBuiltin . hashUTxO @SimpleTx $ fromMaybe mempty utxoToDecommit)
       in verifyPartySignature (headIdToCurrencySymbol headId) snapshotNumber utxoHash utxoToDecommitHash snapshotVersion onChainParty onChainSig
            & counterexample ("headId: " <> show headId)
            & counterexample ("signed: " <> show onChainSig)
            & counterexample ("party: " <> show onChainParty)
            & counterexample ("utxoHash: " <> show utxoHash)
            & counterexample ("version: " <> show version)
            & counterexample ("message: " <> show (getSignableRepresentation snapshot))

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{headId, number, utxo, utxoToDecommit, version} :: Snapshot SimpleTx) ->
    forAll arbitrary $ \sks ->
      let parties = deriveParty <$> sks
          onChainParties = partyToChain <$> parties
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger number
          snapshotVersion = toInteger version
          utxoHash = toBuiltin (hashUTxO @SimpleTx utxo)
          utxoToDecommitHash = (toBuiltin . hashUTxO @SimpleTx $ fromMaybe mempty utxoToDecommit)
       in verifySnapshotSignature onChainParties (headIdToCurrencySymbol headId) snapshotNumber utxoHash utxoToDecommitHash snapshotVersion signatures
