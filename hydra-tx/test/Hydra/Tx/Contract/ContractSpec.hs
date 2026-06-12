{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- XXX: Move this one level up to avoid weird naming?
module Hydra.Tx.Contract.ContractSpec where

import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Cardano.Crypto.Util (SignableRepresentation (getSignableRepresentation))
import Cardano.Ledger.Alonzo.Plutus.TxInfo (TxOutSource (TxOutFromOutput))
import Cardano.Ledger.Babbage.TxInfo (transTxOutV2)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Cardano.Api (
  AssetId (AdaAssetId),
  CtxTx,
  Tx,
  TxOutDatum,
  UTxO,
  filterValue,
  minUTxOValue,
  mkTxOutDatumInline,
  modifyTxOutDatum,
  modifyTxOutValue,
  selectLovelace,
  serialiseToRawBytesHexText,
  toLedgerTxOut,
  toPlutusCurrencySymbol,
  toPlutusTxOut,
  toShelleyNetwork,
  txOutValue,
  txOuts',
 )
import Hydra.Cardano.Api.Pretty (renderTxWithUTxO)
import Hydra.Contract.Commit qualified as Commit
import Hydra.Contract.Head (verifySnapshotSignature)
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.Util qualified as OnChain
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  Snapshot (..),
  deriveParty,
  hashUTxO,
  headIdToCurrencySymbol,
  partyToChain,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.ContestationPeriod (toChain)
import Hydra.Tx.Contract.Close.CloseAny (genCloseAnyMutation, healthyCloseAnyTx)
import Hydra.Tx.Contract.Close.CloseCommitUnused (genCloseCommitUnusedMutation, healthyCloseCommitPendingTx)
import Hydra.Tx.Contract.Close.CloseCommitUsed (genCloseCommitUsedMutation, healthyCloseCommitAppliedTx)
import Hydra.Tx.Contract.Close.CloseInitial (genCloseInitialMutation, healthyCloseInitialTx)
import Hydra.Tx.Contract.Close.CloseUnused (genCloseCurrentMutation, healthyCloseCurrentTx)
import Hydra.Tx.Contract.Close.CloseUsed (genCloseOutdatedMutation, healthyCloseOutdatedTx)
import Hydra.Tx.Contract.Contest.ContestCurrent (genContestMutation)
import Hydra.Tx.Contract.Contest.ContestDec (genContestDecMutation)
import Hydra.Tx.Contract.Contest.ContestInc (genContestIncMutation, healthyContestIncTx)
import Hydra.Tx.Contract.Contest.Healthy (healthyContestTx)
import Hydra.Tx.Contract.Decrement (genDecrementMutation, healthyDecrementTx)
import Hydra.Tx.Contract.Deposit (genDepositMutation, genHealthyDepositTx)
import Hydra.Tx.Contract.FanOut (genFanoutMutation, healthyFanoutTx, healthyFanoutTxWithWalletChange)
import Hydra.Tx.Contract.FinalPartialFanout (genFinalPartialFanoutMutation, healthyFinalPartialFanoutTx)
import Hydra.Tx.Contract.Increment (genIncrementMutation, healthyIncrementTx)
import Hydra.Tx.Contract.Init (genInitMutation, healthyHeadParameters, healthyInitTx, healthyParticipants)
import Hydra.Tx.Contract.PartialFanout (genPartialFanoutMutation, healthyIntermediatePartialFanoutTx, healthyPartialFanoutTx, healthyPartialFanoutTxWithDuplicates)
import Hydra.Tx.Contract.Recover (genRecoverMutation, healthyRecoverTx)
import Hydra.Tx.Crypto (aggregate, sign, toPlutusSignatures)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Observe (observeDepositTx)
import PlutusLedgerApi.V3 (PubKeyHash (..), fromBuiltin, toBuiltin)
import Test.Hydra.Tx.Fixture (defaultPParams, testNetworkId, testPolicyId)
import Test.Hydra.Tx.Gen (
  genUTxOSized,
  genUTxOWithSimplifiedAddresses,
  propTransactionEvaluates,
  shrinkUTxO,
 )
import Test.Hydra.Tx.Mutation (SomeMutation (..), applyMutation, propMutation)
import Test.QuickCheck (
  Property,
  checkCoverage,
  counterexample,
  forAll,
  forAllBlind,
  forAllShrink,
  property,
  resize,
  shuffle,
  (=/=),
  (===),
  (==>),
 )
import Test.QuickCheck.Instances ()

spec :: Spec
spec = parallel $ do
  describe "Signature validator" $ do
    prop "verifies snapshot multi-signature" prop_verifySnapshotSignatures

  describe "TxOut hashing" $ do
    modifyMaxSuccess (const 20) $ do
      prop "hashUTxO == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentOnAndOffChainHashOfTxOuts
      prop "OnChain.hashPreSerializedCommits == OnChain.hashTxOuts (on sorted tx outs)" prop_consistentHashPreSerializedCommits
      prop "does care about ordering of TxOut" prop_hashingCaresAboutOrderingOfTxOuts

  describe "Serializing commits" $
    prop "deserializeCommit . serializeCommit === id" prop_serializingCommitRoundtrip

  describe "Init" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyInitTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyInitTx genInitMutation
    it "head output ADA covers worst-case min-UTxO through all contest rounds" $ do
      let (tx, _) = healthyInitTx
          headOut = fromJust $ txOuts' tx !!? 0
          n = length healthyParticipants
          HeadParameters{parties, contestationPeriod} = healthyHeadParameters
          worstCaseDatum :: TxOutDatum CtxTx
          worstCaseDatum =
            mkTxOutDatumInline $
              Head.Closed
                Head.ClosedDatum
                  { headId = toPlutusCurrencySymbol testPolicyId
                  , parties = map partyToChain parties
                  , contestationPeriod = toChain contestationPeriod
                  , version = 0
                  , snapshotNumber = 0
                  , contesters = replicate (n - 1) (PubKeyHash $ toBuiltin $ BS.replicate 28 0)
                  , contestationDeadline = 2_000_000_000_000
                  , accumulatorCommitment =
                      Accumulator.getAccumulatorCommitment $
                        Accumulator.buildFromSnapshotUTxOs @Tx mempty Nothing Nothing
                  , headAdaOverhead = 0
                  }
          -- minUTxOValue adds maxWord64 internally, so the base must have 0 lovelace
          zeroAdaHead = modifyTxOutValue (filterValue (/= AdaAssetId)) headOut
          worstCaseOut = modifyTxOutDatum (const worstCaseDatum) zeroAdaHead
          worstCaseMinCoin = selectLovelace $ minUTxOValue defaultPParams worstCaseOut
          headCoin = selectLovelace (txOutValue headOut)
      headCoin `shouldSatisfy` (>= worstCaseMinCoin)

  describe "Increment" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyIncrementTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyIncrementTx genIncrementMutation

  describe "Decrement" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyDecrementTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyDecrementTx genDecrementMutation

  describe "Deposit" $ do
    prop "healthy evaluates" $
      forAll genHealthyDepositTx propTransactionEvaluates
    prop "healthy observed" $
      forAll genHealthyDepositTx $ \(tx, _) ->
        isJust $ observeDepositTx testNetworkId tx
    prop "mutated not observed" $
      forAll genHealthyDepositTx $ \(tx, utxo) ->
        forAll (genDepositMutation (tx, utxo)) $ \SomeMutation{label, mutation} -> do
          let (tx', utxo') = (tx, utxo) & applyMutation mutation
          counterexample ("Mutated transaction: " <> renderTxWithUTxO utxo' tx') $
            property (isNothing $ observeDepositTx testNetworkId tx')
              & counterexample "Mutated transaction still observed"
              & genericCoverTable [label]
              & checkCoverage

  describe "Recover" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyRecoverTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyRecoverTx genRecoverMutation

  describe "CloseInitial" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseInitialTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseInitialTx genCloseInitialMutation
  describe "CloseUnused" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseCurrentTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseCurrentTx genCloseCurrentMutation
  describe "CloseUsed" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseOutdatedTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseOutdatedTx genCloseOutdatedMutation
  describe "CloseCommitUnused" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseCommitPendingTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseCommitPendingTx genCloseCommitUnusedMutation
  describe "CloseCommitUsed" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseCommitAppliedTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseCommitAppliedTx genCloseCommitUsedMutation
  describe "CloseAny" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyCloseAnyTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyCloseAnyTx genCloseAnyMutation

  describe "ContestUnused" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyContestTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyContestTx genContestMutation
  describe "ContestUsed" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyContestTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyContestTx genContestDecMutation
  describe "ContestCommit" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyContestIncTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyContestIncTx genContestIncMutation

  describe "Fanout" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyFanoutTx
    prop "accepts trailing wallet change output" $
      propTransactionEvaluates healthyFanoutTxWithWalletChange
    prop "does not survive random adversarial mutations" $
      propMutation healthyFanoutTx genFanoutMutation
  describe "PartialFanout" $ do
    prop "is healthy (from Closed)" $
      propTransactionEvaluates healthyPartialFanoutTx
    prop "is healthy (from FanoutProgress)" $
      propTransactionEvaluates healthyIntermediatePartialFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyPartialFanoutTx genPartialFanoutMutation
    prop "evaluates with duplicate UTxO outputs" $
      -- Two distinct UTxOs with identical TxOut content (same address + value) must
      -- be fanned out correctly. Currently fails due to accumulator deduplication.
      propTransactionEvaluates healthyPartialFanoutTxWithDuplicates
  describe "FinalPartialFanout" $ do
    prop "is healthy" $
      propTransactionEvaluates healthyFinalPartialFanoutTx
    prop "does not survive random adversarial mutations" $
      propMutation healthyFinalPartialFanoutTx genFinalPartialFanoutMutation

--
-- Properties
--

prop_serializingCommitRoundtrip :: Property
prop_serializingCommitRoundtrip =
  forAllBlind (List.head . UTxO.toList <$> genUTxOSized 1) $ \singleUTxO ->
    let serialized = Commit.serializeCommit singleUTxO
        deserialized = serialized >>= Commit.deserializeCommit (toShelleyNetwork testNetworkId)
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
        txOuts = map snd . sortOn fst $ UTxO.toList utxo
     in (hashUTxO @Tx utxo === fromBuiltin (OnChain.hashTxOuts plutusTxOuts))
          & counterexample ("Plutus: " <> show plutusTxOuts)
          & counterexample ("Ledger: " <> show txOuts)

prop_consistentHashPreSerializedCommits :: Property
prop_consistentHashPreSerializedCommits =
  forAllShrink genUTxOWithSimplifiedAddresses shrinkUTxO $ \(utxo :: UTxO) ->
    let unsortedUTxOPairs = UTxO.toList utxo
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
    (UTxO.size utxo > 1) ==>
      let plutusTxOuts =
            rights $
              zipWith
                (\ix o -> transTxOutV2 (TxOutFromOutput $ Ledger.TxIx ix) $ toLedgerTxOut o)
                [0 ..]
                txOuts
          txOuts = snd <$> UTxO.toList utxo
       in forAll (shuffle plutusTxOuts) $ \shuffledTxOuts ->
            (shuffledTxOuts /= plutusTxOuts) ==>
              let hashed = OnChain.hashTxOuts plutusTxOuts
                  hashShuffled = OnChain.hashTxOuts shuffledTxOuts
               in (hashed =/= hashShuffled)
                    & counterexample ("Plutus: " <> show plutusTxOuts)
                    & counterexample ("Shuffled: " <> show shuffledTxOuts)

prop_verifySnapshotSignatures :: Property
prop_verifySnapshotSignatures =
  forAll arbitrary $ \(snapshot@Snapshot{headId, number, version, accumulator} :: Snapshot Tx) ->
    forAll (resize 3 arbitrary) $ \sks ->
      let parties = deriveParty <$> sks
          onChainParties = partyToChain <$> parties
          signatures = toPlutusSignatures $ aggregate [sign sk snapshot | sk <- sks]
          snapshotNumber = toInteger number
          snapshotVersion = toInteger version
       in verifySnapshotSignature onChainParties (headIdToCurrencySymbol headId, snapshotVersion, snapshotNumber, toBuiltin (Accumulator.getAccumulatorHash accumulator)) signatures
            & counterexample ("headId: " <> toString (serialiseToRawBytesHexText headId))
            & counterexample ("version: " <> show snapshotVersion)
            & counterexample ("number: " <> show snapshotNumber)
            & counterexample ("off-chain message: " <> show (Base16.encode $ getSignableRepresentation snapshot))
            & counterexample ("signatures: " <> show signatures)
