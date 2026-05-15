{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.PartialFanout where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.List qualified as List
import Data.Maybe (fromJust)
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (HeadValueIsNotPreserved, LowerBoundBeforeContestationDeadline, PartialFanoutChangedParameters, PartialFanoutMembershipFailed))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Gen ()
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Fanout (partialFanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.KZGTrustedSetup (fanoutChunkSize, fanoutOutputThreshold)
import Hydra.Tx.Party (Party, partyToChain, vkey)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime, toBuiltin)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genAddressInEra, genForParty, genScriptRegistryWithCRSSize, genTxOut, genUTxOWithSimplifiedAddresses, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens, modifyInlineDatum, replaceAccumulatorCommitment, replaceContestationDeadline, replaceHeadId, replaceParties)
import Test.QuickCheck (choose, elements, oneof, resize, suchThat, vectorOf)
import Test.QuickCheck.Instances ()

-- | Build a healthy partial fanout transaction with a given input head state.
-- Used for both the Closed → FanoutProgress and FanoutProgress → FanoutProgress cases,
-- which share identical transaction structure and only differ in the input datum type.
healthyPartialFanoutTxWith :: Head.State -> (Tx, UTxO)
healthyPartialFanoutTxWith inputState = (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    partialFanoutTx
      scriptRegistry
      healthyDistributeUTxO
      (headInput, headOutput)
      healthySlotNo
      healthyProgressDatum
      remainingAccumulator

  scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline inputState)

  headOutput = modifyTxOutValue (<> participationTokens <> UTxO.totalValue healthyFullUTxO) headOutput'

  participationTokens =
    fromList $
      map
        (\party -> (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1))
        healthyParties

-- | Closed → FanoutProgress partial fanout.
healthyPartialFanoutTx :: (Tx, UTxO)
healthyPartialFanoutTx = healthyPartialFanoutTxWith (Head.Closed healthyClosedDatum)

-- | FanoutProgress → FanoutProgress partial fanout.
healthyIntermediatePartialFanoutTx :: (Tx, UTxO)
healthyIntermediatePartialFanoutTx = healthyPartialFanoutTxWith (Head.FanoutProgress healthyProgressDatum)

-- | The full UTxO to be distributed. One more than 'fanoutOutputThreshold'
-- so this exercises the partial fanout path.
healthyFullUTxO :: UTxO
healthyFullUTxO =
  let utxo = UTxO.map adaOnly $ generateWith (resize 100 genUTxOWithSimplifiedAddresses) 42
      utxoList = UTxO.toList utxo
   in UTxO.fromList $ take (fanoutOutputThreshold + 1) utxoList

-- | The first chunk to distribute: the leading 'fanoutChunkSize' entries.
healthyDistributeUTxO :: UTxO
healthyDistributeUTxO =
  UTxO.fromList $ take fanoutChunkSize $ UTxO.toList healthyFullUTxO

-- | UTxOs left after distributing the first chunk; carried over to the next step.
healthyRemainingUTxO :: UTxO
healthyRemainingUTxO =
  UTxO.fromList $ drop fanoutChunkSize $ UTxO.toList healthyFullUTxO

-- | A UTxO whose entries all share identical TxOut content (same address,
-- value, datum) but have distinct TxIns. Reproduces the typical TUI demo case
-- of repeated identical payments. The off-chain accumulator hashes element
-- content only ('utxoToElement' in 'Hydra.Tx.IsTx'), so these collapse into a
-- single accumulator element while the on-chain validator still sees N distinct
-- output positions in the fanout — triggering 'PartialFanoutMembershipFailed'.
duplicateContentFullUTxO :: UTxO
duplicateContentFullUTxO =
  UTxO.fromList [(txIn, canonicalTxOut) | txIn <- distinctTxIns]
 where
  n = fanoutOutputThreshold + 1
  canonicalTxOut = adaOnly (generateWith genTxOut 17)
  distinctTxIns = take n . List.nub $ generateWith (vectorOf (n * 4) genTxIn) 23

duplicateContentDistributeUTxO :: UTxO
duplicateContentDistributeUTxO =
  UTxO.fromList $ take fanoutChunkSize $ UTxO.toList duplicateContentFullUTxO

duplicateContentRemainingUTxO :: UTxO
duplicateContentRemainingUTxO =
  UTxO.fromList $ drop fanoutChunkSize $ UTxO.toList duplicateContentFullUTxO

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

-- | Accumulator covering all UTxOs in the snapshot (used for the input commitment).
fullAccumulator :: Accumulator.HydraAccumulator
fullAccumulator =
  Accumulator.buildFromSnapshotUTxOs
    healthyFullUTxO
    Nothing
    Nothing

-- | Accumulator covering only the UTxOs that remain after distributing the first chunk.
-- This is the commitment that the on-chain output must carry after a partial fanout step.
remainingAccumulator :: Accumulator.HydraAccumulator
remainingAccumulator =
  Accumulator.buildFromUTxO @Tx healthyRemainingUTxO

crsSize :: Int
crsSize = Accumulator.requiredCRSPointCount fullAccumulator

-- | The ClosedDatum used for the first-step (Closed → FanoutProgress) test.
healthyClosedDatum :: Head.ClosedDatum
healthyClosedDatum =
  Head.ClosedDatum
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyFullUTxO
    , alphaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , omegaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , parties = partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = OnChain.contestationPeriodFromDiffTime 10
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    , version = 0
    , accumulatorCommitment = Accumulator.getAccumulatorCommitment fullAccumulator
    }

-- | The FanoutProgressDatum used for both test cases, derived from 'healthyClosedDatum'.
-- Both the Closed-input and FanoutProgress-input tests use this as the datum
-- passed to 'partialFanoutTx' (and as the input datum for the intermediate case).
healthyProgressDatum :: Head.FanoutProgressDatum
healthyProgressDatum = Head.progressFromClosed healthyClosedDatum

-- | Snapshot accumulator covering 'duplicateContentFullUTxO' — note this is a
-- single-element accumulator because all entries hash to the same content.
duplicateContentFullAccumulator :: Accumulator.HydraAccumulator
duplicateContentFullAccumulator =
  Accumulator.buildFromSnapshotUTxOs
    duplicateContentFullUTxO
    Nothing
    Nothing

duplicateContentRemainingAccumulator :: Accumulator.HydraAccumulator
duplicateContentRemainingAccumulator =
  Accumulator.buildFromUTxO @Tx duplicateContentRemainingUTxO

duplicateContentClosedDatum :: Head.ClosedDatum
duplicateContentClosedDatum =
  Head.ClosedDatum
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx duplicateContentFullUTxO
    , alphaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , omegaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , parties = partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = OnChain.contestationPeriodFromDiffTime 10
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    , version = 0
    , accumulatorCommitment = Accumulator.getAccumulatorCommitment duplicateContentFullAccumulator
    }

duplicateContentProgressDatum :: Head.FanoutProgressDatum
duplicateContentProgressDatum = Head.progressFromClosed duplicateContentClosedDatum

-- | Partial fanout transaction over a UTxO whose entries share identical TxOut
-- content. This is the H57 reproduction: on-chain 'subsetScalars' is built from
-- N output positions while the off-chain accumulator only has one element, so
-- 'P_S(τ)·A_remaining(τ) /= A_full(τ)' and the membership pairing fails.
duplicateContentPartialFanoutTx :: (Tx, UTxO)
duplicateContentPartialFanoutTx = (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    partialFanoutTx
      scriptRegistry
      duplicateContentDistributeUTxO
      (headInput, headOutput)
      healthySlotNo
      duplicateContentProgressDatum
      duplicateContentRemainingAccumulator

  -- CRS must be large enough for the on-chain pairing check on a 'fanoutChunkSize'
  -- subset: P_S has degree 'fanoutChunkSize', requiring 'fanoutChunkSize + 1' G2
  -- points. Sized off the full snapshot for parity with the healthy fixture.
  scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline (Head.Closed duplicateContentClosedDatum))

  headOutput = modifyTxOutValue (<> participationTokens <> UTxO.totalValue duplicateContentFullUTxO) headOutput'

  participationTokens =
    fromList $
      map
        (\party -> (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1))
        healthyParties

healthyParties :: [Party]
healthyParties =
  [generateWith arbitrary i | i <- [1 .. 3]]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

data PartialFanoutMutation
  = MutatePartialFanoutValidityBeforeDeadline
  | -- | Partial fanout must NOT burn tokens (unlike full fanout)
    MutatePartialFanoutBurnTokens
  | MutatePartialFanoutOutputValue
  | -- | Steal Ada by reducing the continuing head output and adding a personal output
    MutatePartialFanoutStealAda
  | -- | Continuing FanoutProgressDatum must preserve headId, parties, contestationDeadline
    MutatePartialFanoutChangedParameters
  | -- | Continuing datum must carry the correct remaining accumulator commitment
    MutatePartialFanoutWrongAccumulator
  deriving stock (Generic, Show, Enum, Bounded)

genPartialFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genPartialFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutatePartialFanoutValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , SomeMutation (pure "U01") MutatePartialFanoutBurnTokens <$> do
        let headTokenScript = mkHeadTokenScript testSeedInput
        changeMintedTokens tx (fromList [(AssetId (scriptPolicyId (PlutusScript headTokenScript)) (UnsafeAssetName ""), -1)])
    , SomeMutation [toErrorCode PartialFanoutMembershipFailed, toErrorCode HeadValueIsNotPreserved] MutatePartialFanoutOutputValue <$> do
        let outs = txOuts' tx
            numDistributed = UTxO.size healthyDistributeUTxO
        (ix, out) <- elements (zip [1 .. numDistributed] (drop 1 outs))
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutatePartialFanoutStealAda <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
        stolenValue <- extractAdaFromValue (txOutValue headOut)
        someAddress <- genAddressInEra testNetworkId
        let stolenOutput = TxOut someAddress stolenValue TxOutDatumNone ReferenceScriptNone
        pure $
          Changes
            [ ChangeOutput 0 (modifyTxOutValue (<> negateValue stolenValue) headOut)
            , AppendOutput stolenOutput
            ]
    , SomeMutation (pure $ toErrorCode PartialFanoutChangedParameters) MutatePartialFanoutChangedParameters <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
        oneof
          [ do
              mutatedDeadline <- genMutatedDeadline
              pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headOut
          , do
              mutatedParties <- arbitrary `suchThat` (/= (partyToChain <$> healthyParties))
              pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headOut
          , do
              mutatedHeadId <- arbitrary `suchThat` (/= (toPlutusCurrencySymbol testPolicyId :: CurrencySymbol))
              pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId mutatedHeadId) headOut
          ]
    , SomeMutation (pure $ toErrorCode PartialFanoutMembershipFailed) MutatePartialFanoutWrongAccumulator <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
            -- fullAccumulator commitment differs from remainingAccumulator commitment,
            -- so putting it in the output invalidates the KZG membership proof.
            wrongCommitment = Accumulator.getAccumulatorCommitment fullAccumulator
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment) headOut
    ]
 where
  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)

  -- \| Extract a small amount of Ada from a value for mutation purposes.
  extractAdaFromValue :: Value -> Gen Value
  extractAdaFromValue val = do
    let Coin lovelace = selectLovelace val
    q <- choose (1, min 1_000_000 lovelace)
    pure $ lovelaceToValue (Coin q)

genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline =
  oneof
    [ pure 0
    , arbitrary
    ]
    `suchThat` (/= posixFromUTCTime healthyContestationDeadline)
