{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-ambiguous-fields #-}

module Hydra.Tx.Contract.PartialFanout where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (HeadValueIsNotPreserved, InvalidCRSDatum, LowerBoundBeforeContestationDeadline, PartialFanoutCannotBeLastBatch, PartialFanoutChangedParameters, PartialFanoutMembershipFailed, PartialFanoutZeroOutputs))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Contract.UtilError (UtilError (MintingOrBurningIsForbidden))
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Gen ()
import Hydra.Plutus.Orphans ()
import Hydra.Tx (ScriptRegistry (..), registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Fanout (partialFanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Party (Party, partyToChain, vkey)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (CurrencySymbol, POSIXTime)
import Test.Hydra.Tx.Fixture (dperiod, fanoutChunkSize, fanoutOutputThreshold, slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genAddressInEra, genForParty, genScriptRegistry, genUTxOWithSimplifiedAddresses, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens, modifyInlineDatum, replaceAccumulatorCommitment, replaceContestationDeadline, replaceHeadAdaOverhead, replaceHeadId, replaceParties)
import Test.QuickCheck (choose, elements, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

scriptRegistry :: ScriptRegistry
scriptRegistry = genScriptRegistry `generateWith` 42

-- | Build a healthy partial fanout transaction with a given input head state.
-- Used for both the Closed → FanoutProgress and FanoutProgress → FanoutProgress cases,
-- which share identical transaction structure and only differ in the input datum type.
healthyPartialFanoutTxWith :: Head.State -> (Tx, UTxO)
healthyPartialFanoutTxWith =
  mkHealthyPartialFanoutTxWith scriptRegistry healthyFullUTxO healthyDistributeUTxO healthyProgressDatum remainingAccumulator

mkHealthyPartialFanoutTxWith ::
  ScriptRegistry ->
  UTxO ->
  UTxO ->
  Head.FanoutProgressDatum ->
  Accumulator.HydraAccumulator ->
  Head.State ->
  (Tx, UTxO)
mkHealthyPartialFanoutTxWith reg fullUTxO distributeUTxO progressDatum remainingAcc inputState = (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyHeadInput headOutput
      <> registryUTxO reg

  tx =
    partialFanoutTx
      reg
      distributeUTxO
      (healthyHeadInput, headOutput)
      healthySlotNo
      progressDatum
      remainingAcc

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline inputState)

  headOutput = modifyTxOutValue (<> healthyParticipationTokens <> UTxO.totalValue fullUTxO) headOutput'

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

-- | The ClosedDatum used for the first-step (Closed → FanoutProgress) test.
healthyClosedDatum :: Head.ClosedDatum
healthyClosedDatum =
  Head.ClosedDatum
    { snapshotNumber = 1
    , parties = partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = OnChain.contestationPeriodFromDiffTime 10
    , depositPeriod = DP.toChain dperiod
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    , version = 0
    , accumulatorCommitment = Accumulator.getAccumulatorCommitment fullAccumulator
    , headAdaOverhead = 0
    }

-- | The FanoutProgressDatum used for both test cases, derived from 'healthyClosedDatum'.
-- Both the Closed-input and FanoutProgress-input tests use this as the datum
-- passed to 'partialFanoutTx' (and as the input datum for the intermediate case).
healthyProgressDatum :: Head.FanoutProgressDatum
healthyProgressDatum = Head.progressFromClosed healthyClosedDatum

healthyParties :: [Party]
healthyParties =
  [generateWith arbitrary i | i <- [1 .. 3]]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyHeadInput :: TxIn
healthyHeadInput = generateWith arbitrary 42

healthyParticipationTokens :: Value
healthyParticipationTokens =
  fromList $
    map
      (\party -> (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1))
      healthyParties

-- | A UTxO set where two entries share identical TxOut content (same address + value).
-- This exposes the duplicate-element bug: buildFromUTxO deduplicates them in the
-- off-chain accumulator, but on-chain txOutsToSubsetScalars sees two identical scalars.
duplicateFullUTxO :: UTxO
duplicateFullUTxO =
  case UTxO.toList healthyFullUTxO of
    (firstIn, firstOut) : (secondIn, _) : rest ->
      UTxO.fromList $ (firstIn, firstOut) : (secondIn, firstOut) : rest
    _ -> error "duplicateFullUTxO: healthyFullUTxO has fewer than 2 entries"

duplicateDistributeUTxO :: UTxO
duplicateDistributeUTxO =
  UTxO.fromList $ take fanoutChunkSize $ UTxO.toList duplicateFullUTxO

duplicateRemainingUTxO :: UTxO
duplicateRemainingUTxO =
  UTxO.fromList $ drop fanoutChunkSize $ UTxO.toList duplicateFullUTxO

duplicateFullAccumulator :: Accumulator.HydraAccumulator
duplicateFullAccumulator =
  Accumulator.buildFromSnapshotUTxOs
    duplicateFullUTxO
    Nothing
    Nothing

duplicateRemainingAccumulator :: Accumulator.HydraAccumulator
duplicateRemainingAccumulator =
  Accumulator.buildFromUTxO @Tx duplicateRemainingUTxO

duplicateScriptRegistry :: ScriptRegistry
duplicateScriptRegistry = genScriptRegistry `generateWith` 42

duplicateProgressDatum :: Head.FanoutProgressDatum
duplicateProgressDatum = Head.progressFromClosed duplicateClosedDatum

duplicateClosedDatum :: Head.ClosedDatum
duplicateClosedDatum =
  healthyClosedDatum
    { Head.accumulatorCommitment = Accumulator.getAccumulatorCommitment duplicateFullAccumulator
    }

-- | A partial fanout tx where two distributed UTxOs share identical TxOut content.
healthyPartialFanoutTxWithDuplicates :: (Tx, UTxO)
healthyPartialFanoutTxWithDuplicates =
  mkHealthyPartialFanoutTxWith
    duplicateScriptRegistry
    duplicateFullUTxO
    duplicateDistributeUTxO
    duplicateProgressDatum
    duplicateRemainingAccumulator
    (Head.FanoutProgress duplicateProgressDatum)

-- | A native token under a foreign policy that the head can never burn (only
-- head-policy tokens are burned on fanout), as if it had been committed into the
-- head. This is the kind of "unburned token" that can leave a head stuck (#2334).
unburnedToken :: Value
unburnedToken =
  fromList [(AssetId (scriptPolicyId (PlutusScript CRS.validatorScript)) (UnsafeAssetName "unburned"), 1)]

-- | A partial fanout whose head output additionally carries an 'unburnedToken'.
-- #2334: even though this token can never be burned (so the final, token-burning
-- fanout step is blocked), a partial fanout step still validates and distributes
-- the selected UTxOs. So a participant can recover funds via selection regardless
-- of the stuck token; only the residual head output (holding it) stays stuck.
healthyPartialFanoutTxWithUnburnedToken :: (Tx, UTxO)
healthyPartialFanoutTxWithUnburnedToken = (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton healthyHeadInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    partialFanoutTx
      scriptRegistry
      healthyDistributeUTxO
      (healthyHeadInput, headOutput)
      healthySlotNo
      healthyProgressDatum
      remainingAccumulator

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline (Head.Closed healthyClosedDatum))

  headOutput =
    modifyTxOutValue
      (<> healthyParticipationTokens <> UTxO.totalValue healthyFullUTxO <> unburnedToken)
      headOutput'

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
  | -- | Correct CRS address + reference script but a NON-CANONICAL SRS datum.
    MutatePartialFanoutNonCanonicalCRS
  | -- | Redeemer claims zero distributed outputs. KZG degenerates to e(A,G2) = e(newAcc,G2),
    -- passing trivially when newAcc = A. Must be rejected by mustHaveOutputs.
    MutatePartialFanoutZeroOutputs
  | -- | Output datum carries G1_generator as the new accumulator commitment, indicating
    -- all elements were distributed. Must use FinalPartialFanout instead to burn tokens.
    MutatePartialFanoutLastBatch
  | -- | Changing headAdaOverhead in the continuing FanoutProgressDatum must be rejected.
    MutatePartialFanoutHeadAdaOverhead
  deriving stock (Generic, Show, Enum, Bounded)

genPartialFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genPartialFanoutMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutatePartialFanoutValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutatePartialFanoutBurnTokens <$> do
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
    , -- A CRS reference input at the correct address and reference script but carrying a
      -- non-canonical SRS datum must be rejected: binding only the CRS location and not
      -- its datum lets an attacker substitute a powers-of-tau setup whose tau they know
      -- and forge membership proofs. A same-tau CRS with one extra G2 point keeps the
      -- pairing valid while its datum bytes differ from the canonical CRS.
      SomeMutation (pure $ toErrorCode InvalidCRSDatum) MutatePartialFanoutNonCanonicalCRS <$> do
        let ScriptRegistry{crsReference = (legitCRSIn, legitCRSOut)} = scriptRegistry
        substitutedCRSIn <- arbitrary `suchThat` (/= legitCRSIn)
        let substitutedCRSOut :: TxOut CtxUTxO
            substitutedCRSOut =
              TxOut
                (txOutAddress legitCRSOut)
                (txOutValue legitCRSOut)
                (Accumulator.createCRSG2Datum (Accumulator.defaultItems + 1))
                (mkScriptRef CRS.validatorScript)
            substitutedRedeemer =
              Head.PartialFanout
                { Head.numberOfPartialOutputs = fromIntegral (UTxO.size healthyDistributeUTxO)
                , Head.crsRef = toPlutusTxOutRef substitutedCRSIn
                }
        pure $
          Changes
            [ AddReferenceInput substitutedCRSIn substitutedCRSOut
            , ChangeHeadRedeemer substitutedRedeemer
            ]
    , pure $
        SomeMutation (pure $ toErrorCode PartialFanoutZeroOutputs) MutatePartialFanoutZeroOutputs $
          let ScriptRegistry{crsReference} = scriptRegistry
              crsRef = toPlutusTxOutRef (fst crsReference)
           in ChangeHeadRedeemer
                Head.PartialFanout
                  { Head.numberOfPartialOutputs = 0
                  , Head.crsRef = crsRef
                  }
    , pure $
        SomeMutation (pure $ toErrorCode PartialFanoutCannotBeLastBatch) MutatePartialFanoutLastBatch $
          let headOut = fromJust $ txOuts' tx !!? 0
              g1Generator = Accumulator.getAccumulatorCommitment (Accumulator.buildFromUTxO @Tx mempty)
           in ChangeOutput 0 $ modifyInlineDatum (replaceAccumulatorCommitment g1Generator) headOut
    , SomeMutation (pure $ toErrorCode PartialFanoutChangedParameters) MutatePartialFanoutHeadAdaOverhead <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
        wrongOverhead <- arbitrary `suchThat` (/= 0)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadAdaOverhead wrongOverhead) headOut
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
