{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.PartialFanout where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (HeadValueIsNotPreserved, LowerBoundBeforeContestationDeadline, PartialFanoutChangedParameters, PartialFanoutHashesNotCleared, PartialFanoutMembershipFailed))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Fanout (partialFanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.Party (Party, partyToChain, vkey)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (toBuiltin)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genAddressInEra, genForParty, genScriptRegistryWithCRSSize, genUTxOWithSimplifiedAddresses, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens, modifyInlineDatum, replaceAccumulatorCommitment, replaceSnapshotNumber, replaceUTxOHash)
import Test.QuickCheck (choose, elements, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

-- | A healthy partial fanout transaction that distributes a subset of UTxOs
-- and continues the Closed state with an updated accumulator.
healthyPartialFanoutTx :: (Tx, UTxO)
healthyPartialFanoutTx =
  (tx, lookupUTxO)
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
      healthyClosedDatum
      remainingAccumulator

  scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline healthyPartialFanoutState)

  -- The closed head output holds the full locked UTxO value plus participation tokens.
  headOutput = modifyTxOutValue (<> participationTokens <> UTxO.totalValue healthyFullUTxO) headOutput'

  participationTokens =
    fromList $
      map
        ( \party ->
            (AssetId testPolicyId (UnsafeAssetName . serialiseToRawBytes . verificationKeyHash . vkey $ party), 1)
        )
        healthyParties

-- | The full UTxO to be distributed, matching the E2E test scenario with
-- 20 UTxOs to exercise partial fanout (threshold is 19).
healthyFullUTxO :: UTxO
healthyFullUTxO =
  let utxo = UTxO.map adaOnly $ generateWith (resize 100 genUTxOWithSimplifiedAddresses) 42
      utxoList = UTxO.toList utxo
   in UTxO.fromList $ take 20 utxoList

-- | Split the full UTxO: distribute the first 15, keep the remaining 5.
-- This matches the E2E fanoutChunkSize of 15.
healthyDistributeUTxO :: UTxO
healthyDistributeUTxO =
  UTxO.fromList $ take 15 $ UTxO.toList healthyFullUTxO

healthyRemainingUTxO :: UTxO
healthyRemainingUTxO =
  UTxO.fromList $ drop 15 $ UTxO.toList healthyFullUTxO

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

-- | The full accumulator covers all UTxOs that were originally in the snapshot.
fullAccumulator :: Accumulator.HydraAccumulator
fullAccumulator =
  Accumulator.buildFromSnapshotUTxOs
    healthyFullUTxO
    Nothing
    Nothing

-- | After distributing healthyDistributeUTxO, the remaining accumulator covers
-- only the remaining UTxOs.
remainingAccumulator :: Accumulator.HydraAccumulator
remainingAccumulator =
  Accumulator.buildFromUTxO @Tx healthyRemainingUTxO

crsSize :: Int
crsSize = Accumulator.requiredCRSSize fullAccumulator + 1

-- | The ClosedDatum that the head output currently carries (before partial fanout).
-- This represents a freshly-closed head with the full accumulator.
healthyClosedDatum :: Head.ClosedDatum
healthyClosedDatum =
  Head.ClosedDatum
    { snapshotNumber = 1
    , utxoHash = toBuiltin $ hashUTxO @Tx healthyFullUTxO
    , alphaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , omegaUTxOHash = toBuiltin $ hashUTxO @Tx mempty
    , parties = partyToChain <$> healthyParties
    , contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , contestationPeriod = healthyContestationPeriod
    , headId = toPlutusCurrencySymbol testPolicyId
    , contesters = []
    , version = 0
    , accumulatorCommitment =
        Accumulator.getAccumulatorCommitment fullAccumulator
    }
 where
  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime 10

-- | The Head.State wrapping the ClosedDatum for the datum on the head output.
healthyPartialFanoutState :: Head.State
healthyPartialFanoutState = Head.Closed healthyClosedDatum

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
  | -- | Continuing datum must preserve snapshotNumber, version, contesters, contestationDeadline
    MutatePartialFanoutChangedParameters
  | -- | Continuing datum must clear utxoHash, alphaUTxOHash, omegaUTxOHash to emptyHash
    MutatePartialFanoutHashesNotCleared
  | -- | Continuing datum must carry the correct remaining accumulator commitment
    MutatePartialFanoutWrongAccumulator
  deriving stock (Generic, Show, Enum, Bounded)

genPartialFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genPartialFanoutMutation (tx, _utxo) =
  oneof
    [ -- Partial fanout must also respect the contestation deadline
      SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutatePartialFanoutValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , -- Partial fanout must NOT burn any tokens (tokens are kept for subsequent fanouts).
      -- mustNotMintOrBurn uses error code "U01"
      SomeMutation (pure "U01") MutatePartialFanoutBurnTokens <$> do
        let headTokenScript = mkHeadTokenScript testSeedInput
        changeMintedTokens tx (fromList [(AssetId (scriptPolicyId (PlutusScript headTokenScript)) (UnsafeAssetName ""), -1)])
    , -- Mutating a distributed output value violates either value conservation (if the
      -- head output is not compensated) or membership verification. Both are valid
      -- rejections; which fires first depends on whether conservation is also broken.
      SomeMutation [toErrorCode PartialFanoutMembershipFailed, toErrorCode HeadValueIsNotPreserved] MutatePartialFanoutOutputValue <$> do
        -- The distributed outputs start at index 1 (index 0 is the continuing head output)
        let outs = txOuts' tx
        let numDistributed = UTxO.size healthyDistributeUTxO
        -- Pick one of the distributed outputs (indices 1..numDistributed)
        (ix, out) <- elements (zip [1 .. numDistributed] (drop 1 outs))
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- An adversary must not be able to steal Ada by reducing the continuing head
      -- output value and routing the difference into a personal output.
      SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutatePartialFanoutStealAda <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
        stolenValue <- extractAdaFromValue (txOutValue headOut)
        someAddress <- genAddressInEra testNetworkId
        let stolenOutput = TxOut someAddress stolenValue TxOutDatumNone ReferenceScriptNone
        pure $
          Changes
            [ ChangeOutput 0 (modifyTxOutValue (<> negateValue stolenValue) headOut)
            , AppendOutput stolenOutput
            ]
    , -- The continuing datum must not change snapshotNumber, version, contesters or
      -- contestationDeadline relative to the input datum.
      SomeMutation (pure $ toErrorCode PartialFanoutChangedParameters) MutatePartialFanoutChangedParameters <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
            mutatedSnapshotNumber = healthyClosedDatum.snapshotNumber + 1
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber mutatedSnapshotNumber) headOut
    , -- All three hash fields (utxoHash, alphaUTxOHash, omegaUTxOHash) must be
      -- cleared to emptyHash in the continuing datum after a partial fanout.
      SomeMutation (pure $ toErrorCode PartialFanoutHashesNotCleared) MutatePartialFanoutHashesNotCleared <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
            nonEmptyHash = toBuiltin $ hashUTxO @Tx healthyFullUTxO
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceUTxOHash nonEmptyHash) headOut
    , -- An adversary must not be able to claim a different remaining accumulator
      -- commitment in the continuing datum, which would break subsequent fanout steps.
      SomeMutation (pure $ toErrorCode PartialFanoutMembershipFailed) MutatePartialFanoutWrongAccumulator <$> do
        let headOut = fromJust $ txOuts' tx !!? 0
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
