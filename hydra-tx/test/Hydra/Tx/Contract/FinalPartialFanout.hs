{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.FinalPartialFanout where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import GHC.IsList (IsList (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (BurntTokenNumberMismatch, FinalPartialFanoutMembershipFailed, HeadValueIsNotPreserved, InvalidCRSRefScript, LowerBoundBeforeContestationDeadline))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Fanout (finalPartialFanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.KZGTrustedSetup (fanoutChunkSize)
import Hydra.Tx.Party (Party, partyToChain)
import Hydra.Tx.ScriptRegistry (ScriptRegistry (..))
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusTx.Builtins (bls12_381_G1_uncompress)
import Test.Hydra.Tx.Fixture (slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genAddressInEra, genForParty, genScriptRegistryWithCRSSize, genUTxOWithSimplifiedAddresses, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), changeMintedTokens)
import Test.QuickCheck (choose, elements, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

scriptRegistry :: ScriptRegistry
scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

-- | A healthy final partial fanout transaction. It distributes all remaining
-- UTxOs (from a FanoutProgress state) and burns all head tokens.
healthyFinalPartialFanoutTx :: (Tx, UTxO)
healthyFinalPartialFanoutTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    finalPartialFanoutTx
      scriptRegistry
      healthyDistributeUTxO
      (headInput, headOutput)
      healthySlotNo
      headTokenScript
      healthyRemainingAccumulator

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput' :: TxOut CtxUTxO
  headOutput' =
    mkHeadOutput
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline $ Head.FanoutProgress healthyProgressDatum)

  -- The FanoutProgress head output holds the remaining UTxO value on top of the
  -- ST + PTs already included by mkHeadOutput. Participation tokens are NOT added
  -- again here since mkHeadOutput already includes them.
  headOutput = modifyTxOutValue (<> UTxO.totalValue healthyDistributeUTxO) headOutput'

-- | The UTxOs to distribute in the final partial fanout step.
-- We use a small UTxO set (≤ fanoutChunkSize) representing the last batch.
healthyDistributeUTxO :: UTxO
healthyDistributeUTxO =
  let utxo = UTxO.map adaOnly $ generateWith (resize 100 genUTxOWithSimplifiedAddresses) 99
   in UTxO.fromList $ take fanoutChunkSize $ UTxO.toList utxo

-- | The accumulator covering exactly the UTxOs being distributed in this final step.
healthyRemainingAccumulator :: Accumulator.HydraAccumulator
healthyRemainingAccumulator =
  Accumulator.buildFromUTxO @Tx healthyDistributeUTxO

crsSize :: Int
crsSize = Accumulator.requiredCRSPointCount healthyRemainingAccumulator

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

-- | The FanoutProgressDatum from which the final fanout step reads.
-- accumulatorCommitment matches the remainingAccumulator.
healthyProgressDatum :: Head.FanoutProgressDatum
healthyProgressDatum =
  Head.FanoutProgressDatum
    { Head.headId = toPlutusCurrencySymbol testPolicyId
    , Head.parties = partyToChain <$> healthyParties
    , Head.contestationDeadline = posixFromUTCTime healthyContestationDeadline
    , Head.accumulatorCommitment =
        Accumulator.getAccumulatorCommitment healthyRemainingAccumulator
    }

healthyParties :: [Party]
healthyParties =
  [generateWith arbitrary i | i <- [1 .. 3]]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

data FinalPartialFanoutMutation
  = MutateFinalPartialFanoutValidityBeforeDeadline
  | -- | Final fanout MUST burn all head tokens
    MutateFinalPartialFanoutBurnTokens
  | -- | Changing an output value breaks the membership proof
    MutateFinalPartialFanoutOutputValue
  | -- | Replacing the input datum's accumulator commitment invalidates the KZG membership proof
    MutateFinalPartialFanoutWrongAccumulator
  | -- | Claiming fewer outputs in the redeemer than are actually in the tx
    -- uses a different scalar set, breaking the membership proof
    MutateFinalPartialFanoutOutputCount
  | -- | Supplying a fake CRS UTxO (no CRS reference script) should be rejected.
    -- Currently passes because withCRSLookup does not authenticate the reference script.
    -- Expected to fail with InvalidCRSRefScript.
    MutateFinalPartialFanoutFakeCRS
  | -- | Claim N-1 outputs in the redeemer with a valid proof; the N-th UTxO's value
    -- is left unaccounted.
    MutateFinalPartialFanoutStealAda
  deriving stock (Generic, Show, Enum, Bounded)

genFinalPartialFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFinalPartialFanoutMutation (tx, _utxo) =
  oneof
    [ -- Final fanout must also respect the contestation deadline
      SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutateFinalPartialFanoutValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , -- All head tokens must be burned
      SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateFinalPartialFanoutBurnTokens <$> do
        (token, _) <- elements burntTokens
        changeMintedTokens tx (fromList [(token, 1)])
    , -- Changing an output value breaks the membership proof
      SomeMutation (pure $ toErrorCode FinalPartialFanoutMembershipFailed) MutateFinalPartialFanoutOutputValue <$> do
        let outs = txOuts' tx
        (ix, out) <- elements (zip [0 :: Int ..] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- Replacing the input datum commitment with an empty accumulator invalidates the proof
      pure $
        SomeMutation (pure $ toErrorCode FinalPartialFanoutMembershipFailed) MutateFinalPartialFanoutWrongAccumulator $
          let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.buildFromUTxO @Tx mempty)
              Head.FanoutProgressDatum{headId, parties, contestationDeadline} = healthyProgressDatum
           in ChangeInputHeadDatum (Head.FanoutProgress Head.FanoutProgressDatum{headId, parties, contestationDeadline, accumulatorCommitment = wrongCommitment})
    , -- Under-counting outputs (numberOfPartialOutputs = 0) produces an empty
      -- scalar set, which does not match the proof built for the full set.
      -- The proof field is also a dummy G1 point — the real proof cannot be
      -- recovered from the test fixture without re-running the off-chain prover,
      -- so both count and proof are wrong; either alone would fail membership.
      pure $
        SomeMutation (pure $ toErrorCode FinalPartialFanoutMembershipFailed) MutateFinalPartialFanoutOutputCount $
          let ScriptRegistry{crsReference} = scriptRegistry
              crsRef = toPlutusTxOutRef (fst crsReference)
              dummyProof = Accumulator.getAccumulatorCommitment (Accumulator.buildFromUTxO @Tx mempty)
           in ChangeHeadRedeemer
                Head.FinalPartialFanout
                  { Head.numberOfPartialOutputs = 0
                  , Head.proof = dummyProof
                  , Head.crsRef = crsRef
                  }
    , SomeMutation (pure $ toErrorCode InvalidCRSRefScript) MutateFinalPartialFanoutFakeCRS <$> do
        -- Build a fake CRS UTxO: same datum as the real CRS but at an attacker's
        -- address with no reference script. The G2 data is valid so the pairing
        -- check passes, but the script-hash guard (once added) rejects it.
        let ScriptRegistry{crsReference = (_, legitCRSOut)} = scriptRegistry
            fakeCRSDatum = txOutDatum legitCRSOut
        fakeCRSIn <- arbitrary `suchThat` (/= fst (crsReference scriptRegistry))
        someAddress <- genAddressInEra testNetworkId
        let fakeCRSOut = TxOut someAddress (txOutValue legitCRSOut) fakeCRSDatum ReferenceScriptNone
            realProof =
              bls12_381_G1_uncompress $
                toBuiltin $
                  Accumulator.createMembershipProofFromUTxO @Tx
                    healthyDistributeUTxO
                    healthyRemainingAccumulator
                    (Accumulator.crsG1Points crsSize)
            fakeRedeemer =
              Head.FinalPartialFanout
                { Head.numberOfPartialOutputs = fromIntegral (UTxO.size healthyDistributeUTxO)
                , Head.proof = realProof
                , Head.crsRef = toPlutusTxOutRef fakeCRSIn
                }
        pure $
          Changes
            [ AddReferenceInput fakeCRSIn fakeCRSOut
            , ChangeHeadRedeemer fakeRedeemer
            ]
    , -- Claim one fewer output than the tx actually distributes, with a recomputed
      -- valid proof. The N-th UTxO's value is left unaccounted — no value conservation
      -- check currently catches this.
      pure $
        SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateFinalPartialFanoutStealAda $
          let n = UTxO.size healthyDistributeUTxO - 1
              distributedMinus1 = UTxO.fromList $ take n $ UTxO.toList healthyDistributeUTxO
              proof =
                bls12_381_G1_uncompress $
                  toBuiltin $
                    Accumulator.createMembershipProofFromUTxO @Tx
                      distributedMinus1
                      healthyRemainingAccumulator
                      (Accumulator.crsG1Points crsSize)
              ScriptRegistry{crsReference} = scriptRegistry
              crsRef = toPlutusTxOutRef (fst crsReference)
           in ChangeHeadRedeemer
                Head.FinalPartialFanout
                  { Head.numberOfPartialOutputs = fromIntegral n
                  , Head.proof = proof
                  , Head.crsRef = crsRef
                  }
    ]
 where
  burntTokens =
    case toList . txMintValueToValue . txMintValue $ getTxBodyContent $ txBody tx of
      [] -> error "expected minted value"
      v -> v

  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)
