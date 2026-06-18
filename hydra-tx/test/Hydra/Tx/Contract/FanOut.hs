{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.FanOut where

import Hydra.Cardano.Api
import Hydra.Prelude hiding (label, toList)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import GHC.IsList (IsList (..))
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.CRS qualified as CRS
import Hydra.Contract.Deposit (DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (mkHeadTokenScript)
import Hydra.Data.ContestationPeriod qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime, slotNoToUTCTime)
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (ScriptRegistry (..), mkHeadId, registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Fanout (fanoutTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.Party (Party, partyToChain)
import Hydra.Tx.Utils (adaOnly, splitUTxO, verificationKeyToOnChainId)
import PlutusLedgerApi.V3 (toBuiltin)
import PlutusTx.Builtins (bls12_381_G1_uncompress)
import Test.Hydra.Tx.Fixture (dperiod, slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genAddressInEra, genForParty, genOutputFor, genScriptRegistryWithCRSSize, genUTxOSized, genUTxOWithSimplifiedAddresses, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (Mutation (..), SomeMutation (..), applyMutation, changeMintedTokens, replaceHeadAdaOverhead)
import Test.QuickCheck (choose, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

scriptRegistry :: ScriptRegistry
scriptRegistry = genScriptRegistryWithCRSSize crsSize `generateWith` 42

healthyFanoutTx :: (Tx, UTxO)
healthyFanoutTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> registryUTxO scriptRegistry

  tx =
    fromRight (error "FanOut healthy fixture: proof creation failed") $
      fanoutTx
        scriptRegistry
        (fst healthyFanoutSnapshotUTxO)
        Nothing
        (Just $ snd healthyFanoutSnapshotUTxO)
        healthyFanoutUTxO
        (headInput, headOutput)
        healthySlotNo
        headTokenScript

  headInput = generateWith arbitrary 42

  headTokenScript = mkHeadTokenScript testSeedInput

  headOutput =
    modifyTxOutValue (<> UTxO.totalValue healthyFanoutUTxO) $
      mkHeadOutput @CtxUTxO
        testNetworkId
        testPolicyId
        (verificationKeyToOnChainId <$> healthyParticipants)
        (mkTxOutDatumInline healthyFanoutDatum)

-- | Variant of 'healthyFanoutTx' with a trailing wallet change output appended,
-- simulating a wallet-balanced transaction. The validator must still accept this
-- because 'numberOfFanoutOutputs' excludes the trailing output from the KZG check.
healthyFanoutTxWithWalletChange :: (Tx, UTxO)
healthyFanoutTxWithWalletChange =
  applyMutation (AppendOutput walletChangeOutput) healthyFanoutTx
 where
  walletChangeOutput :: TxOut CtxTx
  walletChangeOutput =
    TxOut
      (mkVkAddress testNetworkId walletVk)
      (lovelaceToValue 2_000_000)
      TxOutDatumNone
      ReferenceScriptNone
  walletVk = generateWith genVerificationKey 99

healthyFanoutUTxO :: UTxO
healthyFanoutUTxO =
  UTxO.map adaOnly $ generateWith (genUTxOWithSimplifiedAddresses `suchThat` \u -> UTxO.size u > 1) 42

healthySlotNo :: SlotNo
healthySlotNo = arbitrary `generateWith` 42

healthyContestationDeadline :: UTCTime
healthyContestationDeadline =
  slotNoToUTCTime systemStart slotLength $ healthySlotNo - 1

healthyFanoutSnapshotUTxO :: (UTxO, UTxO)
healthyFanoutSnapshotUTxO = splitUTxO healthyFanoutUTxO

healthyFanoutSnapshotAccumulator :: Accumulator.HydraAccumulator
healthyFanoutSnapshotAccumulator =
  Accumulator.buildFromSnapshotUTxOs (fst healthyFanoutSnapshotUTxO) Nothing (Just $ snd healthyFanoutSnapshotUTxO)

crsSize :: Int
crsSize = Accumulator.requiredCRSPointCount healthyFanoutSnapshotAccumulator

healthyFanoutDatum :: Head.State
healthyFanoutDatum =
  Head.Closed
    Head.ClosedDatum
      { snapshotNumber = 1
      , parties =
          partyToChain <$> healthyParties
      , contestationDeadline = posixFromUTCTime healthyContestationDeadline
      , contestationPeriod = healthyContestationPeriod
      , depositPeriod = DP.toChain dperiod
      , headId = toPlutusCurrencySymbol testPolicyId
      , contesters = []
      , version = 0
      , accumulatorCommitment =
          Accumulator.getAccumulatorCommitment healthyFanoutSnapshotAccumulator
      , headAdaOverhead = 0
      }
 where
  healthyContestationPeriodSeconds = 10

  healthyContestationPeriod = OnChain.contestationPeriodFromDiffTime $ fromInteger healthyContestationPeriodSeconds

healthyParties :: [Party]
healthyParties =
  [ generateWith arbitrary i | i <- [1 .. 3]
  ]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

data FanoutMutation
  = MutateValidityBeforeDeadline
  | -- | Meant to test that the minting policy is burning all PTs and ST present in tx
    MutateThreadTokenQuantity
  | MutateAddUnexpectedOutput
  | MutateFanoutOutputValue
  | MutateDecommitOutputValue
  | -- | Inject an unrelated v_deposit input into a healthy Fanout.
    FanoutAbsorbForeignDeposit
  | -- | Change headAdaOverhead in the input ClosedDatum, breaking value conservation.
    MutateHeadAdaOverhead
  | -- | Supplying a fake CRS UTxO (no CRS reference script) should be rejected.
    MutateFanoutFakeCRS
  | -- | Correct CRS reference script hash but UTxO at an attacker-controlled address.
    -- Exposes that withCRSLookup must also validate txOutAddress.
    MutateFanoutWrongAddressCRS
  deriving stock (Generic, Show, Enum, Bounded)

genFanoutMutation :: (Tx, UTxO) -> Gen SomeMutation
genFanoutMutation (tx, _utxo) =
  oneof
    [ -- Spec: Transaction is posted after contestation deadline tmin > tfinal .
      SomeMutation (pure $ toErrorCode LowerBoundBeforeContestationDeadline) MutateValidityBeforeDeadline . ChangeValidityInterval <$> do
        lb <- genSlotBefore $ slotNoFromUTCTime systemStart slotLength healthyContestationDeadline
        pure (TxValidityLowerBound lb, TxValidityNoUpperBound)
    , -- Spec: All tokens are burnt |{cid 7→ · 7→ −1} ∈ mint| = m′ + 1.
      SomeMutation (pure $ toErrorCode BurntTokenNumberMismatch) MutateThreadTokenQuantity <$> do
        (token, _) <- elements burntTokens
        changeMintedTokens tx (fromList [(token, 1)])
    , -- Spec: The first m outputs are distributing funds according to η. That is, the outputs exactly
      -- correspond to the UTxO canonically combined U
      SomeMutation (pure $ toErrorCode FanoutUTxOHashMismatch) MutateAddUnexpectedOutput . PrependOutput <$> do
        arbitrary >>= genOutputFor
    , -- Spec: The following n outputs are distributing funds according to η∆ .
      -- That is, the outputs exactly # correspond to the UTxO canonically combined U∆
      SomeMutation (pure $ toErrorCode FanoutUTxOHashMismatch) MutateFanoutOutputValue <$> do
        let outs = txOuts' tx
        let noOfUtxoToOutputs = size $ UTxO.toMap (fst healthyFanoutSnapshotUTxO)
        (ix, out) <- elements (zip [0 .. noOfUtxoToOutputs - 1] outs)
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , -- Spec: The following n outputs are distributing funds according to η∆.
      -- That is, the outputs exactly # correspond to the UTxO canonically combined U∆
      SomeMutation (pure $ toErrorCode FanoutUTxOHashMismatch) MutateDecommitOutputValue <$> do
        let outs = txOuts' tx
        let noOfUtxoToOutputs = size $ UTxO.toMap (fst healthyFanoutSnapshotUTxO)
        (ix, out) <- elements (zip [noOfUtxoToOutputs .. length outs - 1] (drop noOfUtxoToOutputs outs))
        value' <- genValue `suchThat` (/= txOutValue out)
        pure $ ChangeOutput (fromIntegral ix) (modifyTxOutValue (const value') out)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateHeadAdaOverhead <$> do
        -- Changing headAdaOverhead in the input datum shifts the expected conservation
        -- baseline, so the on-chain headInValue == outputs + overhead check fails.
        wrongOverhead <- arbitrary `suchThat` (/= 0)
        pure $ ChangeInputHeadDatum (replaceHeadAdaOverhead wrongOverhead healthyFanoutDatum)
    , -- Fake CRS UTxO with no reference script — the script-hash check rejects it.
      SomeMutation (pure $ toErrorCode InvalidCRSRefScript) MutateFanoutFakeCRS <$> do
        let ScriptRegistry{crsReference = (_, legitCRSOut)} = scriptRegistry
        fakeCRSIn <- arbitrary `suchThat` (/= fst (crsReference scriptRegistry))
        someAddress <- genAddressInEra testNetworkId
        let fakeCRSOut = TxOut someAddress (txOutValue legitCRSOut) (txOutDatum legitCRSOut) ReferenceScriptNone
            fakeRedeemer =
              Head.Fanout
                { Head.numberOfFanoutOutputs = fromIntegral (UTxO.size healthyFanoutUTxO)
                , Head.proof = fanoutProof
                , Head.crsRef = toPlutusTxOutRef fakeCRSIn
                }
        pure $
          Changes
            [ AddReferenceInput fakeCRSIn fakeCRSOut
            , ChangeHeadRedeemer fakeRedeemer
            ]
    , -- Fake CRS UTxO: correct reference script hash but UTxO at an attacker-controlled address.
      -- The script-hash check passes because the reference script bytes are legitimate,
      -- but the address check (once enforced) must reject it.
      SomeMutation (pure $ toErrorCode InvalidCRSRefAddress) MutateFanoutWrongAddressCRS <$> do
        let ScriptRegistry{crsReference = (legitCRSIn, legitCRSOut)} = scriptRegistry
        fakeCRSIn <- arbitrary `suchThat` (/= legitCRSIn)
        wrongAddress <- genAddressInEra testNetworkId `suchThat` (/= txOutAddress legitCRSOut)
        let fakeCRSOut = TxOut wrongAddress (txOutValue legitCRSOut) (txOutDatum legitCRSOut) (mkScriptRef CRS.validatorScript)
            fakeRedeemer =
              Head.Fanout
                { Head.numberOfFanoutOutputs = fromIntegral (UTxO.size healthyFanoutUTxO)
                , Head.proof = fanoutProof
                , Head.crsRef = toPlutusTxOutRef fakeCRSIn
                }
        pure $
          Changes
            [ AddReferenceInput fakeCRSIn fakeCRSOut
            , ChangeHeadRedeemer fakeRedeemer
            ]
    , SomeMutation (pure $ toErrorCode HeadRedeemerNotIncrement) FanoutAbsorbForeignDeposit <$> do
        extraIn <- genTxIn
        extraDeposited <- UTxO.map adaOnly <$> genUTxOSized 1
        attackerVk <- genVerificationKey
        let
          -- Fanout has no upper bound by default; without a finite one
          -- the deposit validator short-circuits before the later guards.
          upperSlot = healthySlotNo + 1000
          upperUTC = slotNoToUTCTime systemStart slotLength upperSlot
          extraDeadline = addUTCTime (60 * 60 * 24) upperUTC
          extraDepositOut :: TxOut CtxUTxO
          extraDepositOut =
            mkDepositOutput
              testNetworkId
              (mkHeadId testPolicyId)
              extraDeposited
              extraDeadline
          attackerOut :: TxOut CtxTx
          attackerOut =
            TxOut
              (mkVkAddress testNetworkId attackerVk)
              (txOutValue extraDepositOut)
              TxOutDatumNone
              ReferenceScriptNone
        pure $
          Changes
            [ AddInput extraIn extraDepositOut (Just $ toScriptData Claim)
            , AppendOutput attackerOut
            , AddScript depositValidatorScript
            , ChangeValidityUpperBound (TxValidityUpperBound upperSlot)
            ]
    ]
 where
  burntTokens =
    case toList . txMintValueToValue . txMintValue $ getTxBodyContent $ txBody tx of
      [] -> error "expected minted value"
      v -> v

  genSlotBefore (SlotNo slot) = SlotNo <$> choose (0, slot)

  fanoutProof =
    bls12_381_G1_uncompress $
      toBuiltin $
        either error id $
          Accumulator.createMembershipProofFromUTxO @Tx
            healthyFanoutUTxO
            healthyFanoutSnapshotAccumulator
            (Accumulator.crsG1Points crsSize)
