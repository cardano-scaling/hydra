{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseAny where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.UtilError (UtilError (MintingOrBurningIsForbidden))
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (Snapshot (..), mkHeadId, registryUTxO)
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyConfirmedSnapshot,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContestationPeriodSeconds,
  healthyHeadAdaOverhead,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  healthySignature,
  healthySplitUTxOInHead,
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Crypto (MultiSignature, toPlutusSignatures)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Snapshot qualified as Snapshot
import Hydra.Tx.Utils (IncrementalAction (..))
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genMintedOrBurnedValue,
  genScriptRegistry,
  genValue,
  genVerificationKey,
 )
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  changeMintedTokens,
  modifyInlineDatum,
  replaceAccumulatorCommitment,
  replaceContestationDeadline,
  replaceContestationPeriod,
  replaceContesters,
  replaceHeadAdaOverhead,
  replaceHeadId,
  replaceParties,
  replacePolicyIdWith,
  replaceSnapshotNumber,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

healthyCloseAnySnapshotNumber :: Snapshot.SnapshotNumber
healthyCloseAnySnapshotNumber = 1

healthyCloseAnySnapshotVersion :: Snapshot.SnapshotVersion
healthyCloseAnySnapshotVersion = 1

-- | Snapshot with no incremental action at all. Exercises the NoThing branch of
-- headAdaOverhead and the CloseAny redeemer path in closeTx.
healthyCloseAnySnapshot :: Snapshot Tx
healthyCloseAnySnapshot =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , version = healthyCloseAnySnapshotVersion
    , number = healthyCloseAnySnapshotNumber
    , confirmed = []
    , utxo = healthySplitUTxOInHead
    , utxoToCommit = Nothing
    , utxoToDecommit = Nothing
    , accumulator = Accumulator.buildFromSnapshotUTxOs healthySplitUTxOInHead Nothing Nothing
    }

healthyCloseAnyAccumulatorHash :: Head.Hash
healthyCloseAnyAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyCloseAnySnapshot

healthyCloseAnyOpenDatum :: Head.State
healthyCloseAnyOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , contestationPeriod = healthyContestationPeriod
      , depositPeriod = DP.toChain Fixture.dperiod
      , headSeed = toPlutusTxOutRef Fixture.testSeedInput
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = toInteger healthyCloseAnySnapshotVersion
      , accumulatorHash = healthyCloseAnyAccumulatorHash
      , headAdaOverhead = healthyHeadAdaOverhead
      }

-- | Healthy close transaction for the plain case: no incremental action, confirmed
-- snapshot. This exercises the CloseAny redeemer path, which uniquely enforces
-- snapshotNumber > 0 on-chain.
healthyCloseAnyTx :: (Tx, UTxO)
healthyCloseAnyTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId Fixture.testPolicyId)
      healthyCloseAnySnapshotVersion
      (healthyConfirmedSnapshot healthyCloseAnySnapshot)
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      NoThing

  lookupUTxO :: UTxO
  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn (healthyOpenHeadTxOut datum)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCloseAnyOpenDatum

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut datum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

data CloseMutation
  = NotContinueContract
  | -- | Unique to CloseAny: snapshotNumber must be > 0. Setting it to 0 fails the
    -- combined check (snapshotNumber > 0 && verifySignature) with FailedCloseAny.
    MutateSnapshotNumberToZero
  | MutateSignatureButNotSnapshotNumber
  | MutateSnapshotNumberButNotSignature
  | MutateSnapshotVersion
  | SnapshotNotSignedByAllParties
  | MutateRequiredSigner
  | MutateNoRequiredSigner
  | MutateMultipleRequiredSigner
  | MutatePartiesInOutput
  | MutateHeadIdInOutput
  | MutateInfiniteLowerBound
  | MutateInfiniteUpperBound
  | MutateContestationDeadline
  | MutateValidityInterval
  | CloseFromDifferentHead
  | MutateTokenMintingOrBurning
  | MutateContesters
  | MutateValueInOutput
  | MutateContestationPeriod
  | MutateAccumulatorCommitment
  | MutateCloseHeadAdaOverhead
  deriving stock (Generic, Show, Enum, Bounded)

genCloseAnyMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseAnyMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , -- CloseAny wraps both snapshotNumber>0 and signature checks in one FailedCloseAny.
      SomeMutation (pure $ toErrorCode FailedCloseAny) MutateSnapshotNumberToZero <$> do
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber 0) headTxOut
    , SomeMutation (pure $ toErrorCode FailedCloseAny) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        signature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $ Head.Close Head.CloseAny{signature, accumulatorHash = healthyCloseAnyAccumulatorHash}
    , SomeMutation (pure $ toErrorCode FailedCloseAny) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyCloseAnySnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCloseAnySnapshotVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode FailedCloseAny) SnapshotNotSignedByAllParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure . ChangeInputHeadDatum $ replaceParties mutatedParties healthyCloseAnyOpenDatum
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        n <- choose (1, length healthyOnChainParties - 1)
        fn <- elements [drop n, take n]
        let mutatedParties = fn healthyOnChainParties
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode NoSigners) MutateNoRequiredSigner <$> do
        pure $ ChangeRequiredSigners []
    , SomeMutation (pure $ toErrorCode TooManySigners) MutateMultipleRequiredSigner <$> do
        otherSigners <- listOf1 (genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey))
        let signerAndOthers = somePartyCardanoVerificationKey : otherSigners
        pure $ ChangeRequiredSigners (verificationKeyHash <$> signerAndOthers)
    , SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateContestationDeadline <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateContestationPeriod <$> do
        mutatedPeriod <- arbitrary
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationPeriod mutatedPeriod) headTxOut
    , SomeMutation (pure $ toErrorCode InfiniteLowerBound) MutateInfiniteLowerBound . ChangeValidityLowerBound <$> do
        pure TxValidityNoLowerBound
    , SomeMutation (pure $ toErrorCode InfiniteUpperBound) MutateInfiniteUpperBound . ChangeValidityUpperBound <$> do
        pure TxValidityNoUpperBound
    , SomeMutation (pure $ toErrorCode HasBoundedValidityCheckFailed) MutateValidityInterval <$> do
        (lowerSlotNo, upperSlotNo, adjustedContestationDeadline) <- genOversizedTransactionValidity
        pure $
          Changes
            [ ChangeValidityInterval (TxValidityLowerBound lowerSlotNo, TxValidityUpperBound upperSlotNo)
            , ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline adjustedContestationDeadline) headTxOut
            ]
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) CloseFromDifferentHead <$> do
        otherHeadId <- headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $
          Changes
            [ ChangeOutput 0 (replacePolicyIdWith Fixture.testPolicyId otherHeadId headTxOut)
            , ChangeInput
                healthyOpenHeadTxIn
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId $ healthyOpenHeadTxOut datum)
                ( Just $
                    toScriptData
                      ( Head.Close
                          Head.CloseAny
                            { signature =
                                toPlutusSignatures $
                                  healthySignature healthyCloseAnySnapshot
                            , accumulatorHash = healthyCloseAnyAccumulatorHash
                            }
                      )
                )
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- resize 3 . listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) MutateAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode ChangedHeadAdaOverhead) MutateCloseHeadAdaOverhead . ChangeOutput 0 <$> do
        wrongOverhead <- arbitrary `suchThat` (/= healthyHeadAdaOverhead)
        pure $ headTxOut & modifyInlineDatum (replaceHeadAdaOverhead wrongOverhead)
    ]
 where
  genOversizedTransactionValidity = do
    lowerValidityBound <- arbitrary :: Gen Word64
    upperValidityBound <- choose (lowerValidityBound + fromIntegral healthyContestationPeriodSeconds, maxBound)
    let adjustedContestationDeadline =
          fromMilliSeconds . DiffMilliSeconds $ (healthyContestationPeriodSeconds + fromIntegral upperValidityBound) * 1000
    pure (SlotNo lowerValidityBound, SlotNo upperValidityBound, adjustedContestationDeadline)

  headTxOut = fromJust $ txOuts' tx !!? 0

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCloseAnyOpenDatum

genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline =
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)
  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)
  deadline = posixFromUTCTime healthyContestationDeadline
