{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseCommitUsed where

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
import Hydra.Tx (
  ConfirmedSnapshot,
  Snapshot (..),
  SnapshotNumber,
  SnapshotVersion,
  getSnapshot,
  mkHeadId,
  registryUTxO,
  signatures,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyConfirmedSnapshot,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContestationPeriodSeconds,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  healthySignature,
  healthySplitUTxOInHead,
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Crypto (MultiSignature (..), toPlutusSignatures)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Utils (IncrementalAction (..), setIncrementalActionMaybe)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (
  genAddressInEra,
  genHash,
  genMintedOrBurnedValue,
  genScriptRegistry,
  genUTxOSized,
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
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyCommitAppliedSnapshotNumber :: SnapshotNumber
healthyCommitAppliedSnapshotNumber = 1

healthyCommitAppliedSnapshotVersion :: SnapshotVersion
healthyCommitAppliedSnapshotVersion = 1

-- | Open state version after the increment was applied (one higher than snapshot version).
healthyCommitAppliedOpenVersion :: SnapshotVersion
healthyCommitAppliedOpenVersion = healthyCommitAppliedSnapshotVersion + 1

-- | UTxO that was committed (deposit already merged into the head).
healthyDepositUTxO :: UTxO
healthyDepositUTxO = genUTxOSized 3 `generateWith` 42

-- | Snapshot with an applied commit: version is one lower than the open state version.
-- Exercises the (ToCommit, False) branch of headAdaOverhead computation in closeTx.
healthyCommitAppliedSnapshot :: Snapshot Tx
healthyCommitAppliedSnapshot =
  Snapshot
    { headId = mkHeadId Fixture.testPolicyId
    , version = healthyCommitAppliedSnapshotVersion
    , number = healthyCommitAppliedSnapshotNumber
    , confirmed = []
    , utxo = healthySplitUTxOInHead
    , utxoToCommit = Just healthyDepositUTxO
    , utxoToDecommit = Nothing
    , accumulator = Accumulator.buildFromSnapshotUTxOs healthySplitUTxOInHead (Just healthyDepositUTxO) Nothing
    }

healthyCommitAppliedAccumulatorHash :: Head.Hash
healthyCommitAppliedAccumulatorHash =
  toBuiltin $ Accumulator.getAccumulatorHash $ accumulator healthyCommitAppliedSnapshot

-- | Commit was applied: both healthySplitUTxOInHead and healthyDepositUTxO are in the head.
healthyCommitAppliedHeadAdaOverhead :: Integer
healthyCommitAppliedHeadAdaOverhead =
  let Coin n = selectLovelace (UTxO.totalValue healthySplitUTxOInHead)
   in negate n

healthyCommitAppliedConfirmedSnapshot :: ConfirmedSnapshot Tx
healthyCommitAppliedConfirmedSnapshot = healthyConfirmedSnapshot healthyCommitAppliedSnapshot

healthyCommitAppliedOpenDatum :: Head.State
healthyCommitAppliedOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , contestationPeriod = healthyContestationPeriod
      , depositPeriod = DP.toChain Fixture.dperiod
      , headSeed = toPlutusTxOutRef Fixture.testSeedInput
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = toInteger healthyCommitAppliedOpenVersion
      , accumulatorHash = healthyCommitAppliedAccumulatorHash
      , headAdaOverhead = healthyCommitAppliedHeadAdaOverhead
      }

-- | Healthy close transaction for the case of closing after a commit was applied.
-- The snapshot version is lower than the open state version (increment was finalised),
-- so the deposit value is already in the head — the CloseUsed redeemer is used.
healthyCloseCommitAppliedTx :: (Tx, UTxO)
healthyCloseCommitAppliedTx =
  (tx, lookupUTxO)
 where
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (mkHeadId Fixture.testPolicyId)
      healthyCommitAppliedOpenVersion
      healthyCommitAppliedConfirmedSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      incrementalAction

  incrementalAction =
    fromMaybe NoThing $
      setIncrementalActionMaybe
        (utxoToCommit $ getSnapshot healthyCommitAppliedConfirmedSnapshot)
        (utxoToDecommit $ getSnapshot healthyCommitAppliedConfirmedSnapshot)

  lookupUTxO :: UTxO
  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn enrichedHeadOutput
      <> registryUTxO scriptRegistry

  -- The head output already holds the deposited UTxO value because the increment
  -- was applied (CommitFinalized), so we enrich the test fixture accordingly.
  enrichedHeadOutput :: TxOut CtxUTxO
  enrichedHeadOutput =
    healthyOpenHeadTxOut datum
      & modifyTxOutValue (<> UTxO.totalValue healthyDepositUTxO)

  scriptRegistry = genScriptRegistry `generateWith` 42

  datum :: TxOutDatum CtxUTxO
  datum = mkTxOutDatumInline healthyCommitAppliedOpenDatum

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, enrichedHeadOutput)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      }

data CloseMutation
  = NotContinueContract
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
  | MutateCloseSignatures
  | MutateCloseType
  | MutateCloseHeadAdaOverhead
  deriving stock (Generic, Show, Enum, Bounded)

genCloseCommitUsedMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseCommitUsedMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode NotPayingToHead) NotContinueContract <$> do
        mutatedAddress <- genAddressInEra Fixture.testNetworkId
        pure $ ChangeOutput 0 (modifyTxOutAddress (const mutatedAddress) headTxOut)
    , -- Changing the redeemer type to CloseUnused while keeping an outdated snapshot
      -- causes the CloseUnused validator path to fail.
      SomeMutation (pure $ toErrorCode FailedCloseUnused) MutateSignatureButNotSnapshotNumber . ChangeHeadRedeemer <$> do
        signature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $ Head.Close Head.CloseUnused{signature, accumulatorHash = healthyCommitAppliedAccumulatorHash}
    , SomeMutation (pure $ toErrorCode FailedCloseUsed) MutateSnapshotNumberButNotSignature <$> do
        mutatedSnapshotNumber <- arbitrarySizedNatural `suchThat` (> healthyCommitAppliedSnapshotNumber)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthyCommitAppliedOpenVersion)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode FailedCloseUsed) SnapshotNotSignedByAllParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure . ChangeInputHeadDatum $ replaceParties mutatedParties healthyCommitAppliedOpenDatum
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
    , SomeMutation (pure $ toErrorCode AccumulatorCommitmentHashMismatch) MutateAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
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
                (replacePolicyIdWith Fixture.testPolicyId otherHeadId enrichedHeadOutput)
                ( Just $
                    toScriptData
                      ( Head.Close
                          Head.CloseUnused
                            { signature =
                                toPlutusSignatures $
                                  healthySignature healthyCommitAppliedSnapshot
                            , accumulatorHash = healthyCommitAppliedAccumulatorHash
                            }
                      )
                )
            ]
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode FailedCloseUsed) MutateCloseSignatures . ChangeHeadRedeemer <$> do
        signature <- toPlutusSignatures <$> (arbitrary `suchThat` (/= signatures healthyCommitAppliedConfirmedSnapshot))
        pure $
          Head.Close
            Head.CloseUsed
              { signature
              , accumulatorHash = healthyCommitAppliedAccumulatorHash
              }
    , SomeMutation (pure $ toErrorCode FailedCloseUnused) MutateCloseType . ChangeHeadRedeemer <$> do
        pure $
          Head.Close
            Head.CloseUnused
              { signature = toPlutusSignatures $ signatures healthyCommitAppliedConfirmedSnapshot
              , accumulatorHash = healthyCommitAppliedAccumulatorHash
              }
    , SomeMutation (pure $ toErrorCode ChangedHeadAdaOverhead) MutateCloseHeadAdaOverhead . ChangeOutput 0 <$> do
        wrongOverhead <- arbitrary `suchThat` (/= healthyCommitAppliedHeadAdaOverhead)
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
  datum = mkTxOutDatumInline healthyCommitAppliedOpenDatum

  enrichedHeadOutput :: TxOut CtxUTxO
  enrichedHeadOutput =
    healthyOpenHeadTxOut datum
      & modifyTxOutValue (<> UTxO.totalValue healthyDepositUTxO)

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
