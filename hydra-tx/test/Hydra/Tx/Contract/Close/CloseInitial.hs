{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hydra.Tx.Contract.Close.CloseInitial where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.Deposit (DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (ToErrorCode (..))
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Contract.HeadState qualified as HeadState
import Hydra.Contract.HeadTokens (headPolicyId)
import Hydra.Contract.UtilError (UtilError (MintingOrBurningIsForbidden))
import Hydra.Plutus (depositValidatorScript)
import Hydra.Plutus.Extras (posixFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx (
  ConfirmedSnapshot (..),
  Snapshot (utxoToCommit, utxoToDecommit),
  SnapshotVersion,
  mkHeadId,
  registryUTxO,
 )
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.Close (OpenThreadOutput (..), closeTx)
import Hydra.Tx.Contract.Close.Healthy (
  healthyCloseLowerBoundSlot,
  healthyCloseUpperBoundPointInTime,
  healthyContestationDeadline,
  healthyContestationPeriod,
  healthyContestationPeriodSeconds,
  healthyOnChainParties,
  healthyOpenHeadTxIn,
  healthyOpenHeadTxOut,
  somePartyCardanoVerificationKey,
 )
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.DepositPeriod qualified as DP
import Hydra.Tx.Snapshot (getSnapshot)
import Hydra.Tx.Utils (IncrementalAction (..), adaOnly, setIncrementalActionMaybe)
import PlutusLedgerApi.V1.Time (DiffMilliSeconds (..), fromMilliSeconds)
import PlutusLedgerApi.V3 (POSIXTime, PubKeyHash (PubKeyHash), toBuiltin)
import Test.Hydra.Tx.Fixture qualified as Fixture
import Test.Hydra.Tx.Gen (genHash, genMintedOrBurnedValue, genScriptRegistry, genUTxOSized, genValue, genVerificationKey)
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
  replaceSnapshotNumber,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, choose, elements, listOf1, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()

data CloseInitialMutation
  = MutateCloseContestationDeadline'
  | -- | Inject an unrelated v_deposit input into a healthy Close.
    CloseAbsorbForeignDeposit
  | -- | CloseInitial-specific: snapshotNumber must be 0 in closed datum.
    MutateSnapshotNumber
  | -- | CloseInitial-specific: version must be 0 in closed datum.
    MutateSnapshotVersion
  | -- | CloseInitial-specific: accumulatorCommitment must be the G1 generator.
    MutateAccumulatorCommitment
  | MutatePartiesInOutput
  | MutateHeadIdInOutput
  | MutateContestationPeriod
  | MutateInfiniteLowerBound
  | MutateInfiniteUpperBound
  | MutateValidityInterval
  | MutateTokenMintingOrBurning
  | MutateContesters
  | MutateValueInOutput
  | MutateRequiredSigner
  | MutateCloseHeadAdaOverhead
  deriving stock (Generic, Show, Enum, Bounded)

healthyCloseSnapshotVersion :: SnapshotVersion
healthyCloseSnapshotVersion = 0

-- | Healthy close transaction for the specific case were we close a head
--   with the initial UtxO, that is, no snapshot have been agreed upon and
--   signed by the head members yet.
healthyCloseInitialTx :: (Tx, UTxO)
healthyCloseInitialTx =
  (tx, lookupUTxO)
 where
  tx :: Tx
  tx =
    closeTx
      scriptRegistry
      somePartyCardanoVerificationKey
      headId
      healthyCloseSnapshotVersion
      closingSnapshot
      healthyCloseLowerBoundSlot
      healthyCloseUpperBoundPointInTime
      openThreadOutput
      incrementalAction

  incrementalAction =
    fromMaybe NoThing $
      setIncrementalActionMaybe (utxoToCommit $ getSnapshot closingSnapshot) (utxoToDecommit $ getSnapshot closingSnapshot)

  initialDatum :: TxOutDatum CtxUTxO
  initialDatum = mkTxOutDatumInline healthyInitialOpenDatum

  lookupUTxO :: UTxO
  lookupUTxO =
    UTxO.singleton healthyOpenHeadTxIn (healthyOpenHeadTxOut initialDatum)
      <> registryUTxO scriptRegistry

  scriptRegistry = genScriptRegistry `generateWith` 42

  openThreadOutput :: OpenThreadOutput
  openThreadOutput =
    OpenThreadOutput
      { openThreadUTxO = (healthyOpenHeadTxIn, healthyOpenHeadTxOut initialDatum)
      , openParties = healthyOnChainParties
      , openContestationPeriod = healthyContestationPeriod
      , openDepositPeriod = DP.toChain Fixture.dperiod
      }

  headId = mkHeadId Fixture.testPolicyId

  closingSnapshot :: ConfirmedSnapshot Tx
  closingSnapshot = InitialSnapshot{headId}

healthyInitialOpenDatum :: HeadState.State
healthyInitialOpenDatum =
  Head.Open
    Head.OpenDatum
      { parties = healthyOnChainParties
      , contestationPeriod = healthyContestationPeriod
      , depositPeriod = DP.toChain Fixture.dperiod
      , headSeed = toPlutusTxOutRef Fixture.testSeedInput
      , headId = toPlutusCurrencySymbol Fixture.testPolicyId
      , version = 0
      , accumulatorHash = toBuiltin $ Accumulator.getAccumulatorHash $ Accumulator.buildFromSnapshotUTxOs @Tx mempty Nothing Nothing
      , headAdaOverhead = 0
      }

--- | Mutations for the specific case of closing with the initial state.
--- We should probably validate all the mutation to this initial state but at
--- least we keep this regression test as we stumbled upon problems with the following case.
--- The nice thing to do would probably to generate either "normal" healthyCloseTx or
--- or healthyCloseInitialTx and apply all the mutations to it but we didn't manage to do that
--- right away.
genCloseInitialMutation :: (Tx, UTxO) -> Gen SomeMutation
genCloseInitialMutation (tx, _utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode IncorrectClosedContestationDeadline) MutateCloseContestationDeadline' <$> do
        mutatedDeadline <- genMutatedDeadline
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceContestationDeadline mutatedDeadline) headTxOut
    , SomeMutation (pure $ toErrorCode HeadRedeemerNotIncrement) CloseAbsorbForeignDeposit <$> do
        extraIn <- genTxIn
        extraDeposited <- UTxO.map adaOnly <$> genUTxOSized 1
        attackerVk <- genVerificationKey
        let
          -- Past the tx upper bound, so the deadline check passes and
          -- the later guard fires instead.
          extraDeadline =
            addUTCTime (60 * 60 * 24) (snd healthyCloseUpperBoundPointInTime)
          extraDepositOut :: TxOut CtxUTxO
          extraDepositOut =
            mkDepositOutput
              Fixture.testNetworkId
              (mkHeadId Fixture.testPolicyId)
              extraDeposited
              extraDeadline
          attackerOut :: TxOut CtxTx
          attackerOut =
            TxOut
              (mkVkAddress Fixture.testNetworkId attackerVk)
              (txOutValue extraDepositOut)
              TxOutDatumNone
              ReferenceScriptNone
        pure $
          Changes
            [ AddInput extraIn extraDepositOut (Just $ toScriptData Claim)
            , AddScript depositValidatorScript
            , AppendOutput attackerOut
            ]
    , SomeMutation (pure $ toErrorCode FailedCloseInitial) MutateSnapshotNumber <$> do
        mutatedSnapshotNumber <- (arbitrarySizedNatural :: Gen Int) `suchThat` (> 0)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotNumber $ toInteger mutatedSnapshotNumber) headTxOut
    , SomeMutation (pure $ toErrorCode MustNotChangeVersion) MutateSnapshotVersion <$> do
        mutatedSnapshotVersion <- (arbitrarySizedNatural :: Gen Int) `suchThat` (> 0)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode FailedCloseInitial) MutateAccumulatorCommitment . ChangeOutput 0 <$> do
        let wrongCommitment = Accumulator.getAccumulatorCommitment (Accumulator.build ["wrong"])
        pure $ headTxOut & modifyInlineDatum (replaceAccumulatorCommitment wrongCommitment)
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutatePartiesInOutput <$> do
        n <- choose (1, length healthyOnChainParties - 1)
        fn <- elements [drop n, take n]
        let mutatedParties = fn healthyOnChainParties
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode ChangedParameters) MutateHeadIdInOutput <$> do
        otherHeadId <- toPlutusCurrencySymbol . headPolicyId <$> arbitrary `suchThat` (/= Fixture.testSeedInput)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceHeadId otherHeadId) headTxOut
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
    , SomeMutation (pure $ toErrorCode MintingOrBurningIsForbidden) MutateTokenMintingOrBurning
        <$> (changeMintedTokens tx =<< genMintedOrBurnedValue)
    , SomeMutation (pure $ toErrorCode ContestersNonEmpty) MutateContesters . ChangeOutput 0 <$> do
        mutatedContesters <- resize 3 . listOf1 $ PubKeyHash . toBuiltin <$> genHash
        pure $ headTxOut & modifyInlineDatum (replaceContesters mutatedContesters)
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) MutateValueInOutput <$> do
        newValue <- genValue
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) MutateRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode ChangedHeadAdaOverhead) MutateCloseHeadAdaOverhead . ChangeOutput 0 <$> do
        wrongOverhead <- arbitrary `suchThat` (/= (0 :: Integer))
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

-- | Generate not acceptable, but interesting deadlines.
genMutatedDeadline :: Gen POSIXTime
genMutatedDeadline = do
  oneof
    [ valuesAroundZero
    , valuesAroundDeadline
    ]
 where
  valuesAroundZero = arbitrary `suchThat` (/= deadline)

  valuesAroundDeadline = arbitrary `suchThat` (/= 0) <&> (+ deadline)

  deadline = posixFromUTCTime healthyContestationDeadline
