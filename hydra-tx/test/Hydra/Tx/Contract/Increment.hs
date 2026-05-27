{-# LANGUAGE DuplicateRecordFields #-}

module Hydra.Tx.Contract.Increment where

import Hydra.Cardano.Api
import Hydra.Plutus.Gen ()
import Hydra.Prelude hiding (label)
import Test.Hydra.Prelude

import Cardano.Api.UTxO qualified as UTxO
import Data.Maybe (fromJust)
import Hydra.Cardano.Api.Gen (genTxIn)
import Hydra.Contract.Commit (Commit)
import Hydra.Contract.Deposit (DepositRedeemer (Claim))
import Hydra.Contract.DepositError (DepositError (..))
import Hydra.Contract.Error (toErrorCode)
import Hydra.Contract.HeadError (HeadError (..))
import Hydra.Contract.HeadState qualified as Head
import Hydra.Data.Party qualified as OnChain
import Hydra.Ledger.Cardano.Time (slotNoFromUTCTime)
import Hydra.Plutus.Orphans ()
import Hydra.Tx.Accumulator qualified as Accumulator
import Hydra.Tx.ContestationPeriod (ContestationPeriod, toChain)
import Hydra.Tx.Contract.Deposit (healthyDeadline)
import Hydra.Tx.Crypto (HydraKey, MultiSignature (..), aggregate, sign, toPlutusSignatures)
import Hydra.Tx.Deposit (mkDepositOutput)
import Hydra.Tx.Deposit qualified as Deposit
import Hydra.Tx.HeadId (mkHeadId)
import Hydra.Tx.HeadParameters (HeadParameters (..))
import Hydra.Tx.Increment (incrementTx)
import Hydra.Tx.Init (mkHeadOutput)
import Hydra.Tx.IsTx (IsTx (hashUTxO))
import Hydra.Tx.Party (Party, deriveParty, partyToChain)
import Hydra.Tx.ScriptRegistry (registryUTxO)
import Hydra.Tx.Snapshot (Snapshot (..), SnapshotNumber, SnapshotVersion)
import Hydra.Tx.Utils (adaOnly, verificationKeyToOnChainId)
import PlutusLedgerApi.V2 qualified as Plutus
import PlutusTx.Builtins (toBuiltin)
import Test.Hydra.Tx.Fixture (aliceSk, bobSk, carolSk, slotLength, systemStart, testNetworkId, testPolicyId, testSeedInput)
import Test.Hydra.Tx.Gen (genForParty, genScriptRegistry, genUTxOSized, genValue, genVerificationKey)
import Test.Hydra.Tx.Mutation (
  Mutation (..),
  SomeMutation (..),
  modifyInlineDatum,
  replaceParties,
  replaceSnapshotVersion,
 )
import Test.QuickCheck (arbitrarySizedNatural, elements, oneof, suchThat)
import Test.QuickCheck.Instances ()

healthyIncrementTx :: (Tx, UTxO)
healthyIncrementTx =
  (tx, lookupUTxO)
 where
  lookupUTxO =
    UTxO.singleton headInput headOutput
      <> depositUTxO
      <> registryUTxO scriptRegistry

  tx =
    incrementTx
      scriptRegistry
      somePartyCardanoVerificationKey
      (testSeedInput, mkHeadId testPolicyId)
      parameters
      (headInput, headOutput)
      healthySnapshot
      depositUTxO
      (slotNoFromUTCTime systemStart slotLength healthyDeadline)
      healthySignature

  parameters =
    HeadParameters
      { parties = healthyParties
      , contestationPeriod = healthyContestationPeriod
      }

  scriptRegistry = genScriptRegistry `generateWith` 42

  headInput = generateWith arbitrary 42

  headOutput =
    mkHeadOutput @CtxUTxO
      testNetworkId
      testPolicyId
      (verificationKeyToOnChainId <$> healthyParticipants)
      (mkTxOutDatumInline healthyDatum)
      & modifyTxOutValue (<> UTxO.totalValue healthyUTxO)

  depositUTxO :: UTxO
  depositUTxO =
    UTxO.singleton healthyDepositInput $
      mkDepositOutput testNetworkId (mkHeadId testPolicyId) healthyDeposited healthyDeadline

healthyDepositInput :: TxIn
healthyDepositInput = arbitrary `generateWith` 123

healthyDeposited :: UTxO
healthyDeposited = genUTxOSized 3 `generateWith` 42

somePartyCardanoVerificationKey :: VerificationKey PaymentKey
somePartyCardanoVerificationKey =
  elements healthyParticipants `generateWith` 42

healthySigningKeys :: [SigningKey HydraKey]
healthySigningKeys = [aliceSk, bobSk, carolSk]

healthyParticipants :: [VerificationKey PaymentKey]
healthyParticipants =
  genForParty genVerificationKey <$> healthyParties

healthyParties :: [Party]
healthyParties = deriveParty <$> healthySigningKeys

healthyOnChainParties :: [OnChain.Party]
healthyOnChainParties = partyToChain <$> healthyParties

healthySignature :: MultiSignature (Snapshot Tx)
healthySignature = aggregate [sign sk healthySnapshot | sk <- healthySigningKeys]

healthySnapshotNumber :: SnapshotNumber
healthySnapshotNumber = 1

healthySnapshotVersion :: SnapshotVersion
healthySnapshotVersion = 1

healthySnapshot :: Snapshot Tx
healthySnapshot =
  Snapshot
    { headId = mkHeadId testPolicyId
    , version = healthySnapshotVersion
    , number = succ healthySnapshotNumber
    , confirmed = []
    , utxo = healthyUTxO
    , utxoToCommit = Just healthyDeposited
    , utxoToDecommit = Nothing
    , accumulator = healthyAccumulator
    }

healthyAccumulatorHash :: ByteString
healthyAccumulatorHash = Accumulator.getAccumulatorHash healthyAccumulator

healthyAccumulator :: Accumulator.HydraAccumulator
healthyAccumulator = Accumulator.buildFromSnapshotUTxOs healthyUTxO (Just healthyDeposited) Nothing

healthyContestationPeriod :: ContestationPeriod
healthyContestationPeriod =
  arbitrary `generateWith` 42

healthyUTxO :: UTxO
healthyUTxO = UTxO.map adaOnly $ generateWith (genUTxOSized 3) 42

healthyDatum :: Head.State
healthyDatum =
  Head.Open
    Head.OpenDatum
      { utxoHash = toBuiltin $ hashUTxO @Tx healthyUTxO
      , parties = healthyOnChainParties
      , contestationPeriod = toChain healthyContestationPeriod
      , headSeed = toPlutusTxOutRef testSeedInput
      , headId = toPlutusCurrencySymbol testPolicyId
      , version = toInteger healthySnapshotVersion
      , accumulatorHash = toBuiltin healthyAccumulatorHash
      }

data IncrementMutation
  = -- | Move the deadline from the deposit datum back in time
    -- so that the increment upper bound is after the deadline
    DepositMutateDepositPeriod
  | -- | Change the head id stored in the deposit datum away from the
    -- head being incremented; checkIncrement must reject this.
    DepositMutateHeadId
  | -- | Change parties in increment output datum
    IncrementMutateParties
  | -- | New version is incremented correctly
    IncrementUseDifferentSnapshotVersion
  | -- | Produce invalid signatures
    ProduceInvalidSignatures
  | -- | Change the head value
    ChangeHeadValue
  | -- | Change the required signers
    AlterRequiredSigner
  | -- | Alter the Claim redeemer `TxOutRef`
    IncrementDifferentClaimRedeemer
  | -- | Add a second v_deposit input alongside an attacker-controlled
    -- output that redirects its value away from the head's continuation.
    IncrementAddExtraDepositInput
  deriving stock (Generic, Show, Enum, Bounded)

genIncrementMutation :: (Tx, UTxO) -> Gen SomeMutation
genIncrementMutation (tx, utxo) =
  oneof
    [ SomeMutation (pure $ toErrorCode DepositPeriodSurpassed) DepositMutateDepositPeriod <$> do
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((headCS', depositDatumDeadline, commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (headCS', Plutus.POSIXTime $ Plutus.getPOSIXTime depositDatumDeadline - 1000, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData Claim)
    , SomeMutation (pure $ toErrorCode DepositHeadInputNotFound) DepositMutateHeadId <$> do
        otherHeadId <- arbitrary `suchThat` (/= toPlutusCurrencySymbol testPolicyId)
        let datum =
              txOutDatum $
                flip modifyInlineDatum (fromCtxUTxOTxOut depositOut) $ \case
                  ((_headCS, depositDatumDeadline, commits) :: (Plutus.CurrencySymbol, Plutus.POSIXTime, [Commit])) ->
                    (otherHeadId, depositDatumDeadline, commits)
        let newOutput = toCtxUTxOTxOut $ TxOut addr val datum rscript
        pure $ ChangeInput depositIn newOutput (Just $ toScriptData Claim)
    , SomeMutation (pure $ toErrorCode ChangedParameters) IncrementMutateParties <$> do
        mutatedParties <- arbitrary `suchThat` (/= healthyOnChainParties)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceParties mutatedParties) headTxOut
    , SomeMutation (pure $ toErrorCode VersionNotIncremented) IncrementUseDifferentSnapshotVersion <$> do
        mutatedSnapshotVersion <- arbitrarySizedNatural `suchThat` (/= healthySnapshotVersion + 1)
        pure $ ChangeOutput 0 $ modifyInlineDatum (replaceSnapshotVersion $ toInteger mutatedSnapshotVersion) headTxOut
    , SomeMutation (pure $ toErrorCode SignatureVerificationFailed) ProduceInvalidSignatures . ChangeHeadRedeemer <$> do
        invalidSignature <- toPlutusSignatures <$> (arbitrary :: Gen (MultiSignature (Snapshot Tx)))
        pure $
          Head.Increment
            Head.IncrementRedeemer
              { signature =
                  invalidSignature
              , snapshotNumber = fromIntegral healthySnapshotNumber
              , increment = toPlutusTxOutRef healthyDepositInput
              }
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) ChangeHeadValue <$> do
        newValue <- genValue `suchThat` (/= txOutValue headTxOut)
        pure $ ChangeOutput 0 (headTxOut{txOutValue = newValue})
    , SomeMutation (pure $ toErrorCode SignerIsNotAParticipant) AlterRequiredSigner <$> do
        newSigner <- verificationKeyHash <$> genVerificationKey `suchThat` (/= somePartyCardanoVerificationKey)
        pure $ ChangeRequiredSigners [newSigner]
    , SomeMutation (pure $ toErrorCode DepositInputNotFound) IncrementDifferentClaimRedeemer . ChangeHeadRedeemer <$> do
        invalidDepositRef <- genTxIn
        pure $
          Head.Increment
            Head.IncrementRedeemer
              { signature = toPlutusSignatures healthySignature
              , snapshotNumber = fromIntegral $ succ healthySnapshotNumber
              , increment = toPlutusTxOutRef invalidDepositRef
              }
    , SomeMutation (pure $ toErrorCode HeadValueIsNotPreserved) IncrementAddExtraDepositInput <$> do
        extraIn <- genTxIn `suchThat` (/= depositIn)
        extraDeposited <- UTxO.map adaOnly <$> genUTxOSized 1
        attackerVk <- genVerificationKey
        let extraDepositOut :: TxOut CtxUTxO
            extraDepositOut =
              mkDepositOutput testNetworkId (mkHeadId testPolicyId) extraDeposited healthyDeadline
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
            ]
    ]
 where
  headTxOut = fromJust $ txOuts' tx !!? 0

  (depositIn, depositOut@(TxOut addr val _ rscript)) =
    fromJust $
      find
        (\(_, TxOut address _ _ _) -> address == Deposit.depositAddress testNetworkId)
        (UTxO.toList (resolveInputsUTxO utxo tx))
